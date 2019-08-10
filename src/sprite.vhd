library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library pll;

use work.types.all;

entity sprite is
  port (
    clk : in std_logic;
    vga_r, vga_g, vga_b : out std_logic_vector(5 downto 0);
    vga_csync : out std_logic
  );
end sprite;

architecture arch of sprite is
  constant SPRITE_RAM_ADDR_WIDTH : natural := 4;
  constant SPRITE_RAM_DATA_WIDTH : natural := 64;

  type state_t is (INIT, LOAD, LATCH, BLIT, JUMP);

  signal state, next_state : state_t;

  -- clock signals
  signal clk_12 : std_logic;
  signal cen_6  : std_logic;

  -- sprite RAM signals
  signal sprite_ram_addr : std_logic_vector(SPRITE_RAM_ADDR_WIDTH-1 downto 0);
  signal sprite_ram_dout : std_logic_vector(SPRITE_RAM_DATA_WIDTH-1 downto 0);

  -- tile ROM signals
  signal tile_rom_addr : std_logic_vector(TILE_ROM_ADDR_WIDTH-1 downto 0);
  signal tile_rom_dout : byte_t;

  -- video signals
  signal video_pos      : pos_t;
  signal video_sync     : sync_t;
  signal video_blank    : blank_t;
  signal video_on       : std_logic;
  signal vblank_falling : std_logic;

  -- frame buffer
  signal frame_buffer_addr_rd : std_logic_vector(15 downto 0);
  signal frame_buffer_addr_wr : std_logic_vector(15 downto 0);
  signal frame_buffer_din     : std_logic_vector(7 downto 0);
  signal frame_buffer_dout    : std_logic_vector(7 downto 0);
  signal frame_buffer_flip    : std_logic;
  signal frame_buffer_rden    : std_logic;
  signal frame_buffer_wren    : std_logic;

  -- pixel data
  signal pixel : nibble_t;

  -- sprite signals
  signal sprite       : sprite_t;
  signal sprite_index : unsigned(1 downto 0);

  -- graphics data
  signal gfx_data : byte_t;

  -- sprite blitter
  signal sprite_blitter_start : std_logic;
  signal sprite_blitter_done  : std_logic;

  -- byte 0
  constant SPRITE_HI_CODE_MSB : natural := 7;
  constant SPRITE_HI_CODE_LSB : natural := 4;
  constant SPRITE_ENABLE_BIT  : natural := 2;
  constant SPRITE_FLIP_Y_BIT  : natural := 1;
  constant SPRITE_FLIP_X_BIT  : natural := 0;

  -- byte 1
  constant SPRITE_LO_CODE_MSB : natural := 15;
  constant SPRITE_LO_CODE_LSB : natural := 8;

  -- byte 2
  constant SPRITE_SIZE_MSB : natural := 17;
  constant SPRITE_SIZE_LSB : natural := 16;

  -- byte 3
  constant SPRITE_PRIORITY_MSB : natural := 31;
  constant SPRITE_PRIORITY_LSB : natural := 30;
  constant SPRITE_HI_POS_Y_BIT : natural := 29;
  constant SPRITE_HI_POS_X_BIT : natural := 28;
  constant SPRITE_COLOR_MSB    : natural := 27;
  constant SPRITE_COLOR_LSB    : natural := 24;

  -- byte 4
  constant SPRITE_LO_POS_Y_MSB : natural := 39;
  constant SPRITE_LO_POS_Y_LSB : natural := 32;

  -- byte 5
  constant SPRITE_LO_POS_X_MSB : natural := 47;
  constant SPRITE_LO_POS_X_LSB : natural := 40;

  -- initialise sprite from a raw 64-bit value
  function init_sprite(data : std_logic_vector(SPRITE_RAM_DATA_WIDTH-1 downto 0)) return sprite_t is
    variable sprite : sprite_t;
  begin
    sprite.code     := unsigned(data(SPRITE_HI_CODE_MSB downto SPRITE_HI_CODE_LSB)) & unsigned(data(SPRITE_LO_CODE_MSB downto SPRITE_LO_CODE_LSB));
    -- sprite.color    := unsigned(data(SPRITE_COLOR_MSB downto SPRITE_COLOR_LSB));
    sprite.enable   := data(SPRITE_ENABLE_BIT);
    -- sprite.flip_x   := data(SPRITE_FLIP_X_BIT);
    -- sprite.flip_y   := data(SPRITE_FLIP_Y_BIT);
    sprite.pos.x    := data(SPRITE_HI_POS_X_BIT) & unsigned(data(SPRITE_LO_POS_X_MSB downto SPRITE_LO_POS_X_LSB));
    sprite.pos.y    := data(SPRITE_HI_POS_Y_BIT) & unsigned(data(SPRITE_LO_POS_Y_MSB downto SPRITE_LO_POS_Y_LSB));
    -- sprite.priority := unsigned(data(SPRITE_PRIORITY_MSB downto SPRITE_PRIORITY_LSB));
    sprite.size     := unsigned(data(SPRITE_SIZE_MSB downto SPRITE_SIZE_LSB));

    return sprite;
  end init_sprite;

  -- XXX: for debugging
  attribute preserve : boolean;
  attribute preserve of tile_rom_addr : signal is true;
  attribute keep : boolean;
  attribute keep of sprite_blitter_start : signal is true;
  attribute keep of sprite_blitter_done : signal is true;
begin
  my_pll : entity pll.pll
  port map (
    refclk   => clk,
    rst      => '0',
    outclk_0 => clk_12,
    locked   => open
  );

  -- generate the 6MHz clock enable signal
  clock_divider_6 : entity work.clock_divider
  generic map (DIVISOR => 2)
  port map (clk => clk_12, cen => cen_6);

  sync_gen : entity work.sync_gen
  port map (
    clk   => clk_12,
    cen   => cen_6,
    pos   => video_pos,
    sync  => video_sync,
    blank => video_blank
  );

  sprite_ram : entity work.single_port_rom
  generic map (
    ADDR_WIDTH => SPRITE_RAM_ADDR_WIDTH,
    DATA_WIDTH => SPRITE_RAM_DATA_WIDTH,
    INIT_FILE  => "rom/sprites.mif",

    -- XXX: for debugging
    ENABLE_RUNTIME_MOD => "YES"
  )
  port map (
    clk  => clk_12,
    addr => sprite_ram_addr,
    dout => sprite_ram_dout
  );

  tile_rom : entity work.single_port_rom
  generic map (
    ADDR_WIDTH => TILE_ROM_ADDR_WIDTH,
    INIT_FILE  => "rom/vid_6g.mif"
  )
  port map (
    clk  => clk_12,
    addr => tile_rom_addr,
    dout => tile_rom_dout
  );

  sprite_frame_buffer : entity work.frame_buffer
  generic map (ADDR_WIDTH => 16, DATA_WIDTH => 8)
  port map (
    clk  => clk_12,
    flip => frame_buffer_flip,

    -- write-only port
    addr_wr => frame_buffer_addr_wr,
    din     => frame_buffer_din,
    wren    => frame_buffer_wren,

    -- read-only port
    addr_rd => frame_buffer_addr_rd,
    dout    => frame_buffer_dout,
    rden    => frame_buffer_rden
  );

  vblank_edge_detector : entity work.edge_detector
  generic map (FALLING => true)
  port map (
    clk  => clk_12,
    data => video_blank.vblank,
    edge => vblank_falling
  );

  sprite_biltter : entity work.sprite_blitter
  port map (
    clk       => clk_12,
    sprite    => sprite,
    src_addr  => tile_rom_addr,
    din       => tile_rom_dout,
    dest_addr => frame_buffer_addr_wr,
    dout      => frame_buffer_din,
    busy      => frame_buffer_wren,
    start     => sprite_blitter_start,
    done      => sprite_blitter_done
  );

  -- XXX: Can this be handled by the FSM? That way we wouldn't need the VBLANK
  -- edge detector.
  page_flipper : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if vblank_falling = '1' then
        frame_buffer_flip <= not frame_buffer_flip;
      end if;
    end if;
  end process;

  -- latch the next state
  latch_state : process (clk_12)
  begin
    if rising_edge(clk_12) then
      state <= next_state;
    end if;
  end process;

  -- state machine
  fsm : process (state, video_blank.vblank, sprite_blitter_done)
  begin
    next_state <= state;

    case state is
      when INIT =>
        if video_blank.vblank = '0' then
          next_state <= LOAD;
        end if;

      when LOAD =>
        next_state <= LATCH;

      when LATCH =>
        next_state <= BLIT;

      when BLIT =>
        if sprite_blitter_done = '1' then
          next_state <= JUMP;
        end if;

      when JUMP =>
        if video_blank.vblank = '0' then
          next_state <= LOAD;
        else
          next_state <= INIT;
        end if;

    end case;
  end process;

  -- increment sprite index counter
  sprite_index_counter : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if state = JUMP then
        sprite_index <= sprite_index + 1;
      end if;
    end if;
  end process;

  -- latch sprite from the sprite RAM
  latch_sprite : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if state = LATCH then
        sprite <= init_sprite(sprite_ram_dout);
      end if;
    end if;
  end process;

  -- enable the blitter
  blit_enable : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if state = LOAD then
        sprite_blitter_start <= '1';
      else
        sprite_blitter_start <= '0';
      end if;
    end if;
  end process;

  video_output : process (clk_12)
  begin
    if rising_edge(clk_12) and cen_6 = '1' then
      if video_on = '1' then
        vga_r <= pixel & pixel(3 downto 2);
        vga_g <= pixel & pixel(3 downto 2);
        vga_b <= pixel & pixel(3 downto 2);
      else
        vga_r <= (others => '0');
        vga_g <= (others => '0');
        vga_b <= (others => '0');
      end if;
    end if;
  end process;

  -- set sprite RAM address
  sprite_ram_addr <= std_logic_vector(resize(sprite_index, sprite_ram_addr'length));

  -- set frame buffer read address
  frame_buffer_addr_rd <= std_logic_vector(video_pos.y(7 downto 0) & video_pos.x(7 downto 0));

  -- read from the frame buffer when video output is enabled
  frame_buffer_rden <= not (video_blank.hblank or video_blank.vblank);

  -- set the pixel
  pixel <= frame_buffer_dout(3 downto 0);

  video_on <= not (video_blank.hblank or video_blank.vblank);
  vga_csync <= not (video_sync.hsync xor video_sync.vsync);
end arch;
