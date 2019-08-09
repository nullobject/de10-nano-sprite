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
  constant TILE_ROM_ADDR_WIDTH   : natural := 15;

  type state_t is (INIT, LOAD, LATCH, CHECK, PRE_BLIT, BLIT, JUMP);

  type sprite_pos_t is record
    x : unsigned(4 downto 0);
    y : unsigned(4 downto 0);
  end record sprite_pos_t;

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
  signal video_pos   : pos_t;
  signal video_sync  : sync_t;
  signal video_blank : blank_t;
  signal video_on    : std_logic;
  signal vblank_falling : std_logic;

  -- frame buffer
  signal frame_buffer_addr_rd : std_logic_vector(15 downto 0);
  signal frame_buffer_addr_wr : std_logic_vector(15 downto 0);
  signal frame_buffer_din     : std_logic_vector(7 downto 0);
  signal frame_buffer_dout    : std_logic_vector(7 downto 0);
  signal frame_buffer_flip    : std_logic;
  signal frame_buffer_rden    : std_logic;
  signal frame_buffer_wren    : std_logic;

  -- position signals
  signal src_pos  : sprite_pos_t;
  signal dest_pos : pos_t;

  -- control signals
  signal col_max : std_logic;
  signal row_max : std_logic;

  -- pixel data
  signal pixel : nibble_t;

  -- sprite signals
  signal sprite         : sprite_t;
  signal sprite_size    : unsigned(5 downto 0);
  signal sprite_visible : std_logic;

  -- graphics data
  signal gfx_data : byte_t;

  signal sprite_index : unsigned(1 downto 0);

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
    -- sprite.enable   := data(SPRITE_ENABLE_BIT);
    -- sprite.flip_x   := data(SPRITE_FLIP_X_BIT);
    -- sprite.flip_y   := data(SPRITE_FLIP_Y_BIT);
    sprite.pos.x    := data(SPRITE_HI_POS_X_BIT) & unsigned(data(SPRITE_LO_POS_X_MSB downto SPRITE_LO_POS_X_LSB));
    sprite.pos.y    := data(SPRITE_HI_POS_Y_BIT) & unsigned(data(SPRITE_LO_POS_Y_MSB downto SPRITE_LO_POS_Y_LSB));
    -- sprite.priority := unsigned(data(SPRITE_PRIORITY_MSB downto SPRITE_PRIORITY_LSB));
    sprite.size     := unsigned(data(SPRITE_SIZE_MSB downto SPRITE_SIZE_LSB));

    return sprite;
  end init_sprite;

  -- calculate sprite size (8x8, 16x16, 32x32)
  function sprite_size_in_pixels(size : unsigned(1 downto 0)) return natural is
  begin
    case size is
      when "00" => return 0;
      when "01" => return 8;
      when "10" => return 16;
      when "11" => return 32;
    end case;
  end sprite_size_in_pixels;
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

    -- for debugging
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

  -- increment sprite index counter
  sprite_index_counter : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if state = JUMP then
        sprite_index <= sprite_index + 1;
      end if;
    end if;
  end process;

  -- increment sprite pixel counters
  pixel_counters : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if state = CHECK then
        src_pos.x <= "11110";
        src_pos.y <= (others => '0');
      elsif state = PRE_BLIT or state = BLIT then
        if state = BLIT and src_pos.x = sprite_size-1 then
          src_pos.x <= (others => '0');

          if src_pos.y = sprite_size-1 then
            src_pos.y <= (others => '0');
          else
            src_pos.y <= src_pos.y + 1;
          end if;
        else
          src_pos.x <= src_pos.x + 1;
        end if;
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

  -- Load graphics data from the tile ROM.
  --
  -- While the current two pixels are being rendered, we need to fetch data for
  -- the next two pixels, so they are loaded in time to render them on the
  -- screen.
  load_gfx_data : process (sprite.code, src_pos.x, src_pos.y)
    variable x : unsigned(3 downto 0);
    variable y : unsigned(4 downto 0);
  begin
    x := src_pos.x(4 downto 1)+1;
    y := src_pos.y(4 downto 0);

    tile_rom_addr <= std_logic_vector(
      sprite.code(9 downto 4) &
      (sprite.code(3 downto 0) or (y(4) & x(3) & y(3) & x(2))) &
      y(2 downto 0) &
      x(1 downto 0)
    );
  end process;

  -- latch graphics data from the tile ROM
  latch_gfx_data : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if (state = PRE_BLIT or state = BLIT) and src_pos.x(0) = '1' then
        gfx_data <= tile_rom_dout;
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
  fsm : process (state, video_blank.vblank, col_max, row_max)
  begin
    next_state <= state;

    case state is
      when INIT =>
        if video_blank.vblank = '0' then
          -- if we're not in a vertical blank, then load the sprite
          next_state <= LOAD;
        end if;

      when LOAD =>
        -- latch the sprite
        next_state <= LATCH;

      when LATCH =>
        -- check the sprite
        next_state <= CHECK;

      when CHECK =>
        if sprite_visible = '1' then
          -- if the sprite is visible on the screen, then blit it
          next_state <= PRE_BLIT;
        else
          -- otherwise, skip to the next sprite
          next_state <= JUMP;
        end if;

      when PRE_BLIT =>
        -- FIXME: this should be the counter maximum, not the sprite width
        if col_max = '1' then
          -- if the pre-blit is done, then start the blit
          next_state <= BLIT;
        end if;

      when BLIT =>
        if col_max = '1' and row_max = '1' then
          -- if the blit is done, then skip to the next sprite
          next_state <= JUMP;
        end if;

      when JUMP =>
        if video_blank.vblank = '0' then
          -- if we're not in a vertical blank, then load the next sprite
          next_state <= LOAD;
        else
          -- otherwise, wait for the next frame to start
          next_state <= INIT;
        end if;

    end case;
  end process;

  -- set sprite RAM address
  sprite_ram_addr <= std_logic_vector(resize(sprite_index, sprite_ram_addr'length));

  -- set sprite size
  sprite_size <= to_unsigned(sprite_size_in_pixels(sprite.size), sprite_size'length);

  -- set sprite visible
  -- TODO: check sprite is enabled
  sprite_visible <= '1' when sprite_size /= 0 else '0';

  -- TODO: handle X/Y axis flipping
  dest_pos.x <= resize(sprite.pos.x+src_pos.x, dest_pos.x'length);
  dest_pos.y <= resize(sprite.pos.y+src_pos.y, dest_pos.y'length);

  -- the blit is done when all the pixels have been copied
  col_max <= '1' when src_pos.x = sprite_size-1 else '0';
  row_max <= '1' when src_pos.y = sprite_size-1 else '0';

  -- set frame buffer write address
  frame_buffer_addr_wr <= std_logic_vector(dest_pos.y(7 downto 0) & dest_pos.x(7 downto 0));

  -- FIXME: why the fuck is this latching on even pixels?
  -- TODO: handle priority and colour
  frame_buffer_din <= ("0000" & gfx_data(7 downto 4)) when src_pos.x(0) = '0' else ("0000" & gfx_data(3 downto 0));

  frame_buffer_wren <= '1' when state = BLIT and dest_pos.x(8) = '0' and dest_pos.y(8) = '0' else '0';

  frame_buffer_addr_rd <= std_logic_vector(video_pos.y(7 downto 0) & video_pos.x(7 downto 0));
  frame_buffer_rden <= not (video_blank.hblank or video_blank.vblank);

  pixel <= frame_buffer_dout(3 downto 0);

  video_on <= not (video_blank.hblank or video_blank.vblank);
  vga_csync <= not (video_sync.hsync xor video_sync.vsync);

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
end arch;
