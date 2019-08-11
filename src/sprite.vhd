library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.types.all;

-- The sprite layer of the graphics pipeline handles the moving graphical
-- elements you see on the screen.
--
-- They can be placed anywhere on the screen with per-pixel precision, can be
-- flipped about thier horizontal and/or vertical axes, and can even overlap
-- each other.
--
-- There are three different sprite sizes – 8x8, 16x16, and 32x32 – all of
-- which are composed from one or more 8x8 tiles.
--
-- The data which describes the characteristics of each sprite – such as
-- position, size, etc. – is stored in the sprite RAM. The pixel data for the
-- 8x8 tiles which make up each sprite is stored in the sprite tile ROM.
entity sprite is
  port (
    clk         : in std_logic;
    video_pos   : in pos_t;
    video_sync  : in sync_t;
    video_blank : in blank_t;
    data        : out std_logic_vector(FRAME_BUFFER_DATA_WIDTH-1 downto 0)
  );
end sprite;

architecture arch of sprite is
  type state_t is (INIT, LOAD, LATCH, BLIT, JUMP, DONE);

  signal state, next_state : state_t;

  -- sprite RAM signals
  signal sprite_ram_addr : std_logic_vector(SPRITE_RAM_ADDR_WIDTH-1 downto 0);
  signal sprite_ram_dout : std_logic_vector(SPRITE_RAM_DATA_WIDTH-1 downto 0);

  -- tile ROM signals
  signal tile_rom_addr : std_logic_vector(TILE_ROM_ADDR_WIDTH-1 downto 0);
  signal tile_rom_dout : byte_t;

  -- video signals
  signal vblank_falling : std_logic;

  -- frame buffer
  signal frame_buffer_addr_rd : std_logic_vector(FRAME_BUFFER_ADDR_WIDTH-1 downto 0);
  signal frame_buffer_addr_wr : std_logic_vector(FRAME_BUFFER_ADDR_WIDTH-1 downto 0);
  signal frame_buffer_din     : std_logic_vector(FRAME_BUFFER_DATA_WIDTH-1 downto 0);
  signal frame_buffer_flip    : std_logic;
  signal frame_buffer_rden    : std_logic;
  signal frame_buffer_wren    : std_logic;

  -- sprite signals
  signal sprite       : sprite_t;
  signal sprite_index : unsigned(1 downto 0) := (others => '1');

  -- control signals
  signal frame_done : std_logic;
  signal blit_start : std_logic;
  signal blit_done  : std_logic;
begin
  sprite_ram : entity work.single_port_rom
  generic map (
    ADDR_WIDTH => SPRITE_RAM_ADDR_WIDTH,
    DATA_WIDTH => SPRITE_RAM_DATA_WIDTH,
    INIT_FILE  => "rom/sprites.mif",

    -- XXX: for debugging
    ENABLE_RUNTIME_MOD => "YES"
  )
  port map (
    clk  => clk,
    addr => sprite_ram_addr,
    dout => sprite_ram_dout
  );

  tile_rom : entity work.single_port_rom
  generic map (
    ADDR_WIDTH => TILE_ROM_ADDR_WIDTH,
    INIT_FILE  => "rom/vid_6g.mif"
  )
  port map (
    clk  => clk,
    addr => tile_rom_addr,
    dout => tile_rom_dout
  );

  sprite_frame_buffer : entity work.frame_buffer
  generic map (
    ADDR_WIDTH => FRAME_BUFFER_ADDR_WIDTH,
    DATA_WIDTH => FRAME_BUFFER_DATA_WIDTH
  )
  port map (
    clk  => clk,
    flip => frame_buffer_flip,

    -- write-only port
    addr_wr => frame_buffer_addr_wr,
    din     => frame_buffer_din,
    wren    => frame_buffer_wren,

    -- read-only port
    addr_rd => frame_buffer_addr_rd,
    dout    => data,
    rden    => frame_buffer_rden
  );

  vblank_edge_detector : entity work.edge_detector
  generic map (FALLING => true)
  port map (
    clk  => clk,
    data => video_blank.vblank,
    edge => vblank_falling
  );

  sprite_biltter : entity work.sprite_blitter
  port map (
    clk       => clk,
    sprite    => sprite,
    src_addr  => tile_rom_addr,
    din       => tile_rom_dout,
    dest_addr => frame_buffer_addr_wr,
    dout      => frame_buffer_din,
    busy      => frame_buffer_wren,
    start     => blit_start,
    done      => blit_done
  );

  -- XXX: Can this be handled by the FSM? That way we wouldn't need the VBLANK
  -- edge detector.
  page_flipper : process (clk)
  begin
    if rising_edge(clk) then
      if vblank_falling = '1' then
        frame_buffer_flip <= not frame_buffer_flip;
      end if;
    end if;
  end process;

  -- latch the next state
  latch_state : process (clk)
  begin
    if rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

  -- state machine
  fsm : process (state, video_blank.vblank, blit_done, frame_done)
  begin
    next_state <= state;

    case state is
      -- this is the default state, we just wait for the beginning of the frame
      when INIT =>
        if video_blank.vblank = '0' then
          next_state <= LOAD;
        end if;

      -- load the next sprite
      when LOAD =>
        next_state <= LATCH;

      -- latch the sprite
      when LATCH =>
        next_state <= BLIT;

      -- blit the sprite
      when BLIT =>
        if blit_done = '1' then
          next_state <= JUMP;
        end if;

      -- check whether the frame is done
      when JUMP =>
        if frame_done = '1' then
          next_state <= DONE;
        else
          next_state <= LOAD;
        end if;

      -- wait for the end of the frame
      when DONE =>
        if video_blank.vblank = '1' then
          next_state <= INIT;
        end if;

    end case;
  end process;

  -- Update the sprite index counter.
  --
  -- Sprites are sorted from highest to lowest priority, so we need to iterate
  -- backwards to ensure that the sprites with the highest priority are drawn
  -- last.
  sprite_index_counter : process (clk)
  begin
    if rising_edge(clk) then
      if state = JUMP then
        sprite_index <= sprite_index - 1;
      end if;
    end if;
  end process;

  -- latch sprite from the sprite RAM
  latch_sprite : process (clk)
  begin
    if rising_edge(clk) then
      if state = LATCH then
        sprite <= init_sprite(sprite_ram_dout);
      end if;
    end if;
  end process;

  -- start the sprite blitter
  blit_sprite : process (clk)
  begin
    if rising_edge(clk) then
      if state = LOAD then
        blit_start <= '1';
      else
        blit_start <= '0';
      end if;
    end if;
  end process;

  -- set sprite RAM address
  sprite_ram_addr <= std_logic_vector(resize(sprite_index, sprite_ram_addr'length));

  -- the frame is done when all the sprites have been blitted
  frame_done <= '1' when sprite_index = 0 else '0';

  -- set frame buffer read address
  frame_buffer_addr_rd <= std_logic_vector(video_pos.y(7 downto 0) & video_pos.x(7 downto 0));

  -- read from the frame buffer when video output is enabled
  frame_buffer_rden <= not (video_blank.hblank or video_blank.vblank);
end arch;
