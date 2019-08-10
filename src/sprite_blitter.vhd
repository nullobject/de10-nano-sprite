library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.types.all;

entity sprite_blitter is
  port (
    -- clock
    clk : in std_logic;

    -- sprite descriptor
    sprite : in sprite_t;

    -- data in
    src_addr : out std_logic_vector(TILE_ROM_ADDR_WIDTH-1 downto 0);
    din      : in byte_t;

    -- data out
    dest_addr : out std_logic_vector(FRAME_BUFFER_ADDR_WIDTH-1 downto 0);
    dout      : out std_logic_vector(FRAME_BUFFER_DATA_WIDTH-1 downto 0);

    -- control signals
    start : in std_logic;
    busy  : out std_logic;
    done  : out std_logic
  );
end sprite_blitter;

architecture arch of sprite_blitter is
  type state_t is (INIT, CHECK, PRELOAD, BLIT);

  signal state, next_state : state_t;

  -- position signals
  signal src_pos  : sprite_pos_t;
  signal load_pos : sprite_pos_t;
  signal dest_pos : pos_t;

  -- control signals
  signal preload_done : std_logic;
  signal blit_done    : std_logic;

  signal sprite_size    : unsigned(5 downto 0);
  signal sprite_visible : std_logic;

  signal gfx_data : byte_t;

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
  -- latch the next state
  latch_state : process (clk)
  begin
    if rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

  -- state machine
  fsm : process (state, start, preload_done, blit_done)
  begin
    next_state <= state;

    case state is
      -- This is the default state, we just wait for the start signal.
      when INIT =>
        if start = '1' then
          next_state <= CHECK;
        end if;

      -- Check whether the sprite is visible, before we bother to render it.
      when CHECK =>
        if sprite_visible = '1' then
          next_state <= PRELOAD;
        else
          next_state <= INIT;
        end if;

      -- Preload the data for the first pixel.
      when PRELOAD =>
        if preload_done = '1' then
          next_state <= BLIT;
        end if;

      -- Copy pixels from the source to the destination.
      when BLIT =>
        if blit_done = '1' then
          next_state <= INIT;
        end if;

    end case;
  end process;

  -- the source position represents the current pixel offset of the sprite to
  -- be copied to the frame buffer
  src_pos_counter : process (clk)
  begin
    if rising_edge(clk) then
      if state = INIT then
        -- set source position to first pixel
        src_pos.x <= (others => '0');
        src_pos.y <= (others => '0');
      elsif state = BLIT then
        if src_pos.x = sprite_size-1 then
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

  -- the load position represents the position of the next pixel to be loaded
  load_pos_counter : process (clk)
  begin
    if rising_edge(clk) then
      if state = INIT then
        -- set load position to first pixel
        load_pos.x <= (others => '0');
        load_pos.y <= (others => '0');
      elsif state = PRELOAD or state = BLIT then
        if load_pos.x = sprite_size-1 then
          load_pos.x <= (others => '0');

          if load_pos.y = sprite_size-1 then
            load_pos.y <= (others => '0');
          else
            load_pos.y <= load_pos.y + 1;
          end if;
        else
          load_pos.x <= load_pos.x + 1;
        end if;
      end if;
    end if;
  end process;

  -- latch fresh graphics data from the tile ROM while we are blitting the odd
  -- pixels to the frame buffer
  latch_gfx_data : process (clk)
  begin
    if rising_edge(clk) then
      if (state = PRELOAD or state = BLIT) and load_pos.x(0) = '1' then
        gfx_data <= din;
      end if;
    end if;
  end process;

  -- set sprite size
  sprite_size <= to_unsigned(sprite_size_in_pixels(sprite.size), sprite_size'length);

  -- the sprite is visible if it is enabled and has a non-zero size
  sprite_visible <= '1' when sprite.enable = '1' and sprite.size /= 0 else '0';

  -- set tile ROM address
  src_addr <= std_logic_vector(
    sprite.code(9 downto 4) &
    (sprite.code(3 downto 0) or (load_pos.y(4) & load_pos.x(4) & load_pos.y(3) & load_pos.x(3))) &
    load_pos.y(2 downto 0) &
    load_pos.x(2 downto 1)
  );

  -- set destination position and handle X/Y axis flipping
  dest_pos.x <= resize(sprite.pos.x+src_pos.x, dest_pos.x'length) when sprite.flip_x = '0' else
                resize(sprite.pos.x-src_pos.x+sprite_size-1, dest_pos.x'length);
  dest_pos.y <= resize(sprite.pos.y+src_pos.y, dest_pos.y'length) when sprite.flip_y = '0' else
                resize(sprite.pos.y-src_pos.y+sprite_size-1, dest_pos.y'length);

  -- the pre-blit is done when the first two pixels have been loaded
  preload_done <= '1' when state = PRELOAD and load_pos.x = 1 else '0';

  -- the blit is done when all the pixels have been copied
  blit_done <= '1' when state = BLIT and src_pos.x = sprite_size-1 and src_pos.y = sprite_size-1 else '0';

  -- set frame buffer write address
  dest_addr <= std_logic_vector(dest_pos.y(7 downto 0) & dest_pos.x(7 downto 0));

  -- set output data
  dout <= (std_logic_vector(sprite.priority & sprite.color) & gfx_data(7 downto 4)) when src_pos.x(0) = '0' else
          (std_logic_vector(sprite.priority & sprite.color) & gfx_data(3 downto 0));

  -- write to the frame buffer when we're blitting to the visible part of the frame
  busy <= '1' when state = BLIT and dest_pos.x(8) = '0' and dest_pos.y(8) = '0' else '0';

  -- set blit done output
  done <= '1' when state = INIT else '0';
end arch;
