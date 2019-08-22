-- Copyright (c) 2019 Josh Bassett
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

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
-- There are four different sprite sizes – 8x8, 16x16, 32x32, and 64x64 – which
-- are all composed from one or more 8x8 tiles.
--
-- The data which describes the characteristics of each sprite – such as
-- position, size, etc. – is stored in the sprite RAM. The pixel data for the
-- 8x8 tiles which make up each sprite is stored in the sprite tile ROM.
entity sprite is
  port (
    -- clock
    clk : in std_logic;

    -- video signals
    video : in video_t;

    -- graphics data
    priority : out priority_t;
    data     : out byte_t
  );
end sprite;

architecture arch of sprite is
  type state_t is (INIT, LOAD, LATCH, BLIT, JUMP, DONE, FLIP);

  -- state signals
  signal state, next_state : state_t;

  -- sprite RAM signals
  signal sprite_ram_addr : std_logic_vector(SPRITE_RAM_ADDR_WIDTH-1 downto 0);
  signal sprite_ram_dout : std_logic_vector(SPRITE_RAM_DATA_WIDTH-1 downto 0);

  -- tile ROM signals
  signal tile_rom_addr : std_logic_vector(SPRITE_TILE_ROM_ADDR_WIDTH-1 downto 0);
  signal tile_rom_dout : byte_t;

  -- frame buffer signals
  signal frame_buffer_addr_rd : std_logic_vector(FRAME_BUFFER_ADDR_WIDTH-1 downto 0);
  signal frame_buffer_addr_wr : std_logic_vector(FRAME_BUFFER_ADDR_WIDTH-1 downto 0);
  signal frame_buffer_din     : std_logic_vector(FRAME_BUFFER_DATA_WIDTH-1 downto 0);
  signal frame_buffer_dout    : std_logic_vector(FRAME_BUFFER_DATA_WIDTH-1 downto 0);
  signal frame_buffer_flip    : std_logic;
  signal frame_buffer_rden    : std_logic;
  signal frame_buffer_wren    : std_logic;

  -- sprite counter
  signal sprite_counter : natural range 0 to 3;

  -- sprite descriptor
  signal sprite : sprite_t;

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
    ADDR_WIDTH => SPRITE_TILE_ROM_ADDR_WIDTH,
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
    dout    => frame_buffer_dout,
    rden    => frame_buffer_rden
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

  -- latch the next state
  latch_state : process (clk)
  begin
    if rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

  -- state machine
  fsm : process (state, video.vblank, blit_done, frame_done)
  begin
    next_state <= state;

    case state is
      -- this is the default state, we just wait for the beginning of the frame
      when INIT =>
        if video.vblank = '0' then
          next_state <= LOAD;
        end if;

      -- load the sprite
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
        if video.vblank = '1' then
          next_state <= FLIP;
        end if;

      -- flip the frame buffer
      when FLIP =>
        next_state <= INIT;
    end case;
  end process;

  -- Update the sprite counter.
  --
  -- Sprites are sorted from lowest to highest priority. If sprites are
  -- overlapping, then the higher priority sprites will appear above lower
  -- priority sprites.
  update_sprite_counter : process (clk)
  begin
    if rising_edge(clk) then
      if state = JUMP then
        sprite_counter <= sprite_counter + 1;
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

  -- start a blit operation
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

  -- flip the frame buffer page
  flip_frame_buffer: process(clk)
  begin
    if rising_edge(clk) then
      if state = FLIP then
        frame_buffer_flip <= not frame_buffer_flip;
      end if;
    end if;
  end process;

  -- set sprite RAM address
  sprite_ram_addr <= std_logic_vector(to_unsigned(sprite_counter, sprite_ram_addr'length));

  -- the frame is done when all the sprites have been blitted
  frame_done <= '1' when sprite_counter = sprite_counter'high else '0';

  -- set frame buffer read address
  frame_buffer_addr_rd <= std_logic_vector(video.pos.y(7 downto 0) & video.pos.x(7 downto 0));

  -- read from the frame buffer when video output is enabled
  frame_buffer_rden <= video.enable;

  -- set layer data
  priority <= unsigned(frame_buffer_dout(9 downto 8));
  data     <= frame_buffer_dout(7 downto 0);
end arch;
