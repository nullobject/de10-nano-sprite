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

library pll;

use work.types.all;

entity top is
  port (
    -- 50MHz reference clock
    clk : in std_logic;

    -- VGA signals
    vga_r, vga_g, vga_b : out std_logic_vector(5 downto 0);
    vga_csync           : out std_logic
  );
end top;

architecture arch of top is
  -- clock signals
  signal rom_clk : std_logic;
  signal sys_clk : std_logic;
  signal cen_6   : std_logic;

  -- video signals
  signal video : video_t;

  -- sprite priority data
  signal sprite_priority : priority_t;

  -- sprite data
  signal sprite_data : byte_t;

  -- RGB data
  signal rgb : nibble_t;
begin
  -- generate a 12MHz clock signal
  my_pll : entity pll.pll
  port map (
    refclk   => clk,
    rst      => '0',
    outclk_0 => SDRAM_CLK,
    outclk_1 => rom_clk,
    outclk_2 => sys_clk,
    locked   => open
  );

  -- generate a 6MHz clock enable signal
  clock_divider_6 : entity work.clock_divider
  generic map (DIVISOR => 2)
  port map (clk => sys_clk, cen => cen_6);

  -- video timing generator
  sync_gen : entity work.sync_gen
  port map (
    clk   => sys_clk,
    cen_6 => cen_6,
    video => video
  );

  -- sprite layer
  sprite_layer : entity work.sprite
  port map (
    clk      => sys_clk,
    video    => video,
    priority => sprite_priority,
    data     => sprite_data
  );

  -- latch RGB data from the palette RAM
  latch_pixel_data : process (sys_clk)
  begin
    if rising_edge(sys_clk) then
      if cen_6 = '1' then
        if video.enable = '1' then
          vga_r <= rgb & rgb(3 downto 2);
          vga_g <= rgb & rgb(3 downto 2);
          vga_b <= rgb & rgb(3 downto 2);
        else
          vga_r <= (others => '0');
          vga_g <= (others => '0');
          vga_b <= (others => '0');
        end if;
      end if;
    end if;
  end process;

  -- set the RGB data
  rgb <= sprite_data(3 downto 0);

  -- composite sync
  vga_csync <= not (video.hsync xor video.vsync);
end arch;
