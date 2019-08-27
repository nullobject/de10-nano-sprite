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
    vga_csync           : out std_logic;

    -- SDRAM interface
    SDRAM_A    : out std_logic_vector(SDRAM_ADDR_WIDTH-1 downto 0);
    SDRAM_BA   : out std_logic_vector(SDRAM_BANK_WIDTH-1 downto 0);
    SDRAM_DQ   : inout std_logic_vector(SDRAM_DATA_WIDTH-1 downto 0);
    SDRAM_CLK  : out std_logic;
    SDRAM_nCS  : out std_logic;
    SDRAM_nRAS : out std_logic;
    SDRAM_nCAS : out std_logic;
    SDRAM_nWE  : out std_logic;
    SDRAM_CKE  : out std_logic;
    SDRAM_DQML : out std_logic;
    SDRAM_DQMH : out std_logic
  );
end top;

architecture arch of top is
  -- clock signals
  signal rom_clk : std_logic;
  signal sys_clk : std_logic;
  signal cen_6   : std_logic;

  -- SDRAM signals
  signal sdram_addr  : std_logic_vector(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal sdram_din   : std_logic_vector(SDRAM_INPUT_DATA_WIDTH-1 downto 0) := (others => '0');
  signal sdram_dout  : std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
  signal sdram_rden  : std_logic;
  signal sdram_wren  : std_logic;
  signal sdram_ready : std_logic;
  signal sdram_valid : std_logic;

  -- video signals
  signal video : video_t;

  -- ROM signals
  signal sprite_rom_addr : std_logic_vector(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
  signal sprite_rom_data : std_logic_vector(SPRITE_ROM_DATA_WIDTH-1 downto 0);

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

  -- SDRAM controller
  sdram : entity work.sdram
  port map (
    clk => rom_clk,

    reset => '0',

    -- IO interface
    addr  => sdram_addr,
    din   => sdram_din,
    dout  => sdram_dout,
    ready => sdram_ready,
    valid => sdram_valid,
    rden  => sdram_rden,
    wren  => sdram_wren,

    -- SDRAM interface
    sdram_a     => SDRAM_A,
    sdram_ba    => SDRAM_BA,
    sdram_dq    => SDRAM_DQ,
    sdram_cke   => SDRAM_CKE,
    sdram_cs_n  => SDRAM_nCS,
    sdram_ras_n => SDRAM_nRAS,
    sdram_cas_n => SDRAM_nCAS,
    sdram_we_n  => SDRAM_nWE,
    sdram_dqml  => SDRAM_DQML,
    sdram_dqmh  => SDRAM_DQMH
  );

  tile_rom : entity work.single_port_rom
  generic map (
    ADDR_WIDTH => SPRITE_ROM_ADDR_WIDTH,
    DATA_WIDTH => SPRITE_ROM_DATA_WIDTH,
    INIT_FILE  => "rom/vid_6g.mif"
  )
  port map (
    clk  => sys_clk,
    addr => sprite_rom_addr,
    dout => sprite_rom_data
  );

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
    rom_addr => sprite_rom_addr,
    rom_data => sprite_rom_data,
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
