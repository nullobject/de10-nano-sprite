library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library pll;

use work.types.all;

entity top is
  port (
    clk : in std_logic;
    vga_r, vga_g, vga_b : out std_logic_vector(5 downto 0);
    vga_csync : out std_logic
  );
end top;

architecture arch of top is
  -- clock signals
  signal clk_12 : std_logic;
  signal cen_6  : std_logic;

  -- video signals
  signal video : video_t;

  -- sprite data
  signal sprite_data : std_logic_vector(FRAME_BUFFER_DATA_WIDTH-1 downto 0);

  -- pixel data
  signal pixel : nibble_t;
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
    video => video
  );

  sprite : entity work.sprite
  port map (
    clk   => clk_12,
    video => video,
    data  => sprite_data
  );

  video_output : process (clk_12)
  begin
    if rising_edge(clk_12) and cen_6 = '1' then
      if video.enable = '1' then
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

  -- set the pixel data
  pixel <= sprite_data(3 downto 0);

  vga_csync <= video.csync;
end arch;
