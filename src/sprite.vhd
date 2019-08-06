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
  signal clk_12 : std_logic;
  signal cen_6 : std_logic;
  signal move_clken : std_logic;

  -- video signals
  signal video_pos   : pos_t;
  signal video_sync  : sync_t;
  signal video_blank : blank_t;
  signal video_on    : std_logic;

  -- frame buffer
  signal frame_buffer_addr_rd : std_logic_vector(15 downto 0);
  signal frame_buffer_addr_wr : std_logic_vector(15 downto 0);
  signal frame_buffer_din     : std_logic_vector(7 downto 0);
  signal frame_buffer_dout    : std_logic_vector(7 downto 0);
  signal frame_buffer_flip    : std_logic;
  signal frame_buffer_rden    : std_logic;
  signal frame_buffer_wren    : std_logic;

  signal vblank_falling : std_logic;

  signal x : natural range 0 to 255;
  signal y : natural range 0 to 255;
  signal n : natural range 0 to 255;

  -- pixel data
  signal pixel : nibble_t;
  signal pixel_2 : nibble_t;
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

  move_clken_gen : entity work.clock_divider
  generic map (DIVISOR => 600000)
  port map (clk => clk_12, cen => move_clken);

  sync_gen : entity work.sync_gen
  port map (
    clk   => clk_12,
    cen   => cen_6,
    pos   => video_pos,
    sync  => video_sync,
    blank => video_blank
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

  page_flipper : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if vblank_falling = '1' then
        frame_buffer_flip <= not frame_buffer_flip;
      end if;
    end if;
  end process;

  process (clk_12)
  begin
    if rising_edge(clk_12) then
      if x = 63 then
        x <= 0;

        if y = 63+16 then
          y <= 16;
        else
          y <= y + 1;
        end if;
      else
        x <= x + 1;
      end if;

      if move_clken = '1' then
        n <= n + 1;
      end if;
    end if;
  end process;

  frame_buffer_addr_wr <= std_logic_vector(to_unsigned(y, 8) & to_unsigned(x+n, 8));
  frame_buffer_din <= (others => '1');
  frame_buffer_wren <= '1';

  frame_buffer_addr_rd <= std_logic_vector(video_pos.y(7 downto 0) & video_pos.x(7 downto 0));
  frame_buffer_rden <= not (video_blank.hblank or video_blank.vblank);

  pixel <= frame_buffer_dout(7 downto 4);

  video_on <= not (video_blank.hblank or video_blank.vblank);
  vga_csync <= not (video_sync.hsync xor video_sync.vsync);

  process (clk_12)
  begin
    if rising_edge(clk_12) then
      if cen_6 = '1' then
        pixel_2 <= pixel;
        if video_on = '1' then
          vga_r <= pixel_2 & pixel_2(3 downto 2);
          vga_g <= pixel_2 & pixel_2(3 downto 2);
          vga_b <= pixel_2 & pixel_2(3 downto 2);
        else
          vga_r <= (others => '0');
          vga_g <= (others => '0');
          vga_b <= (others => '0');
        end if;
      end if;
    end if;
  end process;
end arch;
