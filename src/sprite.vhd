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
  type state_t is (INIT, BLIT);

  type sprite_pos_t is record
    x : unsigned(4 downto 0);
    y : unsigned(4 downto 0);
  end record sprite_pos_t;

  signal state, next_state : state_t;

  signal clk_12 : std_logic;
  signal cen_6 : std_logic;

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

  -- sprite size in pixels
  signal sprite_size : unsigned(5 downto 0);

  -- control signals
  signal blit_done : std_logic;

  -- pixel data
  signal pixel : nibble_t;
  signal pixel_2 : nibble_t;

  constant sprite : sprite_t := (
    code  => "00000000",
    pos   => (x => "001000000", y => "001000000"),
    size  => "01"
  );

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

  -- sprite pixel counters
  pixel_counters : process (clk_12)
  begin
    if rising_edge(clk_12) then
      if state = INIT then
        src_pos.x <= (others => '0');
        src_pos.y <= (others => '0');
      else
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

  fsm_sync : process (clk_12)
  begin
    if rising_edge(clk_12) then
      state <= next_state;
    end if;
  end process;

  fsm_comb : process (state)
  begin
    next_state <= state;

    if state = INIT then
      next_state <= BLIT;
    elsif state = BLIT then
      if blit_done = '1' then
        next_state <= INIT;
      end if;
    end if;
  end process;

  -- set sprite size
  sprite_size <= to_unsigned(sprite_size_in_pixels(sprite.size), sprite_size'length);

  -- TODO: handle flipping
  dest_pos.x <= resize(sprite.pos.x+src_pos.x, dest_pos.x'length);
  dest_pos.y <= resize(sprite.pos.y+src_pos.y, dest_pos.y'length);

  -- the blit is done when all the sprite pixels have been copied
  blit_done <= '1' when src_pos.x = sprite_size-1 and src_pos.y = sprite_size-1 else '0';

  frame_buffer_addr_wr <= std_logic_vector(dest_pos.y(7 downto 0) & dest_pos.x(7 downto 0));
  frame_buffer_din <= (others => '1');
  frame_buffer_wren <= '1' when dest_pos.x(8) = '0' and dest_pos.y(8) = '0' else '0';

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
