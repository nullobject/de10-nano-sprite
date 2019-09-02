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

-- The ROM controller provides an interface to the GPU for accessing tile data.
--
-- The original arcade hardware has multiple ROM chips that store the tile data
-- for the different graphics layers. Unfortunately, the Cyclone V chip doesn't
-- have enough memory blocks for us to implement these ROMs. Instead, we need
-- to store the tile data in the SDRAM.
--
-- The tile ROMs are accessed concurrently by the GPU, so the job of the ROM
-- controller is to manage reading the tile data from the SDRAM in a fair, and
-- timely manner.
entity rom_controller is
  port (
    -- clock signals
    clk   : in std_logic;
    reset : in std_logic;

    -- ROM interface
    sprite_rom_addr : in unsigned(SPRITE_ROM_ADDR_WIDTH-1 downto 0);
    sprite_rom_data : out std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    char_rom_addr   : in unsigned(CHAR_ROM_ADDR_WIDTH-1 downto 0);
    char_rom_data   : out std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    fg_rom_addr     : in unsigned(FG_ROM_ADDR_WIDTH-1 downto 0);
    fg_rom_data     : out std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    bg_rom_addr     : in unsigned(BG_ROM_ADDR_WIDTH-1 downto 0);
    bg_rom_data     : out std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);

    -- SDRAM interface
    sdram_addr  : out unsigned(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
    sdram_din   : out std_logic_vector(SDRAM_INPUT_DATA_WIDTH-1 downto 0);
    sdram_dout  : in std_logic_vector(SDRAM_OUTPUT_DATA_WIDTH-1 downto 0);
    sdram_we    : out std_logic;
    sdram_ack   : in std_logic;
    sdram_valid : in std_logic;
    sdram_ready : in std_logic;

    -- IOCTL interface
    ioctl_addr     : in unsigned(IOCTL_ADDR_WIDTH-1 downto 0);
    ioctl_data     : in byte_t;
    ioctl_wr       : in std_logic;
    ioctl_download : in std_logic
  );
end rom_controller;

architecture arch of rom_controller is
  type state_t is (IDLE, WRITE);
  type rom_t is (SPRITE_ROM, CHAR_ROM, FG_ROM, BG_ROM);

  -- state signals
  signal state, next_state : state_t;

  -- currently enabled ROM
  signal current_rom : rom_t;

  -- control signals
  signal start : std_logic;

  -- chip select signals
  signal sprite_rom_cs : std_logic;
  signal char_rom_cs   : std_logic;
  signal fg_rom_cs     : std_logic;
  signal bg_rom_cs     : std_logic;

  -- address mux signals
  signal ioctl_sdram_addr      : unsigned(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal sprite_rom_sdram_addr : unsigned(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal char_rom_sdram_addr   : unsigned(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal fg_rom_sdram_addr     : unsigned(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
  signal bg_rom_sdram_addr     : unsigned(SDRAM_INPUT_ADDR_WIDTH-1 downto 0);
begin
  sprite_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => SPRITE_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#00#
  )
  port map (
    clk => clk,

    cs => sprite_rom_cs,

    -- ROM interface
    rom_addr => sprite_rom_addr,
    rom_data => sprite_rom_data,

    -- SDRAM interface
    sdram_addr  => sprite_rom_sdram_addr,
    sdram_data  => sdram_dout,
    sdram_valid => sdram_valid
  );

  char_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => CHAR_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#10#
  )
  port map (
    clk => clk,

    cs => char_rom_cs,

    -- ROM interface
    rom_addr => char_rom_addr,
    rom_data => char_rom_data,

    -- SDRAM interface
    sdram_addr  => char_rom_sdram_addr,
    sdram_data  => sdram_dout,
    sdram_valid => sdram_valid
  );

  fg_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => FG_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#20#
  )
  port map (
    clk => clk,

    cs => fg_rom_cs,

    -- ROM interface
    rom_addr => fg_rom_addr,
    rom_data => fg_rom_data,

    -- SDRAM interface
    sdram_addr  => fg_rom_sdram_addr,
    sdram_data  => sdram_dout,
    sdram_valid => sdram_valid
  );

  bg_rom_segment : entity work.segment
  generic map (
    ROM_ADDR_WIDTH => BG_ROM_ADDR_WIDTH,
    SEGMENT_OFFSET => 16#30#
  )
  port map (
    clk => clk,

    cs => bg_rom_cs,

    -- ROM interface
    rom_addr => bg_rom_addr,
    rom_data => bg_rom_data,

    -- SDRAM interface
    sdram_addr  => bg_rom_sdram_addr,
    sdram_data  => sdram_dout,
    sdram_valid => sdram_valid
  );

  -- Convert bytes received from IOCTL interface into 32-bit words.
  --
  -- The SDRAM controller has a 32-bit interface, so we need to buffer bytes
  -- received from the IOCTL interface in order to write 32-bit words to the
  -- SDRAM.
  download_buffer : entity work.download_buffer
  generic map (SIZE => 4)
  port map (
    reset => reset,
    clk   => clk,
    din   => ioctl_data,
    dout  => sdram_din,
    we    => ioctl_download and ioctl_wr,
    valid => start
  );

  -- state machine
  fsm : process (state, start, sdram_ack)
  begin
    next_state <= state;

    case state is
      -- this is the default state, we just wait for the start signal
      when IDLE =>
        if start = '1' then
          next_state <= WRITE;
        end if;

      -- write to the SDRAM
      when WRITE =>
        if sdram_ack = '1' then
          next_state <= IDLE;
        end if;
    end case;
  end process;

  -- latch the next state
  latch_next_state : process (clk, reset)
  begin
    if reset = '1' then
      state <= IDLE;
    elsif rising_edge(clk) then
      state <= next_state;
    end if;
  end process;

  -- update the current ROM
  update_current_rom : process (clk, reset)
  begin
    if reset = '1' then
      current_rom <= SPRITE_ROM;
    elsif rising_edge(clk) then
      if sdram_valid = '1' then
        current_rom <= rom_t'succ(current_rom);
      end if;
    end if;
  end process;

  -- set the chip select signals
  sprite_rom_cs <= '1' when state = IDLE and current_rom = SPRITE_ROM else '0';
  char_rom_cs   <= '1' when state = IDLE and current_rom = CHAR_ROM   else '0';
  fg_rom_cs     <= '1' when state = IDLE and current_rom = FG_ROM     else '0';
  bg_rom_cs     <= '1' when state = IDLE and current_rom = BG_ROM     else '0';

  -- Set the IOCTL address if we're writing, otherwise set it to zero.
  --
  -- We need to divide the IOCTL address by four, because we are writing
  -- a 32-bit word to the SDRAM.
  ioctl_sdram_addr <= resize(shift_right(ioctl_addr, 2), ioctl_sdram_addr'length) when ioctl_download = '1' else (others => '0');

  -- mux the SDRAM address
  sdram_addr <= ioctl_sdram_addr or
                sprite_rom_sdram_addr or
                char_rom_sdram_addr or
                fg_rom_sdram_addr or
                bg_rom_sdram_addr;

  -- set SDRAM write enable
  sdram_we <= '1' when state = WRITE else '0';
end architecture arch;
