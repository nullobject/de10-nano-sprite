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

package types is
  subtype byte_t is std_logic_vector(7 downto 0);
  subtype nibble_t is std_logic_vector(3 downto 0);

  constant TILE_ROM_ADDR_WIDTH : natural := 15;

  -- represents a position
  type pos_t is record
    x : unsigned(8 downto 0);
    y : unsigned(8 downto 0);
  end record pos_t;

  -- represents the horizontal and vertical sync signals
  type sync_t is record
    hsync : std_logic;
    vsync : std_logic;
  end record sync_t;

  -- represents the horizontal and vertical blank signals
  type blank_t is record
    hblank : std_logic;
    vblank : std_logic;
  end record blank_t;

  -- represents a sprite
  type sprite_t is record
    code   : unsigned(11 downto 0);
    enable : std_logic;
    pos    : pos_t;
    size   : unsigned(1 downto 0);
  end record sprite_t;

  -- represents the position of a pixel in a sprite
  type sprite_pos_t is record
    x : unsigned(4 downto 0);
    y : unsigned(4 downto 0);
  end record sprite_pos_t;
end package types;
