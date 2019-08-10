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

  constant TILE_ROM_ADDR_WIDTH     : natural := 15;
  constant FRAME_BUFFER_ADDR_WIDTH : natural := 16;
  constant FRAME_BUFFER_DATA_WIDTH : natural := 10;
  constant SPRITE_RAM_ADDR_WIDTH   : natural := 4;
  constant SPRITE_RAM_DATA_WIDTH   : natural := 64;

  -- byte 0
  constant SPRITE_HI_CODE_MSB : natural := 7;
  constant SPRITE_HI_CODE_LSB : natural := 4;
  constant SPRITE_ENABLE_BIT  : natural := 2;
  constant SPRITE_FLIP_Y_BIT  : natural := 1;
  constant SPRITE_FLIP_X_BIT  : natural := 0;

  -- byte 1
  constant SPRITE_LO_CODE_MSB : natural := 15;
  constant SPRITE_LO_CODE_LSB : natural := 8;

  -- byte 2
  constant SPRITE_SIZE_MSB : natural := 17;
  constant SPRITE_SIZE_LSB : natural := 16;

  -- byte 3
  constant SPRITE_PRIORITY_MSB : natural := 31;
  constant SPRITE_PRIORITY_LSB : natural := 30;
  constant SPRITE_HI_POS_Y_BIT : natural := 29;
  constant SPRITE_HI_POS_X_BIT : natural := 28;
  constant SPRITE_COLOR_MSB    : natural := 27;
  constant SPRITE_COLOR_LSB    : natural := 24;

  -- byte 4
  constant SPRITE_LO_POS_Y_MSB : natural := 39;
  constant SPRITE_LO_POS_Y_LSB : natural := 32;

  -- byte 5
  constant SPRITE_LO_POS_X_MSB : natural := 47;
  constant SPRITE_LO_POS_X_LSB : natural := 40;

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
    code     : unsigned(11 downto 0);
    color    : unsigned(3 downto 0);
    enable   : std_logic;
    flip_x   : std_logic;
    flip_y   : std_logic;
    pos      : pos_t;
    priority : unsigned(1 downto 0);
    size     : unsigned(1 downto 0);
  end record sprite_t;

  -- represents the position of a pixel in a sprite
  type sprite_pos_t is record
    x : unsigned(4 downto 0);
    y : unsigned(4 downto 0);
  end record sprite_pos_t;

  -- initialise sprite from a raw 64-bit value
  function init_sprite(data : std_logic_vector(SPRITE_RAM_DATA_WIDTH-1 downto 0)) return sprite_t;
end package types;

package body types is
  --  byte     bit        description
  -- --------+-76543210-+----------------
  --       0 | xxxx---- | hi code
  --         | -----x-- | enable
  --         | ------x- | flip Y
  --         | -------x | flip X
  --       1 | xxxxxxxx | lo code
  --       2 | ------xx | size
  --       3 | xx-------| priority
  --         | --x----- | hi pos Y
  --         | ---x---- | hi pos X
  --         | ----xxxx | colour
  --       4 | xxxxxxxx | lo pos Y
  --       5 | xxxxxxxx | lo pos X
  --       6 | -------- |
  --       7 | -------- |
  function init_sprite(data : std_logic_vector(SPRITE_RAM_DATA_WIDTH-1 downto 0)) return sprite_t is
    variable sprite : sprite_t;
  begin
    sprite.code     := unsigned(data(SPRITE_HI_CODE_MSB downto SPRITE_HI_CODE_LSB)) & unsigned(data(SPRITE_LO_CODE_MSB downto SPRITE_LO_CODE_LSB));
    sprite.color    := unsigned(data(SPRITE_COLOR_MSB downto SPRITE_COLOR_LSB));
    sprite.enable   := data(SPRITE_ENABLE_BIT);
    sprite.flip_x   := data(SPRITE_FLIP_X_BIT);
    sprite.flip_y   := data(SPRITE_FLIP_Y_BIT);
    sprite.pos.x    := data(SPRITE_HI_POS_X_BIT) & unsigned(data(SPRITE_LO_POS_X_MSB downto SPRITE_LO_POS_X_LSB));
    sprite.pos.y    := data(SPRITE_HI_POS_Y_BIT) & unsigned(data(SPRITE_LO_POS_Y_MSB downto SPRITE_LO_POS_Y_LSB));
    sprite.priority := unsigned(data(SPRITE_PRIORITY_MSB downto SPRITE_PRIORITY_LSB));
    sprite.size     := unsigned(data(SPRITE_SIZE_MSB downto SPRITE_SIZE_LSB));

    return sprite;
  end init_sprite;
end package body types;
