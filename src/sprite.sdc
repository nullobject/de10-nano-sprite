#
# Copyright (c) 2017 Intel Corporation
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to
# deal in the Software without restriction, including without limitation the
# rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
# sell copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
#

# inform quartus that the clk port brings a 50MHz clock into our design so
# that timing closure on our design can be analyzed

create_clock -name clk -period 20 [get_ports clk]

derive_pll_clocks

create_generated_clock -name SDRAM_CLK -source [get_pins {my_pll|pll_inst|altera_pll_i|outclk_wire[2]~CLKENA0|outclk}] [get_ports {SDRAM_CLK}]

derive_clock_uncertainty

# This is tAC in the data sheet
set_input_delay -max -clock SDRAM_CLK 6.0ns [get_ports SDRAM_DQ[*]]
# this is tOH in the data sheet
set_input_delay -min -clock SDRAM_CLK 2.5ns [get_ports SDRAM_DQ[*]]

# This is tIS in the data sheet (setup time)
set_output_delay -max -clock SDRAM_CLK 1.5ns [get_ports {SDRAM_D* SDRAM_A* SDRAM_BA* SDRAM_n* SDRAM_CKE}]
# This is tIH in the data sheet (hold time)
set_output_delay -min -clock SDRAM_CLK 0.8ns [get_ports {SDRAM_D* SDRAM_A* SDRAM_BA* SDRAM_n* SDRAM_CKE}]

# inform quartus that the LED output port has no critical timing requirements
# its a single output port driving an LED, there are no timing relationships
# that are critical for this

set_false_path -from * -to [get_ports {vga_*}]
