
# Clock signal
#Bank = 34, Pin name = ,					Sch name = CLK100MHZ
set_property PACKAGE_PIN W5 [get_ports clk]
set_property IOSTANDARD LVCMOS33 [get_ports clk]
create_clock -period 6.400 -name sys_clk_pin -waveform {0.000 3.200} -add [get_ports clk]

# Pin constraints
set_property PACKAGE_PIN V17 [get_ports {arst_g}]
set_property IOSTANDARD LVCMOS33 [get_ports {arst_g}]
set_property PACKAGE_PIN V16 [get_ports {msg}]
set_property IOSTANDARD LVCMOS33 [get_ports {msg}]
set_property PACKAGE_PIN W16 [get_ports {msg_rd}]
set_property IOSTANDARD LVCMOS33 [get_ports {msg_rd}]
set_property PACKAGE_PIN W17 [get_ports {som}]
set_property IOSTANDARD LVCMOS33 [get_ports {som}]
set_property PACKAGE_PIN W15 [get_ports {eom}]
set_property IOSTANDARD LVCMOS33 [get_ports {eom}]
set_property PACKAGE_PIN V15 [get_ports {dgt}]
set_property IOSTANDARD LVCMOS33 [get_ports {dgt}]
set_property PACKAGE_PIN V16 [get_ports {rdy}]
set_property IOSTANDARD LVCMOS33 [get_ports {rdy}]

# Bitstream contraints
set_property BITSTREAM.GENERAL.COMPRESS TRUE [current_design]
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design]
set_property CONFIG_MODE SPIx4 [current_design]

set_property BITSTREAM.CONFIG.CONFIGRATE 33 [current_design]

set_property CONFIG_VOLTAGE 3.3 [current_design]
set_property CFGBVS VCCO [current_design]












