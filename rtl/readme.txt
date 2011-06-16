SPI_MASTER_SLAVE
----------------

VHDL files for spi master/slave project:

	spi_master.vhd		spi master module, can be used independently
	spi_slave.vhd		spi slave module, can be used independently
	spi_loopback.vhd	wrapper module for the master and slave modules
	spi_loopback_test.vhd	testbench for the loopback module, test master against slave
	spi_loopback.ucf	constraints file for Spartan-6, optimized for area, LUT compression.


The original development is done in Xilinx ISE 13.1, targeted to a Spartan-6 device.

Verification was done in ISIM, after Place & Route, with default constraints, for the slowest 
Spartan-6 device, synthesis generated 59 slices, and the design was tested at 40MHz for the 
spi_2x_clk (20MHz spi SCK), and 125MHz for the parallel interfaces clocks.
With the attached .ucf file, optimized for area and using LUT compression, synthesis generated
44 slices, and design tested OK at 20MHz of SPI clock.
 


