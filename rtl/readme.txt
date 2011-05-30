SPI_MASTER_SLAVE
----------------

VHDL files for spi master/slave project:

	spi_master.vhd		spi master module, can be used independently
	spi_slave.vhd		spi slave module, can be used independently
	spi_loopback.vhd	wrapper module for the master and slave modules
	spi_loopback_test.vhd	testbench for the loopback module, test master against slave


The original development is done in Xilinx ISE 13.1, targeted to a Spartan-6 device.

Verification was done in ISIM, after Place & Route, with default constraints, for the slowest 
Spartan-6 device, tested at 50MHz for the spi_2x_clk (25MHz spi SCK), and 125MHz for the parallel
interfaces clocks.


