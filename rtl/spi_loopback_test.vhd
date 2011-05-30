--------------------------------------------------------------------------------
-- Company: 
-- Engineer:        Jonny Doin
--
-- Create Date:     22:59:18 04/25/2011
-- Design Name:   
-- Module Name:     C:/dropbox/Dropbox/VHDL_training/projects/SPI_interface/spi_loopback_test.vhd
-- Project Name:    SPI_interface
-- Target Device:   Spartan-6
-- Tool versions:   ISE 13.1
-- Description:     Testbench to simulate the master and slave SPI interfaces. Each module can be tested
--                  in a "real" environment, where the 'spi_master' exchanges data with the 'spi_slave'
--                  module, simulating the internal working of each design.
--                  In behavioral simulation, select a matching data width (N) and spi mode (CPOL, CPHA) for
--                  both modules, and also a different clock domain for each parallel interface.
--                  Different values for PREFETCH for each interface can be tested, to model the best value
--                  for the pipelined memory / bus that is attached to the di/do ports.
--                  To test the parallel interfaces, a simple ROM memory is simulated for each interface, with
--                  8 words of data to be sent, synchronous to each clock and flow control signals.
--
-- 
-- VHDL Test Bench Created by ISE for modules: 'spi_master' and 'spi_slave'
-- 
-- Dependencies:
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Revision 1.05 - Implemented FIFO simulation for each interface.
-- Additional Comments:
--
-- Notes: 
-- This testbench has been automatically generated using types std_logic and
-- std_logic_vector for the ports of the unit under test.  Xilinx recommends
-- that these types always be used for the top-level I/O of a design in order
-- to guarantee that the testbench will bind correctly to the post-implementation 
-- simulation model.
--------------------------------------------------------------------------------
LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

--library WORK;
--use WORK.DEBUG_PKG.ALL;
 
ENTITY spi_loopback_test IS
    Generic (   N : positive := 32);                                                -- 32bit serial word length is default
END spi_loopback_test;
 
ARCHITECTURE behavior OF spi_loopback_test IS 

    --=========================================================
    -- Component Declarations for the Unit Under Test (UUT)
    --=========================================================

	COMPONENT spi_loopback
	PORT(
        ----------------MASTER-----------------------
        m_spi_clk_i : IN std_logic := 'X';
        m_par_clk_i : IN std_logic := 'X';
        m_rst_i : IN std_logic := 'X';
        m_spi_ssel_o : OUT std_logic;
        m_spi_sck_o : OUT std_logic;
        m_spi_mosi_o : OUT std_logic;
        m_spi_miso_i : IN std_logic := 'X';
        m_di_i : IN std_logic_vector(31 downto 0) := (others => 'X');
        m_do_o : OUT std_logic_vector(31 downto 0);
        m_di_rdy_o : OUT std_logic;
        m_wren_i : IN std_logic := 'X';
        m_do_valid_o : OUT std_logic;
--        m_state_dbg_o : OUT std_logic_vector(5 downto 0);
--        m_sh_reg_dbg_o : OUT std_logic_vector(31 downto 0);
        ----------------SLAVE-----------------------
		s_clk_i : IN std_logic := 'X';
		s_rst_i : IN std_logic := 'X';
		s_spi_ssel_i : IN std_logic := 'X';
		s_spi_sck_i : IN std_logic := 'X';
		s_spi_mosi_i : IN std_logic := 'X';
		s_spi_miso_o : OUT std_logic;
		s_di_i : IN std_logic_vector(31 downto 0) := (others => 'X');
		s_do_o : OUT std_logic_vector(31 downto 0);
		s_di_rdy_o : OUT std_logic;
		s_wren_i : IN std_logic := 'X';
		s_do_valid_o : OUT std_logic
--		s_state_dbg_o : OUT std_logic_vector(5 downto 0);
--		s_sh_reg_dbg_o : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;


    --=========================================================
    -- constants
    --=========================================================
    constant fifo_memory_size : integer := 8;
    
    --=========================================================
    -- types
    --=========================================================
    type fifo_memory_type is array (0 to fifo_memory_size-1) of std_logic_vector (N-1 downto 0);

    --=========================================================
    -- signals to connect the instances
    --=========================================================
    -- internal clk and rst
    signal spi_2x_clk : std_logic := '0';           -- This is the SPI_SCK clock source. Must be 2x spi sck.
    signal m_clk : std_logic := '0';                -- clock domain for the master parallel interface. Must be faster than spi bus sck.
    signal s_clk : std_logic := '0';                -- clock domain for the slave parallel interface. Must be faster than spi bus sck.
    signal rst : std_logic := 'X';
    -- spi bus wires
    signal spi_sck : std_logic;
    signal spi_ssel : std_logic;
    signal spi_miso : std_logic;
    signal spi_mosi : std_logic;
    -- master parallel interface
    signal di_m : std_logic_vector (N-1 downto 0) := (others => '0');
    signal do_m : std_logic_vector (N-1 downto 0);
    signal do_valid_m : std_logic;
    signal di_rdy_m : std_logic;
    signal wren_m : std_logic := '0';
--    signal sh_reg_m : integer;
--    signal state_m : integer;
    -- slave parallel interface
    signal di_s : std_logic_vector (N-1 downto 0) := (others => '0');
    signal do_s : std_logic_vector (N-1 downto 0);
    signal do_valid_s : std_logic;
    signal di_rdy_s : std_logic;
    signal wren_s : std_logic := '0';
--    signal sh_reg_s : integer;
--    signal state_s : integer;

    --=========================================================
    -- Clock period definitions
    --=========================================================
    constant spi_2x_clk_period : time := 20 ns;     -- 33.3MHz SPI SCK clock
    constant m_clk_period : time := 8 ns;           -- 125MHz master parallel clock
    constant s_clk_period : time := 8 ns;           -- 125MHz slave parallel clock

BEGIN
 
    --=========================================================
    -- instantiation of UUT
    --=========================================================

	Inst_spi_loopback: spi_loopback PORT MAP(
        ----------------MASTER-----------------------
        m_spi_clk_i => spi_2x_clk,
        m_par_clk_i => m_clk,
        m_rst_i => rst,
        m_spi_ssel_o => spi_ssel,
        m_spi_sck_o => spi_sck,
        m_spi_mosi_o => spi_mosi,
        m_spi_miso_i => spi_miso,
        m_di_i => di_m,
        m_do_o => do_m,
        m_di_rdy_o => di_rdy_m,
        m_wren_i => wren_m,
        m_do_valid_o => do_valid_m,
--        m_state_dbg_o => state_m,
--        m_sh_reg_dbg_o => sh_reg_m,
        ----------------SLAVE-----------------------
		s_clk_i => s_clk,
		s_rst_i => rst,
		s_spi_ssel_i => spi_ssel,
		s_spi_sck_i => spi_sck,
		s_spi_mosi_i => spi_mosi,
		s_spi_miso_o => spi_miso,
		s_di_i => di_s,
		s_do_o => do_s,
		s_di_rdy_o => di_rdy_s,
		s_wren_i => wren_s,
		s_do_valid_o => do_valid_s
--		s_state_dbg_o => state_s,
--		s_sh_reg_dbg_o => sh_reg_s
	);

    --=========================================================
    -- Clock generator processes
    --=========================================================
    spi_2x_clk_process : process
    begin
        spi_2x_clk <= '0';
        wait for spi_2x_clk_period/2;
        spi_2x_clk <= '1';
        wait for spi_2x_clk_period/2;
    end process spi_2x_clk_process;

    m_clk_process : process
    begin
        m_clk <= '0';
        wait for m_clk_period/2;
        m_clk <= '1';
        wait for m_clk_period/2;
    end process m_clk_process;

    s_clk_process : process
    begin
        s_clk <= '0';
        wait for s_clk_period/2;
        s_clk <= '1';
        wait for s_clk_period/2;
    end process s_clk_process;

    --=========================================================
    -- rst_i process
    --=========================================================
    rst <= '0', '1' after 100 ns, '0' after 200 ns;
    
    --=========================================================
    -- Master interface process
    --=========================================================
    master_tx_fifo_proc: process is
        variable fifo_memory : fifo_memory_type := 
            (X"87654321",X"abcdef01",X"faceb007",X"10203049",X"85a5a5a5",X"7aaa5551",X"7adecabe",X"57564789");
        variable fifo_head : integer range 0 to fifo_memory_size-1;
    begin
        -- synchronous rst_i
        wait until rst = '1';
        wait until m_clk'event and m_clk = '1';
        di_m <= (others => '0');
        wren_m <= '0';
        fifo_head := 0;
        wait until rst = '0';
        -- load next fifo contents into shift register
        for cnt in 0 to fifo_memory_size-1 loop
            fifo_head := cnt;                               -- pre-compute next pointer 
            wait until di_rdy_m = '1';                      -- wait shift register request for data
            wait until m_clk'event and m_clk = '1';           -- sync fifo data load at next rising edge
            di_m <= fifo_memory(fifo_head);                 -- place data into tx_data input bus
            wait until m_clk'event and m_clk = '1';           -- sync fifo data load at next rising edge
            wren_m <= '1';                                  -- write data into spi master
            wait until di_rdy_m = '0';                      -- wait data be accepted to compute next pointer
            wait until m_clk'event and m_clk = '1';           -- sync fifo data load at next rising edge
            wren_m <= '0';                                  -- remove write enable signal
        end loop;
        wait;
    end process master_tx_fifo_proc;


    --=========================================================
    -- Slave interface process
    --=========================================================
    slave_tx_fifo_proc: process is
        variable fifo_memory : fifo_memory_type := 
            (X"90201031",X"97640231",X"ef55aaf1",X"babaca51",X"b00b1ee5",X"51525354",X"81828384",X"91929394");
        variable fifo_head : integer range 0 to fifo_memory_size-1;
    begin
        -- synchronous rst_i
        wait until rst = '1';
        wait until s_clk'event and s_clk = '1';
        di_s <= (others => '0');
        wren_s <= '0';
        fifo_head := 0;
        wait until rst = '0';
        -- load next fifo contents into shift register
        for cnt in 0 to fifo_memory_size-1 loop
            fifo_head := cnt;                               -- pre-compute next pointer 
            wait until di_rdy_s = '1';                      -- wait shift register request for data
            wait until s_clk'event and s_clk = '1';           -- sync fifo data load at next rising edge
            di_s <= fifo_memory(fifo_head);                 -- place data into tx_data input bus
            wait until s_clk'event and s_clk = '1';           -- sync fifo data load at next rising edge
            wren_s <= '1';                                  -- write data into shift register
            wait until di_rdy_s = '0';                      -- wait data be accepted to compute next pointer
            wait until s_clk'event and s_clk = '1';           -- sync fifo data load at next rising edge
            wren_s <= '0';                                  -- remove write enable signal
        end loop;
        wait;
    end process slave_tx_fifo_proc;
 
    --=========================================================
    -- Debug processes
    --=========================================================

--    sh_reg_m <= dbg_shift_m;
--    state_m <= dbg_state_m;
--    sh_reg_s <= dbg_shift_s;
--    state_s <= dbg_state_s;

END ARCHITECTURE behavior;
