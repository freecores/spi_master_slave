----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    23:44:37 05/17/2011 
-- Design Name: 
-- Module Name:    spi_loopback - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity spi_loopback is
    port(
        ----------------MASTER-----------------------
        m_spi_clk_i : IN std_logic;
        m_par_clk_i : IN std_logic;
        m_rst_i : IN std_logic;
        m_spi_ssel_o : OUT std_logic;
        m_spi_sck_o : OUT std_logic;
        m_spi_mosi_o : OUT std_logic;
        m_spi_miso_i : IN std_logic;
        m_di_i : IN std_logic_vector(31 downto 0);
        m_do_o : OUT std_logic_vector(31 downto 0);
        m_di_rdy_o : OUT std_logic;
        m_wren_i : IN std_logic;          
        m_do_valid_o : OUT std_logic;
--        m_state_dbg_o : OUT std_logic_vector(5 downto 0);
--        m_sh_reg_dbg_o : OUT std_logic_vector(31 downto 0);
        ----------------SLAVE-----------------------
		s_clk_i : IN std_logic;
		s_rst_i : IN std_logic;
		s_spi_ssel_i : IN std_logic;
		s_spi_sck_i : IN std_logic;
		s_spi_mosi_i : IN std_logic;
		s_spi_miso_o : OUT std_logic;
		s_di_i : IN std_logic_vector(31 downto 0);
		s_do_o : OUT std_logic_vector(31 downto 0);
		s_di_rdy_o : OUT std_logic;
		s_wren_i : IN std_logic;          
		s_do_valid_o : OUT std_logic
--		s_state_dbg_o : OUT std_logic_vector(5 downto 0);
--		s_sh_reg_dbg_o : OUT std_logic_vector(31 downto 0)
        );
end spi_loopback;

architecture Behavioral of spi_loopback is

	COMPONENT spi_master
	PORT(
		spi_2x_clk_i : IN std_logic;
		par_clk_i : IN std_logic;
		rst_i : IN std_logic;
		spi_miso_i : IN std_logic;
		di_i : IN std_logic_vector(31 downto 0);
		wren_i : IN std_logic;          
		spi_ssel_o : OUT std_logic;
		spi_sck_o : OUT std_logic;
		spi_mosi_o : OUT std_logic;
		do_o : OUT std_logic_vector(31 downto 0);
		di_rdy_o : OUT std_logic;
		do_valid_o : OUT std_logic
--		state_dbg_o : OUT std_logic_vector(5 downto 0);
--		sh_reg_dbg_o : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;

	COMPONENT spi_slave
	PORT(
		clk_i : IN std_logic;
		rst_i : IN std_logic;
		spi_ssel_i : IN std_logic;
		spi_sck_i : IN std_logic;
		spi_mosi_i : IN std_logic;
		di_i : IN std_logic_vector(31 downto 0);
		wren_i : IN std_logic;          
		spi_miso_o : OUT std_logic;
		do_o : OUT std_logic_vector(31 downto 0);
		di_rdy_o : OUT std_logic;
		do_valid_o : OUT std_logic
--		state_dbg_o : OUT std_logic_vector(5 downto 0);
--		sh_reg_dbg_o : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;

begin

	Inst_spi_master: spi_master PORT MAP(
		spi_2x_clk_i => m_spi_clk_i,
		par_clk_i => m_par_clk_i,
		rst_i => m_rst_i,
		spi_ssel_o => m_spi_ssel_o,
		spi_sck_o => m_spi_sck_o,
		spi_mosi_o => m_spi_mosi_o,
		spi_miso_i => m_spi_miso_i,
		di_i => m_di_i,
		do_o => m_do_o,
		di_rdy_o => m_di_rdy_o,
		wren_i => m_wren_i,
		do_valid_o => m_do_valid_o
--		state_dbg_o => m_state_dbg_o,
--		sh_reg_dbg_o => m_sh_reg_dbg_o
	);

	Inst_spi_slave: spi_slave PORT MAP(
		clk_i => s_clk_i,
		rst_i => s_rst_i,
		spi_ssel_i => s_spi_ssel_i,
		spi_sck_i => s_spi_sck_i,
		spi_mosi_i => s_spi_mosi_i,
		spi_miso_o => s_spi_miso_o,
		di_i => s_di_i,
		do_o => s_do_o,
		di_rdy_o => s_di_rdy_o,
		wren_i => s_wren_i,
		do_valid_o => s_do_valid_o
--		state_dbg_o => s_state_dbg_o,
--		sh_reg_dbg_o => s_sh_reg_dbg_o
	);

end Behavioral;



