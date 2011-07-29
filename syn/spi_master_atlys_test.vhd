-- TestBench Template 

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity testbench is
end testbench;

architecture behavior of testbench is 

    --=============================================================================================
    -- Constants
    --=============================================================================================
    -- clock period
    constant CLK_PERIOD : time := 10 ns;

    -- button definitions
    constant btRESET    : integer := 0;             -- these are constants to use as btn_i(x)
    constant btUP       : integer := 1;
    constant btLEFT     : integer := 2;
    constant btDOWN     : integer := 3;
    constant btRIGHT    : integer := 4;
    constant btCENTER   : integer := 5;

    --=============================================================================================
    -- COMPONENT DECLARATIONS
    --=============================================================================================
    component spi_master_atlys_top
    port(
        gclk_i : in std_logic;
        sw_i : in std_logic_vector(7 downto 0);
        btn_i : in std_logic_vector(5 downto 0);          
        spi_ssel_o : out std_logic;
        spi_sck_o : out std_logic;
        spi_mosi_o : out std_logic;
        spi_miso_o : out std_logic;
        led_o : out std_logic_vector(7 downto 0);
        dbg_o : out std_logic_vector(11 downto 0)
    );
    end component;

    --=============================================================================================
    -- Signals for state machine control
    --=============================================================================================

    --=============================================================================================
    -- Signals for internal operation
    --=============================================================================================
    --- clock signals ---
    signal sysclk           : std_logic := '0';                                 -- 100MHz clock
    --- switch debouncer signals ---
    signal sw_data          : std_logic_vector (7 downto 0) := (others => '0'); -- switch data
    --- pushbutton debouncer signals ---
    signal btn_data         : std_logic_vector (5 downto 0) := (others => '0'); -- pushbuttons
    --- spi port signals ---
    signal spi_ssel         : std_logic;
    signal spi_sck          : std_logic;
    signal spi_mosi         : std_logic;
    signal spi_miso         : std_logic;
    -- debug output signals
    signal leds             : std_logic_vector (7 downto 0) := (others => '0');
    signal dbg              : std_logic_vector (11 downto 0) := (others => '0');
    -- debug ports
    signal spi_do_s         : std_logic_vector (7 downto 0) := (others => '0');
    signal spi_state_s      : std_logic_vector (3 downto 0) := (others => '0');
begin

    --=============================================================================================
    -- COMPONENT INSTANTIATIONS FOR THE CORES UNDER TEST
    --=============================================================================================
    -- spi_master_atlys_top:
    --      receives the 100 MHz clock from the board clock oscillator
    --      receives the 8 slide switches and 5 pushbuttons as test stimuli
    --      connects to 4 spi signals
    --      connects to 8 board LEDs
    --      connects to 12 debug pins
	inst_spi_master_atlys_top: spi_master_atlys_top 
    port map(
        gclk_i => sysclk,
        spi_ssel_o => spi_ssel,
        spi_sck_o => spi_sck,
        spi_mosi_o => spi_mosi,
        spi_miso_o => spi_miso,
        sw_i => sw_data,
        btn_i => btn_data,
        led_o => leds,
        dbg_o => dbg
	);

    spi_do_s <= dbg(7 downto 0);
    spi_state_s <= dbg(11 downto 8);

    --=============================================================================================
    -- CLOCK GENERATION
    --=============================================================================================
    gclk_proc: process is
    begin
        loop
            sysclk <= not sysclk;
            wait for CLK_PERIOD / 2;
        end loop;
    end process gclk_proc;

    --=============================================================================================
    -- TEST BENCH STIMULI
    --=============================================================================================
    tb : process
    begin
        wait for 100 ns; -- wait until global set/reset completes

        sw_data <= X"5A";
        btn_data(btRIGHT) <= '1';
        
        wait; -- will wait forever
    end process tb;
    --  End Test Bench 
END;
