----------------------------------------------------------------------------------
-- Engineer: Jonny Doin
-- 
-- Create Date:     01:21:32 06/30/2011 
-- Design Name: 
-- Module Name:     spi_master_atlys_top
-- Project Name:    spi_master_slave
-- Target Devices:  Spartan-6 LX45
-- Tool versions:   ISE 13.1
-- Description: 
--          This is a verification project for the Digilent Atlys board, to test the SPI_MASTER, SPI_SLAVE and GRP_DEBOUNCE cores.
--          It uses the board's 100MHz clock input, and clocks all sequential logic at this clock.
--
--          See the "spi_master_atlys.ucf" file for pin assignments.
--          The test circuit uses the VHDCI connector on the Atlys to implement a 16-pin debug port to be used
--          with a Tektronix MSO2014. The 16 debug pins are brought to 2 8x2 headers that form a umbilical
--          digital pod port.
--          The board switches are used to set the SPI_MASTER transmit data, and the SPI_SLAVE receive data drives the switch LEDs.
--          The pushbuttons drive the slave transmit data, and the master received data drives the parallel debug port.
--
------------------------------ REVISION HISTORY -----------------------------------------------------------------------
--
-- 2011/07/02   v0.01.0010  [JD]    implemented a wire-through from switches to LEDs, just to test the toolchain. It worked!
-- 2011/07/03   v0.01.0020  [JD]    added clock input, and a simple LED blinker for each LED. 
-- 2011/07/03   v0.01.0030  [JD]    added clear input, and instantiated a SPI_MASTER from my OpenCores project. 
-- 2011/07/04   v0.01.0040  [JD]    changed all clocks to clock enables, and use the 100MHz board gclk_i to clock all registers.
--                                  this change made the design go up to 288MHz, after synthesis.
-- 2011/07/07   v0.03.0050  [JD]    implemented a 16pin umbilical port for the MSO2014 in the Atlys VmodBB board, and moved all
--                                  external monitoring pins to the VHDCI ports.
-- 2011/07/10   v1.10.0075  [JD]    verified spi_master_slave at 50MHz, 25MHz, 16.666MHz, 12.5MHz, 10MHz, 8.333MHz, 7.1428MHz, 
--                                  6.25MHz, 1MHz and 500kHz 
--
--
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity spi_master_atlys_top is
    Port (
        gclk_i : in std_logic := 'X';               -- board clock input 100MHz
        clear_i : in std_logic := '0';              -- btn used as clear signal
        --- SPI interface ---
        spi_ssel_o : out std_logic;                 -- spi port SSEL
        spi_sck_o : out std_logic;                  -- spi port SCK
        spi_mosi_o : out std_logic;                 -- spi port MOSI
        spi_miso_o : out std_logic;                 -- spi port MISO
        --- input slide switches ---
        sw_i : in std_logic_vector (7 downto 0);    -- 8 input slide switches
        --- input buttons ---
        btn_i : in std_logic_vector (5 downto 0);   -- 6 input push buttons
        --- output LEDs ----
        led_o : out std_logic_vector (7 downto 0);  -- output leds
        --- debug outputs ---
        dbg_o : out std_logic_vector (7 downto 0);  -- 10 generic debug pins
        --- spi debug pins ---
        spi_rx_bit_m_o : out std_logic;             -- master rx bit feedback
        spi_rx_bit_s_o : out std_logic;             -- slave rx bit feedback
        spi_do_valid_o : out std_logic;             -- spi data valid
        spi_di_req_o : out std_logic;               -- spi data request
        spi_wren_o : out std_logic;                 -- spi write enable
        spi_wren_ack_o : out std_logic              -- spi write enable ack
    );                      
end spi_master_atlys_top;

architecture behavioral of spi_master_atlys_top is

    --=============================================================================================
    -- Constants
    --=============================================================================================
    -- clock divider count values from gclk_i (100MHz board clock)
    -- these constants shall not be zero
    constant FSM_CE_DIV         : integer := 1;     -- fsm operates at 100MHz
    constant SPI_2X_CLK_DIV     : integer := 1;     -- 50MHz SPI clock
    constant SAMP_CE_DIV        : integer := 1;     -- board signals sampled at 100MHz
    -- spi port generics
    constant N : integer := 8;                      -- 8 bits
    
    -- button definitions
    constant btRESET    : integer := 0;             -- these are constants to use as btn_i(x)
    constant btUP       : integer := 1;
    constant btLEFT     : integer := 2;
    constant btDOWN     : integer := 3;
    constant btRIGHT    : integer := 4;
    constant btCENTER   : integer := 5;

    --=============================================================================================
    -- Type definitions
    --=============================================================================================
    type fsm_state_type is (st_reset, st_wait_spi_idle, st_wait_new_switch, 
                            st_send_spi_data, st_wait_spi_ack, st_wait_spi_finish ); 

    --=============================================================================================
    -- Signals for state machine control
    --=============================================================================================
    signal state_reg        : fsm_state_type := st_reset;
    signal state_next       : fsm_state_type := st_reset;

    --=============================================================================================
    -- Signals for internal operation
    --=============================================================================================
    --- clock enable signals ---
    signal samp_ce          : std_logic := '1';         -- clock enable for sample inputs
    signal fsm_ce           : std_logic := '1';         -- clock enable for fsm logic
    --- switch debouncer signals ---
    signal sw_data          : std_logic_vector (7 downto 0) := (others => '0'); -- debounced switch data
    signal sw_reg           : std_logic_vector (7 downto 0) := (others => '0'); -- registered switch data 
    signal sw_next          : std_logic_vector (7 downto 0) := (others => '0'); -- combinatorial switch data
    signal new_switch       : std_logic := '0';                                 -- detector for new switch data
    --- pushbutton debouncer signals ---
    signal btn_data         : std_logic_vector (5 downto 0) := (others => '0'); -- debounced state of pushbuttons
    signal btn_reg          : std_logic_vector (5 downto 0) := (others => '0'); -- registered button data 
    signal btn_next         : std_logic_vector (5 downto 0) := (others => '0'); -- combinatorial button data
    signal new_button       : std_logic := '0';                                 -- detector for new button data
    --- spi port signals ---
    -- spi bus wires
    signal spi_ssel         : std_logic;
    signal spi_sck          : std_logic;
    signal spi_mosi         : std_logic;
    signal spi_miso         : std_logic;
    -- spi master port control signals
    signal spi_rst_reg      : std_logic := '1';
    signal spi_rst_next     : std_logic := '1';
    signal spi_ssel_reg     : std_logic;
    signal spi_wren_reg_m   : std_logic := '0';
    signal spi_wren_next_m  : std_logic := '0';
    -- spi master port flow control flags
    signal spi_di_req_m     : std_logic;
    signal spi_do_valid_m   : std_logic;
    -- spi master port parallel data bus
    signal spi_di_reg_m     : std_logic_vector (N-1 downto 0) := (others => '0');
    signal spi_di_next_m    : std_logic_vector (N-1 downto 0) := (others => '0');
    signal spi_do_m         : std_logic_vector (N-1 downto 0);
    -- spi master port debug flags
    signal spi_rx_bit_m     : std_logic;
    signal spi_wr_ack_m     : std_logic;
    -- spi slave port control signals
    signal spi_wren_reg_s   : std_logic := '1';
    signal spi_wren_next_s  : std_logic := '0';
    -- spi slave port flow control flags
    signal spi_di_req_s     : std_logic;
    signal spi_do_valid_s   : std_logic;
    -- spi slave port parallel data bus
    signal spi_di_reg_s     : std_logic_vector (N-1 downto 0) := (7 => '1', 6 => '0', 5 => '1', others => '0');
    signal spi_di_next_s    : std_logic_vector (N-1 downto 0) := (others => '0');
    signal spi_do_s         : std_logic_vector (N-1 downto 0);
    -- spi slave port debug flags
    signal spi_rx_bit_s     : std_logic;
    signal spi_wr_ack_s     : std_logic;
    -- other signals
    signal clear            : std_logic := '0';
    -- debug output signals
    signal leds_reg         : std_logic_vector (7 downto 0) := (others => '0');
    signal dbg              : std_logic_vector (7 downto 0) := (others => '0');
begin

    --=============================================================================================
    -- COMPONENT INSTANTIATIONS FOR THE CORES UNDER TEST
    --=============================================================================================
    -- spi master port: 
    --      receives parallel data from the slide switches, transmits to slave port.
    --      receives serial data from slave port, sends to 8bit parallel debug port.
    Inst_spi_master_port: entity work.spi_master(rtl) 
        generic map (N => N, CPOL => '0', CPHA => '0', PREFETCH => 3, SPI_2X_CLK_DIV => SPI_2X_CLK_DIV)
        port map( 
            sclk_i => gclk_i,                   -- system clock is used for serial and parallel ports
            pclk_i => gclk_i,
            rst_i => spi_rst_reg,
            spi_ssel_o => spi_ssel,
            spi_sck_o => spi_sck,
            spi_mosi_o => spi_mosi,
            spi_miso_i => spi_miso,
            di_req_o => spi_di_req_m,
            di_i => spi_di_reg_m,
            do_valid_o => spi_do_valid_m,
            do_o => spi_do_m,
            rx_bit_reg_o => spi_rx_bit_m,
            wren_i => spi_wren_reg_m,
            wren_o => spi_wren_o,
            wren_ack_o => spi_wr_ack_m          -- monitor wren ack from inside spi port
        );

    dbg(7 downto 0) <= spi_do_m(7 downto 0);    -- connect master received data to 8bit debug port
    spi_rx_bit_m_o  <= spi_rx_bit_m;            -- connect rx_bit monitor for master port

    -- spi slave port
    --      receives parallel data from the pushbuttons, transmits to master port.
    --      receives serial data from master port, sends to the 8 LEDs.
    Inst_spi_slave_port: entity work.spi_slave(rtl) 
        generic map (N => N, CPOL => '0', CPHA => '0', PREFETCH => 3)
        port map( 
            clk_i => gclk_i,
            spi_ssel_i => spi_ssel,             -- generated by the spi master
            spi_sck_i => spi_sck,               -- generated by the spi master
            spi_mosi_i => spi_mosi,
            spi_miso_o => spi_miso,
            di_req_o => spi_di_req_s,
            di_i => spi_di_reg_s,
            wren_i => spi_wren_reg_s,
            rx_bit_reg_o => spi_rx_bit_s,
            do_valid_o => spi_do_valid_s,
            do_o => spi_do_s
        );                      

    spi_di_reg_s(7 downto 5) <= B"101";                 -- get the slave transmit data from pushbuttons
    spi_di_reg_s(4 downto 0) <= btn_data(5 downto 1);
    spi_wren_reg_s <= '1';                              -- fix wren to '1', for continuous load of transmit data
    spi_rx_bit_s_o <= spi_rx_bit_s;                     -- connect rx_bit monitor for slave port

    -- debounce for the input switches, with new data strobe output
    Inst_sw_debouncer: entity work.grp_debouncer(rtl)
        generic map (N => 8, CNT_VAL => 10000)  -- debounce 8 inputs with 100 us settling time
        port map(  
            clk_i => gclk_i,                    -- system clock
            data_i => sw_i,                     -- noisy input data
            data_o => sw_data                   -- registered stable output data
--            strb_o => dbg(0)                    -- monitor the debounced data strobe
        );                      

    -- debounce for the input pushbuttons, with new data strobe output
    Inst_btn_debouncer: entity work.grp_debouncer(rtl)
        generic map (N => 6, CNT_VAL => 50000)  -- debounce 6 inputs with 500 us settling time
        port map(  
            clk_i => gclk_i,                    -- system clock
            data_i => btn_i,                    -- noisy input data
            data_o => btn_data                  -- registered stable output data
--            strb_o => dbg(3)                    -- monitor the debounced data strobe
        );                      

    --=============================================================================================
    --  CONSTANTS CONSTRAINTS CHECKING
    --=============================================================================================
    -- clock dividers shall not be zero
    assert FSM_CE_DIV > 0
    report "Constant 'FSM_CE_DIV' should not be zero"
    severity FAILURE;
    -- minimum prefetch lookahead check
    assert SPI_2X_CLK_DIV > 0
    report "Constant 'SPI_2X_CLK_DIV' should not be zero"
    severity FAILURE;
    -- maximum prefetch lookahead check
    assert SAMP_CE_DIV > 0
    report "Constant 'SAMP_CE_DIV' should not be zero"
    severity FAILURE;

    --=============================================================================================
    --  CLOCK GENERATION
    --=============================================================================================
    -- The clock generation block derives 3 internal clocks, divided down from the 100MHz input clock 
    --      core clock, 
    --      spi 2x base clock,
    --      fsm clock,
    -----------------------------------------------------------------------------------------------
    -- generate the sampling clock enable from the 100MHz board input clock 
    samp_ce_gen_proc: process (gclk_i) is
        variable clk_cnt : integer range SAMP_CE_DIV-1 downto 0 := 0;
    begin
        if gclk_i'event and gclk_i = '1' then
            if clk_cnt = SAMP_CE_DIV-1 then
                samp_ce <= '1';
                clk_cnt := 0;
            else
                samp_ce <= '0';
                clk_cnt := clk_cnt + 1;
            end if;
        end if;
    end process samp_ce_gen_proc;
    -- generate the fsm clock enable from the 100MHz board input clock 
    fsm_ce_gen_proc: process (gclk_i) is
        variable clk_cnt : integer range FSM_CE_DIV-1 downto 0 := 0;
    begin
        if gclk_i'event and gclk_i = '1' then
            if clk_cnt = FSM_CE_DIV-1 then
                fsm_ce <= '1';
                clk_cnt := 0;
            else
                fsm_ce <= '0';
                clk_cnt := clk_cnt + 1;
            end if;
        end if;
    end process fsm_ce_gen_proc;

    --=============================================================================================
    -- INPUTS LOGIC
    --=============================================================================================
    -- registered inputs
    samp_inputs_proc: process (gclk_i) is
    begin
        if gclk_i'event and gclk_i = '1' then
            if samp_ce = '1' then
                clear <= btn_data(btUP);        -- clear is button UP
                leds_reg <= spi_do_s;           -- update LEDs with spi_slave received data
            end if;
        end if;
    end process samp_inputs_proc;

    --=============================================================================================
    --  REGISTER TRANSFER PROCESSES
    --=============================================================================================
    -- fsm state and data registers: synchronous to the spi base reference clock
    fsm_reg_proc : process (gclk_i) is
    begin
        -- FFD registers clocked on rising edge and cleared on sync 'clear'
        if gclk_i'event and gclk_i = '1' then
            if clear = '1' then                 -- sync reset
                state_reg <= st_reset;          -- only provide local reset for the state register
            else
                if fsm_ce = '1' then
                    state_reg <= state_next;    -- state register
                end if;
            end if;
        end if;
        -- FFD registers clocked on rising edge, with no reset
        if gclk_i'event and gclk_i = '1' then
            if fsm_ce = '1' then
                spi_wren_reg_m <= spi_wren_next_m;
                spi_di_reg_m <= spi_di_next_m;
--                spi_wren_reg_s <= spi_wren_next_s;
--                spi_di_reg_s <= spi_di_next_s;
                spi_rst_reg <= spi_rst_next;
                spi_ssel_reg <= spi_ssel;
                sw_reg <= sw_next;
                btn_reg <= btn_next;
            end if;
        end if;
    end process fsm_reg_proc;

    --=============================================================================================
    --  COMBINATORIAL NEXT-STATE LOGIC PROCESSES
    --=============================================================================================
    -- edge detector for new switch data
    new_switch_proc: new_switch <= '1' when sw_data /= sw_reg else '0';     -- '1' for difference
    -- edge detector for new button data
    new_button_proc: new_button <= '1' when btn_data /= btn_reg else '0';   -- '1' for difference
    -- fsm state and combinatorial logic
    -- the sequencer will wait for a new switch combination, and send the switch data to the spi port
    fsm_combi_proc: process (   state_reg, spi_wren_reg_m, spi_di_reg_m, spi_di_req_m, spi_wr_ack_m, -- spi_di_reg_s, 
                                spi_wren_reg_s, spi_ssel_reg, spi_rst_reg, sw_data, 
                                sw_reg, new_switch, btn_data, btn_reg, new_button) is
    begin
        spi_rst_next <= spi_rst_reg;
        spi_di_next_m <= spi_di_reg_m;
        spi_wren_next_m <= spi_wren_reg_m;
--        spi_di_next_s <= spi_di_reg_s;
--        spi_wren_next_s <= spi_wren_reg_s;
        sw_next <= sw_reg;
        btn_next <= btn_reg;
        state_next <= state_reg;
        case state_reg is
            when st_reset =>
                spi_rst_next <= '1';                        -- place spi interface on reset
                spi_di_next_m <= (others => '0');           -- clear spi data port
                spi_di_next_s <= (others => '0');           -- clear spi data port
                spi_wren_next_m <= '0';                     -- deassert write enable
                spi_wren_next_s <= '0';                     -- deassert write enable
                state_next <= st_wait_spi_idle;
                
            when st_wait_spi_idle =>
                if spi_ssel_reg = '1' then
                    spi_rst_next <= '0';                    -- remove reset when interface is idle
                    state_next <= st_wait_new_switch;
                end if;

            when st_wait_new_switch =>
                if new_switch = '1' then                    -- wait for new stable switch data
                    sw_next <= sw_data;                     -- load new switch data (end the mismatch condition)
                    state_next <= st_send_spi_data;
                elsif new_button = '1' then
                    btn_next <= btn_data;                   -- load new button data (end the mismatch condition)
                    if btn_data /= B"000001" then
                        state_next <= st_send_spi_data;
                    end if;
                end if;
            
            when st_send_spi_data =>
                spi_di_next_m <= sw_reg;                    -- load switch register to the spi port
                spi_wren_next_m <= '1';                     -- write data on next clock
                state_next <= st_wait_spi_ack;

            when st_wait_spi_ack =>                         -- the actual write happens on this state
                spi_di_next_m <= sw_reg;                    -- load switch register to the spi port
                if spi_wr_ack_m = '1' then                  -- wait acknowledge
                    spi_wren_next_m <= '0';                 -- remove write strobe on next clock
                    state_next <= st_wait_spi_finish;
                end if;
        
            when st_wait_spi_finish =>
                if spi_ssel_reg = '1' then
                    state_next <= st_wait_new_switch;
                end if;

            when others =>
                state_next <= st_reset;                     -- state st_reset is safe state
        end case; 
    end process fsm_combi_proc;

    --=============================================================================================
    --  OUTPUT LOGIC PROCESSES
    --=============================================================================================
    -- connect the spi output wires
    spi_ssel_o_proc:        spi_ssel_o      <= spi_ssel;
    spi_sck_o_proc:         spi_sck_o       <= spi_sck;
    spi_mosi_o_proc:        spi_mosi_o      <= spi_mosi;
    spi_miso_o_proc:        spi_miso_o      <= spi_miso;
    spi_do_valid_o_proc:    spi_do_valid_o  <= spi_do_valid_m;
    spi_di_req_o_proc:      spi_di_req_o    <= spi_di_req_m;
    spi_wren_ack_o_proc:    spi_wren_ack_o  <= spi_wr_ack_m;
    led_o_proc:             led_o           <= leds_reg;        -- connect leds_reg signal to LED outputs

    --=============================================================================================
    --  DEBUG LOGIC PROCESSES
    --=============================================================================================
    -- connect the debug vector outputs
    dbg_o_proc:             dbg_o <= dbg;

end behavioral;

