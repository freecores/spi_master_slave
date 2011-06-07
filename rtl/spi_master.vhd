-----------------------------------------------------------------------------------------------------------------------
-- Author:          Jonny Doin, jdoin@opencores.org
-- 
-- Create Date:     12:18:12 04/25/2011 
-- Module Name:     SPI_MASTER - RTL
-- Project Name:    SPI MASTER / SLAVE INTERFACE
-- Target Devices:  Spartan-6
-- Tool versions:   ISE 13.1
-- Description: 
--
--      This block is the SPI master interface, implemented in one single entity.
--      All internal core operations are synchronous to a spi base clock, that generates the spi sck clock directly.
--      All parallel i/o interface operations are synchronous to a system clock, that can be asynchronous to the spi base clock.
--      Fully pipelined circuitry guarantees that no setup artifacts occur on the buffers that are accessed by the two clock domains.
--      The block is very simple to use, and has parallel inputs and outputs that behave like a synchronous memory i/o.
--      It is parameterizable for the data width ('N'), SPI mode via generics (CPHA and CPOL), and lookahead prefetch 
--      signaling ('PREFETCH').
--
--      PARALLEL WRITE INTERFACE
--      The parallel interface has a input port 'di_i' and an output port 'do_o'.
--      Parallel load is controlled using 3 signals: 'di_i', 'di_rdy_o' and 'wren_i'. 'di_rdy_o' is a look ahead data request line,
--      that is set 'PREFETCH' clock cycles in advance to synchronize a pipelined memory or fifo to present the 
--      next input data at 'di_i' in time to have continuous clock at the spi bus, to allow back-to-back continuous load.
--      For a pipelined sync RAM, a PREFETCH of 2 cycles allows an address generator to present the new adress to the RAM in one
--      cycle, and the RAM to respond in one more cycle, in time for 'di_i' to be latched by the shifter.
--      If the user sequencer needs a different value for PREFETCH, the generic can be altered at instantiation time.
--      The 'wren_i' write enable strobe must be valid at least one setup time before the rising edge of the last clock cycle,
--      if continuous transmission is intended. If 'wren_i' is not valid 2 clock cycles after the last tranmitted bit, the interface
--      enters idle state and deasserts SSEL.
--      When the interface is idle, 'wren_i' write strobe loads the data and starts transmission. 'di_rdy_o' is always asserted when idle.
--      The interaction for data load is:
--
--      PARALLEL WRITE PIPELINED SEQUENCE
--      =================================
--                         __    __    __    __    __    __    __    __    __   
--      par_clk_i       __/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__...     -- parallel interface clock
--                               ___________________________________
--      di_rdy_o        ________/                                   \___________...     -- 'di_rdy_o' asserted on rising edge of 'par_clk_i'
--                      ______________ _________________________________________
--      di_i            __old_data____X__________new_data_______________________...     -- user circuit loads data on 'di_i' at next rising edge
--                                         ________________________________             -- user circuit asserts 'wren_i' at next edge, 
--      wren_i          __________________/                                \____...     -- and removes 'wren_i' after 'di_rdy_o' is removed
--                      
--
--      PARALLEL READ INTERFACE
--      An internal buffer is used to copy the internal shift register data to drive the 'do_o' port. When a complete word is received,
--      the core shift register is transferred to the buffer, at the rising edge of the spi clock, 'spi_2x_clk_i'.
--      The signal 'do_valid_o' is set one 'spi_2x_clk_i' clock after, to directly drive a synchronous memory or fifo write enable.
--      'do_valid_o' is synchronous to the parallel interface clock, and changes only on rising edges of 'par_clk_i'.
--      When the interface is idle, data at the 'do_o' port holds the last word received.
--
--      PARALLEL READ PIPELINED SEQUENCE
--      ================================
--                      ______        ______        ______        ______        ______        
--      spi_2x_clk_i     bit1 \______/ bitN \______/bitN-1\______/bitN-2\______/bitN-3\_... -- spi 2x base clock
--                      _    __    __    __    __    __    __    __    __    __    __           
--      par_clk_i        \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__... -- parallel interface clock
--                      _____________ __________________________________________________... -- 1) rx data is transferred to 'do_buffer_reg'
--      do_o            ___old_data__X__________new_data________________________________... --    after last rx bit, at rising 'spi_2x_clk_i'.
--                                                         ___________                      -- 2) 'do_valid_o' asserted on rising 'par_clk_i',
--      do_valid_o      __________________________________/           \_________________... --    at next bit (bit N-1) of the SPI transfer.
--                      
--
--      The propagation delay of spi_sck_o and spi_mosi_o, referred to the internal clock, is balanced by similar path delays,
--      but the sampling delay of spi_miso_i imposes a setup time referred to the sck signal that limits the high frequency
--      of the interface, for full duplex operation.
--
--      This design was originally targeted to a Spartan-6 platform, synthesized with XST and normal constraints.
--
------------------------------ COPYRIGHT NOTICE -----------------------------------------------------------------------
--                                                                   
--      This file is part of the SPI MASTER/SLAVE INTERFACE project http://opencores.org/project,spi_master_slave                
--                                                                   
--      Author(s):      Jonny Doin, jdoin@opencores.org
--                                                                   
--      Copyright (C) 2011 Authors and OPENCORES.ORG
--      --------------------------------------------
--                                                                   
--      This source file may be used and distributed without restriction provided that this copyright statement is not    
--      removed from the file and that any derivative work contains the original copyright notice and the associated 
--      disclaimer. 
--                                                                   
--      This source file is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser 
--      General Public License as published by the Free Software Foundation; either version 2.1 of the License, or 
--      (at your option) any later version.
--                                                                   
--      This source is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
--      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more  
--      details.
--
--      You should have received a copy of the GNU Lesser General Public License along with this source; if not, download 
--      it from http://www.opencores.org/lgpl.shtml
--                                                                   
------------------------------ REVISION HISTORY -----------------------------------------------------------------------
--
-- 2011/04/28   v0.01.0010  [JD]    shifter implemented as a sequential process. timing problems and async issues in synthesis.
-- 2011/05/01   v0.01.0030  [JD]    changed original shifter design to a fully pipelined RTL fsmd. solved all synthesis issues.
-- 2011/05/05   v0.01.0034  [JD]    added an internal buffer register for rx_data, to allow greater liberty in data load/store.
-- 2011/05/08   v0.10.0038  [JD]    increased one state to have SSEL start one cycle before SCK. Implemented full CPOL/CPHA 
--                                  logic, based on generics, and do_valid_o signal.
-- 2011/05/13   v0.20.0045  [JD]    streamlined signal names, added PREFETCH parameter, added assertions.
-- 2011/05/17   v0.80.0049  [JD]    added explicit clock synchronization circuitry across clock boundaries.
-- 2011/05/18   v0.95.0050  [JD]    clock generation circuitry, with generators for all-rising-edge clock core.
-- 2011/06/05   v0.96.0053  [JD]    changed async clear to sync resets.
-- 2011/06/07   v0.97.0065  [JD]    added cross-clock buffers, fixed fsm async glitches.
--
--                                                                   
-----------------------------------------------------------------------------------------------------------------------
--  TODO
--  ====
--
--      - DEBUG_PACKAGE:
--          - package to export signals to the verification testbench
--
-- 
-----------------------------------------------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity spi_master is
    Generic (   
        N : positive := 32;                                             -- 32bit serial word length is default
        CPOL : std_logic := '0';                                        -- SPI mode selection (mode 0 default)
        CPHA : std_logic := '0';                                        -- CPOL = clock polarity, CPHA = clock phase.
        PREFETCH : positive := 1);                                      -- prefetch lookahead cycles
    Port (  
        spi_2x_clk_i : in std_logic := 'X';                             -- spi base reference clock: 2x 'spi_sck_o'
        par_clk_i : in std_logic := 'X';                                -- parallel interface clock
        rst_i : in std_logic := 'X';                                    -- async reset: clear all registers
        spi_ssel_o : out std_logic;                                     -- spi bus slave select line
        spi_sck_o : out std_logic;                                      -- spi bus sck
        spi_mosi_o : out std_logic;                                     -- spi bus mosi output
        spi_miso_i : in std_logic := 'X';                               -- spi bus spi_miso_i input
        di_i : in  std_logic_vector (N-1 downto 0) := (others => 'X');  -- parallel data in (clocked on rising spi_2x_clk_i after last bit)
        do_o : out  std_logic_vector (N-1 downto 0);                    -- parallel output (clocked on rising spi_2x_clk_i after last bit)
        di_rdy_o : out std_logic;                                       -- preload lookahead: HIGH for PREFETCH cycles before last bit
        wren_i : in std_logic := 'X';                                   -- write enable (write di_i data at next rising spi_2x_clk_i edge) 
                                                                        -- wren_i starts transmission. must be valid 1 spi_2x_clk_i cycle 
                                                                        -- before current transmission ends.
        do_valid_o : out std_logic;                                     -- do_o data valid signal, valid during one spi_2x_clk_i rising edge.
        do_transfer_o : out std_logic;                                  -- debug: internal transfer driver
        state_dbg_o : out std_logic_vector (5 downto 0);                -- debug: internal state register
        rx_bit_reg_o : out std_logic;                                   -- debug: internal rx bit
        sh_reg_dbg_o : out std_logic_vector (N-1 downto 0)              -- debug: internal shift register
    );                      
end spi_master;

--================================================================================================================
-- this architecture is a pipelined register-transfer description.
-- all signals are clocked at the rising edge of the system clock 'spi_2x_clk_i'.
--================================================================================================================
architecture RTL of spi_master is

    -- core clocks, generated from 'spi_2x_clk_i'
    signal core_clk : std_logic;        -- continuous fsm core clock, positive logic
    signal core_n_clk : std_logic;      -- continuous fsm core clock, negative logic
    -- spi base clock, generated from 'spi_2x_clk_i'
    signal spi_clk : std_logic;         -- spi bus output clock, positive polarity
    signal spi_n_clk : std_logic;       -- spi bus output clock, negative polarity
    -- core fsm clock
    signal fsm_clk : std_logic;         -- data change clock: fsm registers clocked at rising edge
    signal samp_clk : std_logic;        -- data sampling clock: input serial data clocked at rising edge
    -- internal state signals for register and combinational stages
    signal state_reg : natural range N+1 downto 0 := 0;
    signal state_next : natural range N+1 downto 0 := 0;
    -- shifter signals for register and combinational stages
    signal sh_reg : std_logic_vector (N-1 downto 0) := (others => '0');
    signal sh_next : std_logic_vector (N-1 downto 0) := (others => '0');
    -- input bit sampled buffer
    signal rx_bit_reg : std_logic := '0';
    -- buffered di_i data signals for register and combinational stages
    signal di_reg : std_logic_vector (N-1 downto 0) := (others => '0');
    -- internal SSEL enable control signals
    signal ena_ssel_reg : std_logic := '0';
    signal ena_ssel_next : std_logic := '0';
    -- internal SCK enable control signals
    signal ena_sck_reg : std_logic := '0';
    signal ena_sck_next : std_logic := '0';
    -- buffered do_o data signals for register and combinational stages
    signal do_buffer_reg : std_logic_vector (N-1 downto 0) := (others => '0');
    signal do_buffer_next : std_logic_vector (N-1 downto 0) := (others => '0');
    -- internal signal to flag transfer to do_buffer_reg
    signal do_transfer_reg : std_logic := '0';
    signal do_transfer_next : std_logic := '0';
    -- internal registered do_valid_o
    signal do_valid_oreg : std_logic := '0';
    signal do_valid_reg : std_logic := '0';
    signal do_valid_next : std_logic := '0';
    -- internal registered di_rdy_o
    signal di_rdy_oreg : std_logic := '0';
    signal di_rdy_reg : std_logic := '1';
    signal di_rdy_next : std_logic := '1';
begin
    --=============================================================================================
    --  GENERICS CONSTRAINTS CHECKING
    --=============================================================================================
    -- minimum word width is 8 bits
    assert N >= 8
    report "Generic parameter 'N' error: SPI shift register size needs to be 8 bits minimum"
    severity FAILURE;    
    -- maximum prefetch lookahead check
    assert PREFETCH <= N-5
    report "Generic parameter 'PREFETCH' error: lookahead count out of range, needs to be N-5 maximum"
    severity FAILURE;    

    --=============================================================================================
    --  CLOCK GENERATION
    --=============================================================================================
    -- The clock generation block derive 2 sets of signals from the 2x spi base clock, with positive 
    -- and negative phase. The core clock runs continuously and drives the core fsm, and the spi clock
    -- drives the spi bus 'spi_sck_o' output directly, and is controlled by 'ena_sck_reg', driven by the
    -- fsm logic.
    -- The 2 clocks are generated each with one FFD, with a selected phase to drive the core with rising
    -- edge clocks only. The 2 sets of clocks have similar logic delays, which is important for the data
    -- setup time of the serial input related to the data setup time of the serial output.
    -----------------------------------------------------------------------------------------------
    -- divide down 'spi_2x_clk_i' by 2
    -- this should be synthesized as a single ffd with sync reset
    core_clock_gen_proc : process (rst_i, spi_2x_clk_i) is
    begin
        if spi_2x_clk_i'event and spi_2x_clk_i = '1' then
            if rst_i = '1' then
                core_clk <= '0';                -- positive logic clk: idle LOW
                core_n_clk <= '1';              -- negative logic clk: idle HIGH
            else
                core_clk <= core_n_clk;         -- divided by 2 clock, differential
                core_n_clk <= not core_n_clk;
            end if;
        end if;
    end process core_clock_gen_proc;
    -----------------------------------------------------------------------------------------------
    -- spi sck generator: divide input 2x clock by 2, with a CE controlled by the fsm
    -- this should be sinthesized as a single FFD with sync reset and clock enable
    spi_clock_gen_proc : process (rst_i, spi_2x_clk_i, ena_sck_reg) is
    begin
        if spi_2x_clk_i'event and spi_2x_clk_i = '1' then
            if rst_i = '1' then
                spi_clk <= '0';                 -- positive logic clk: idle LOW
                spi_n_clk <= '1';               -- negative logic clk: idle HIGH
            elsif ena_sck_reg = '1' then
                spi_clk <= spi_n_clk;           -- divided by 2 clock, differential
                spi_n_clk <= not spi_n_clk;
            end if;
        end if;
    end process spi_clock_gen_proc;
    -----------------------------------------------------------------------------------------------
    -- SCK out logic: generate sck from spi_clk or spi_n_clk depending on CPOL
    spi_sck_cpol_0_proc :  
        if CPOL = '0' generate
        begin
            spi_sck_o <= spi_clk;           -- for CPOL=0, spi clk has idle LOW
        end generate;
    spi_sck_cpol_1_proc :  
        if CPOL = '1' generate
        begin
            spi_sck_o <= spi_n_clk;         -- for CPOL=1, spi clk has idle HIGH
        end generate;
    -----------------------------------------------------------------------------------------------
    -- Sampling clock generation: generate 'samp_clk' from core_clk or core_n_clk depending on CPHA
    smp_cpha_0_proc :  
        if CPHA = '0' generate
        begin
            samp_clk <= spi_clk;            -- for CPHA=0, sample at end of sample cell
        end generate;
    smp_cpha_1_proc :  
        if CPHA = '1' generate
        begin
            samp_clk <= spi_n_clk;          -- for CPHA=1, sample at end of sample cell
        end generate;
    -----------------------------------------------------------------------------------------------
    -- FSM clock generation: generate 'fsm_clock' from core_clk or core_n_clk depending on CPHA
    fsm_cpha_0_proc :  
        if CPHA = '0' generate
        begin
            fsm_clk <= core_n_clk;          -- for CPHA=0, latch registers at rising edge of negative core clock
        end generate;
    fsm_cpha_1_proc :  
        if CPHA = '1' generate
        begin
            fsm_clk <= core_clk;            -- for CPHA=1, latch registers at rising edge of positive core clock
        end generate;

    --=============================================================================================
    --  RTL REGISTER PROCESSES
    --=============================================================================================
    -- rx bit flop: capture rx bit after SAMPLE edge of sck
    rx_bit_proc : process (samp_clk, spi_miso_i) is
    begin
        if samp_clk'event and samp_clk = '1' then
            rx_bit_reg <= spi_miso_i;
        end if;
    end process rx_bit_proc;
    -- state and data registers: synchronous to the spi base reference clock
    core_reg_proc : process (fsm_clk, rst_i) is
    begin
        if fsm_clk'event and fsm_clk = '1' then
            if rst_i = '1' then                             -- sync reset
                sh_reg <= (others => '0');
                state_reg <= 0;
                ena_ssel_reg <= '0';
                ena_sck_reg <= '0';
                do_buffer_reg <= (others => '0');
                do_transfer_reg <= '0';
            else
                sh_reg <= sh_next;
                state_reg <= state_next;
                ena_ssel_reg <= ena_ssel_next;
                ena_sck_reg <= ena_sck_next;
                do_buffer_reg <= do_buffer_next;
                do_transfer_reg <= do_transfer_next;
            end if;
        end if;
    end process core_reg_proc;
    -- cross-clock registers change on half-cycle of sck (ffd with async clear)
    -- this is to prevent fsm state change glitches causing setup time artifacts at async clk_i edges
    cross_reg_proc : process (rst_i, fsm_clk, ena_ssel_reg) is
    begin
        if ena_ssel_reg = '0' then
            di_rdy_reg <= '1';                              -- di_rdy true during idle    
        elsif fsm_clk'event and fsm_clk = '0' then          -- on half-cycle edge, update cross registers
            di_rdy_reg <= di_rdy_next;
        end if;
        if rst_i = '1' then
            do_valid_reg <= '0';                            -- async clear on do_valid
        elsif fsm_clk'event and fsm_clk = '0' then          -- on half-cycle edge, update cross registers
            do_valid_reg <= do_valid_next;
        end if;
    end process cross_reg_proc;
    -- parallel i/o interface registers: synchronous to the parallel interface clock
    par_reg_proc : process (rst_i, par_clk_i, ena_ssel_reg) is
    begin
        if par_clk_i'event and par_clk_i = '1' then
            if rst_i = '1' then                             -- sync reset
                di_rdy_oreg <= '0';
                do_valid_oreg <= '0';
                di_reg <= (others => '0');
            else
                di_rdy_oreg <= di_rdy_reg;                  -- di_rdy is synchronous to parallel interface clock
                do_valid_oreg <= (do_valid_reg and ena_ssel_reg) or (do_transfer_reg and not ena_ssel_reg);
                di_reg <= di_i;                             -- sample di_i at interface clock
            end if;
        end if;
    end process par_reg_proc;

    --=============================================================================================
    --  RTL COMBINATIONAL LOGIC PROCESSES
    --=============================================================================================
    -- state and datapath combinational logic
    core_combi_proc : process ( sh_reg, state_reg, rx_bit_reg, ena_ssel_reg, ena_sck_reg, do_buffer_reg, 
                                do_valid_reg, do_transfer_reg, di_reg, di_rdy_reg, wren_i ) is
    begin
        sh_next <= sh_reg;                                              -- all output signals are assigned to (avoid latches)
        ena_ssel_next <= ena_ssel_reg;
        ena_sck_next <= ena_sck_reg;
        do_buffer_next <= do_buffer_reg;
        do_valid_next <= do_valid_reg;
        do_transfer_next <= do_transfer_reg;
        di_rdy_next <= di_rdy_reg;
        spi_mosi_o <= '0';                                              -- will output '0' when shifter is empty
        state_next <= state_reg - 1;                                    -- next state is next bit
        case state_reg is
            when (N+1) =>                                           -- this state is to enable SSEL before SCK
                ena_ssel_next <= '1';                                   -- tx in progress: will assert SSEL
                ena_sck_next <= '1';                                    -- enable SCK on next cycle (stays off on first SSEL clock cycle)
                di_rdy_next <= '0';                                     -- deassert next-data request when shifting data
                spi_mosi_o <= sh_reg(N-1);                              -- shift out tx bit from the MSb
            when (N) =>                                             -- deassert 'di_rdy'
                di_rdy_next <= '0';                                     -- deassert next-data request when shifting data
                spi_mosi_o <= sh_reg(N-1);                              -- shift out tx bit from the MSb
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);          -- shift inner bits
                sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
            when (N-1) downto (PREFETCH+3) =>                       -- if rx data is valid, raise 'do_valid'. remove 'do_transfer'
                di_rdy_next <= '0';                                     -- deassert next-data request when start shifting
                do_valid_next <= do_transfer_reg;                       -- assert valid rx data, with plenty of pipeline delay for 'do_buffer'
                do_transfer_next <= '0';                                -- reset transfer signal
                spi_mosi_o <= sh_reg(N-1);                              -- shift out tx bit from the MSb
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);          -- shift inner bits
                sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
            when (PREFETCH+2) downto 2 =>                           -- raise prefetch 'di_rdy_next' signal and remove 'do_valid'
                di_rdy_next <= '1';                                     -- request data in advance to allow for pipeline delays
                do_valid_next <= '0';                                   -- make do_valid_o HIGH for one cycle only
                spi_mosi_o <= sh_reg(N-1);                              -- shift out tx bit from the MSb
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);          -- shift inner bits
                sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
            when 1 =>                                               -- transfer rx data to do_buffer and restart if wren
                do_buffer_next(N-1 downto 1) <= sh_reg(N-2 downto 0);   -- shift rx data directly into rx buffer
                do_buffer_next(0) <= rx_bit_reg;                        -- shift last rx bit into rx buffer
                do_transfer_next <= '1';                                -- signal transfer to do_buffer
                spi_mosi_o <= sh_reg(N-1);                              -- shift out last tx bit from the MSb
                if wren_i = '1' then                                    -- load tx register if valid data present at di_i
                    state_next <= N;                                  	-- next state is top bit of new data
                    sh_next <= di_reg;                                  -- load parallel data from di_reg into shifter
                    ena_sck_next <= '1';                                -- SCK enabled
                else
                    ena_sck_next <= '0';                                -- SCK disabled: tx empty, no data to send
                end if;
            when 0 =>
                ena_sck_next <= '0';                                    -- SCK disabled: tx empty, no data to send
                di_rdy_next <= '1';                                     -- will request data if shifter empty
                do_valid_next <= do_transfer_reg;                       -- assert valid rx data after data received, when interface idle
                if wren_i = '1' then                                    -- load tx register if valid data present at di_i
                    ena_ssel_next <= '1';                               -- enable interface SSEL
                    state_next <= N+1;                                  -- start from idle: let one cycle for SSEL settling
                    do_valid_next <= '0';                               -- start: clear rx data valid signal
                    spi_mosi_o <= di_reg(N-1);                          -- shift out first tx bit from the MSb
                    sh_next <= di_reg;                                  -- load bits from di_reg into shifter
                else
                    ena_ssel_next <= '0';                               -- deassert SSEL: interface is idle
                    state_next <= 0;                                    -- when idle, keep this state
                end if;
            when others =>
                null;
        end case; 
    end process core_combi_proc;

    --=============================================================================================
    --  OUTPUT LOGIC PROCESSES
    --=============================================================================================
    -- output signal connections
    spi_ssel_proc:      spi_ssel_o <= not ena_ssel_reg;                 -- drive active-low slave select line 
    do_proc :           do_o <= do_buffer_reg;                          -- do_o always available
    do_valid_proc:      do_valid_o <= do_valid_oreg;                    -- copy registered do_valid_o to output
    di_rdy_proc:        di_rdy_o <= di_rdy_oreg;                        -- copy registered di_rdy_o to output

    --=============================================================================================
    --  DEBUG LOGIC PROCESSES
    --=============================================================================================
    do_transfer_proc:   do_transfer_o <= do_transfer_reg;
    state_dbg_proc:     state_dbg_o <= std_logic_vector(to_unsigned(state_reg, 6)); -- export internal state to debug
    sh_reg_dbg_proc:    sh_reg_dbg_o <= sh_reg;                                     -- export sh_reg to debug
    rx_bit_reg_proc:    rx_bit_reg_o <= rx_bit_reg;

end architecture RTL;

