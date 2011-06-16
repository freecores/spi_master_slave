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
--      It is parameterizable via generics for the data width ('N'), SPI mode (CPHA and CPOL), and lookahead prefetch 
--      signaling ('PREFETCH').
--
--      PARALLEL WRITE INTERFACE
--      The parallel interface has an input port 'di_i' and an output port 'do_o'.
--      Parallel load is controlled using 3 signals: 'di_i', 'di_req_o' and 'wren_i'. 'di_req_o' is a look ahead data request line,
--      that is set 'PREFETCH' clock cycles in advance to synchronize a pipelined memory or fifo to present the 
--      next input data at 'di_i' in time to have continuous clock at the spi bus, to allow back-to-back continuous load.
--      For a pipelined sync RAM, a PREFETCH of 2 cycles allows an address generator to present the new adress to the RAM in one
--      cycle, and the RAM to respond in one more cycle, in time for 'di_i' to be latched by the shifter.
--      If the user sequencer needs a different value for PREFETCH, the generic can be altered at instantiation time.
--      The 'wren_i' write enable strobe must be valid at least one setup time before the rising edge of the last SPI clock cycle,
--      if continuous transmission is intended. If 'wren_i' is not valid 2 SPI clock cycles after the last transmitted bit, the interface
--      enters idle state and deasserts SSEL.
--      When the interface is idle, 'wren_i' write strobe loads the data and starts transmission. 'di_req_o' will strobe when entering 
--      idle state, if a previously loaded data has already been transferred.
--
--      PARALLEL WRITE SEQUENCE
--      =======================
--                         __    __    __    __    __    __    __ 
--      clk_i           __/  \__/  \__/  \__/  \__/  \__/  \__/  \...     -- parallel interface clock
--                               ___________                        
--      di_req_o        ________/           \_____________________...     -- 'di_req_o' asserted on rising edge of 'clk_i'
--                      ______________ ___________________________...
--      di_i            __old_data____X______new_data_____________...     -- user circuit loads data on 'di_i' at next 'clk_i' rising edge
--                                                 _______                        
--      wren_i          __________________________/       \_______...     -- user strobes 'wren_i' for one cycle of 'clk_i'
--                      
--
--      PARALLEL READ INTERFACE
--      An internal buffer is used to copy the internal shift register data to drive the 'do_o' port. When a complete word is received,
--      the core shift register is transferred to the buffer, at the rising edge of the spi clock, 'spi_2x_clk_i'.
--      The signal 'do_valid_o' is set one 'spi_2x_clk_i' clock after, to directly drive a synchronous memory or fifo write enable.
--      'do_valid_o' is synchronous to the parallel interface clock, and changes only on rising edges of 'clk_i'.
--      When the interface is idle, data at the 'do_o' port holds the last word received.
--
--      PARALLEL READ SEQUENCE
--      ======================
--                      ______        ______        ______        ______   
--      spi_2x_clk_i     bit1 \______/ bitN \______/bitN-1\______/bitN-2\__...  -- spi 2x base clock
--                      _    __    __    __    __    __    __    __    __  
--      clk_i            \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \_...  -- parallel interface clock
--                      _____________ _____________________________________...  -- 1) rx data is transferred to 'do_buffer_reg'
--      do_o            ___old_data__X__________new_data___________________...  --    after last rx bit, at rising 'spi_2x_clk_i'.
--                                                   ____________               
--      do_valid_o      ____________________________/            \_________...  -- 2) 'do_valid_o' strobed for 2 'clk_i' cycles
--                                                                              --    on the 3rd 'clk_i' rising edge.
--
--
--      The propagation delay of spi_sck_o and spi_mosi_o, referred to the internal clock, is balanced by similar path delays,
--      but the sampling delay of spi_miso_i imposes a setup time referred to the sck signal that limits the high frequency
--      of the interface, for full duplex operation.
--
--      This design was originally targeted to a Spartan-6 platform, synthesized with XST and normal constraints.
--      The VHDL dialect used is VHDL'93, accepted largely by all synthesis tools.
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
-- 2011/06/09   v0.97.0068  [JD]    reduced control sets (resets, CE, presets) to the absolute minimum to operate, to reduce
--                                  synthesis LUT overhead in Spartan-6 architecture.
-- 2011/06/11   v0.97.0075  [JD]    redesigned all parallel data interfacing ports, and implemented cross-clock strobe logic.
-- 2011/06/12   v0.97.0079  [JD]    streamlined wren_ack for all cases and eliminated unnecessary register resets.
-- 2011/06/14   v0.97.0083  [JD]    (bug CPHA effect) : redesigned SCK output circuit.
--                                  (minor bug) : removed fsm registers from (not rst_i) chip enable.
-- 2011/06/15   v0.97.0086  [JD]    removed master MISO input register, to relax MISO data setup time (to get higher speed).
--
--
-----------------------------------------------------------------------------------------------------------------------
--  TODO
--  ====
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
        PREFETCH : positive := 2);                                      -- prefetch lookahead cycles
    Port (  
        spi_2x_clk_i : in std_logic := 'X';                             -- spi base reference clock: 2x 'spi_sck_o'
        clk_i : in std_logic := 'X';                                    -- parallel interface clock
        rst_i : in std_logic := 'X';                                    -- reset core
        spi_ssel_o : out std_logic;                                     -- spi bus slave select line
        spi_sck_o : out std_logic;                                      -- spi bus sck
        spi_mosi_o : out std_logic;                                     -- spi bus mosi output
        spi_miso_i : in std_logic := 'X';                               -- spi bus spi_miso_i input
        di_req_o : out std_logic;                                       -- preload lookahead data request line
        di_i : in  std_logic_vector (N-1 downto 0) := (others => 'X');  -- parallel data in (clocked on rising spi_2x_clk_i after last bit)
        wren_i : in std_logic := 'X';                                   -- user data write enable, starts transmission when interface is idle
        do_valid_o : out std_logic;                                     -- do_o data valid signal, valid during one spi_2x_clk_i rising edge.
        do_o : out  std_logic_vector (N-1 downto 0);                    -- parallel output (clocked on rising spi_2x_clk_i after last bit)
        --- debug ports: can be removed for the application circuit ---
        do_transfer_o : out std_logic;                                  -- debug: internal transfer driver
        wren_o : out std_logic;                                         -- debug: internal state of the wren_i pulse stretcher
        wren_ack_o : out std_logic;                                     -- debug: wren ack from state machine
        rx_bit_reg_o : out std_logic;                                   -- debug: internal rx bit
        state_dbg_o : out std_logic_vector (5 downto 0);                -- debug: internal state register
        core_clk_o : out std_logic;
        core_n_clk_o : out std_logic;
        sh_reg_dbg_o : out std_logic_vector (N-1 downto 0)              -- debug: internal shift register
    );                      
end spi_master;

--================================================================================================================
-- this architecture is a pipelined register-transfer description.
-- all signals are clocked at the rising edge of the system clock 'spi_2x_clk_i'.
--================================================================================================================
architecture RTL of spi_master is
    -- core clocks, generated from 'spi_2x_clk_i': initialized to differential values
    signal core_clk : std_logic := '0';     -- continuous fsm core clock, positive logic
    signal core_n_clk : std_logic := '1';   -- continuous fsm core clock, negative logic
    -- spi bus clock, generated from the CPOL selected core clock polarity
    signal spi_clk : std_logic;             -- spi bus output clock
    -- core fsm clock
    signal fsm_clk : std_logic;             -- data change clock: fsm registers clocked at rising edge
    signal samp_clk : std_logic;            -- data sampling clock: input serial data clocked at rising edge
    --
    -- GLOBAL RESET: 
    --      all signals are initialized to zero at GSR (global set/reset) by giving explicit
    --      initialization values at declaration. This is needed for all Xilinx FPGAs, and 
    --      especially for the Spartan-6 and newer CLB architectures, where a local reset can
    --      reduce the usability of the slice registers, due to the need to share the control 
    --      set (RESET/PRESET, CLOCK ENABLE and CLOCK) by all 8 registers in a slice.
    --      By using GSR for the initialization, and reducing RESET local init to the bare
    --      essential, the model achieves better LUT/FF packing and CLB usability.
    --
    -- internal state signals for register and combinational stages
    signal state_next : natural range N+1 downto 0 := 0;
    signal state_reg : natural range N+1 downto 0 := 0;
    -- shifter signals for register and combinational stages
    signal sh_next : std_logic_vector (N-1 downto 0) := (others => '0');
    signal sh_reg : std_logic_vector (N-1 downto 0) := (others => '0');
    -- input bit sampled buffer
    signal rx_bit_reg : std_logic := '0';
    -- buffered di_i data signals for register and combinational stages
    signal di_reg : std_logic_vector (N-1 downto 0) := (others => '0');
    -- internal wren_i stretcher for fsm combinational stage
    signal wren : std_logic := '0';
    signal wren_ack_next : std_logic := '0';
    signal wren_ack_reg : std_logic := '0';
    -- internal SSEL enable control signals
    signal ena_ssel_next : std_logic := '0';
    signal ena_ssel_reg : std_logic := '0';
    -- internal SCK enable control signals
    signal ena_sck_next : std_logic := '0';
    signal ena_sck_reg : std_logic := '0';
    -- buffered do_o data signals for register and combinational stages
    signal do_buffer_next : std_logic_vector (N-1 downto 0) := (others => '0');
    signal do_buffer_reg : std_logic_vector (N-1 downto 0) := (others => '0');
    -- internal signal to flag transfer to do_buffer_reg
    signal do_transfer_next : std_logic := '0';
    signal do_transfer_reg : std_logic := '0';
    -- internal input data request signal 
    signal di_req_next : std_logic := '0';
    signal di_req_reg : std_logic := '0';
    -- cross-clock do_valid_o pipeline
    signal do_valid_next : std_logic := '0';
    signal do_valid_A : std_logic := '0';
    signal do_valid_B : std_logic := '0';
    signal do_valid_C : std_logic := '0';
    signal do_valid_D : std_logic := '0';
    signal do_valid_o_reg : std_logic := '0';
    -- cross-clock di_req_o pipeline
    signal di_req_o_next : std_logic := '1';
    signal di_req_o_A : std_logic := '0';
    signal di_req_o_B : std_logic := '0';
    signal di_req_o_C : std_logic := '0';
    signal di_req_o_D : std_logic := '0';
    signal di_req_o_reg : std_logic := '1';
begin
    --=============================================================================================
    --  GENERICS CONSTRAINTS CHECKING
    --=============================================================================================
    -- minimum word width is 8 bits
    assert N >= 8
    report "Generic parameter 'N' error: SPI shift register size needs to be 8 bits minimum"
    severity FAILURE;    
    -- minimum prefetch lookahead check
    assert PREFETCH >= 2
    report "Generic parameter 'PREFETCH' error: needs to be 1 minimum"
    severity FAILURE;    
    -- maximum prefetch lookahead check
    assert PREFETCH <= N-5
    report "Generic parameter 'PREFETCH' error: lookahead count out of range, needs to be N-5 maximum"
    severity FAILURE;    

    --=============================================================================================
    --  CLOCK GENERATION
    --=============================================================================================
    -- The clock generation block derive 2 continuous antiphase signals from the 2x spi base clock 
    -- for the core clocking.
    -- The 2 clock phases are generated by sepparate and synchronous FFDs, and should have only 
    -- interconnect delays.
    -- The clock phase is selected for serial input sampling, fsm clocking, and spi SCK output, based
    -- on the configuration of CPOL and CPHA.
    -- Each phase is selected so that all the registers can be clocked with a rising edge on all SPI
    -- modes.
    -----------------------------------------------------------------------------------------------
    -- divide down 'spi_2x_clk_i' by 2
    -- this should be synthesized as two synchronous FFDs
    core_clock_gen_proc : process (spi_2x_clk_i) is
    begin
        if spi_2x_clk_i'event and spi_2x_clk_i = '1' then
            core_clk <= core_n_clk;         -- divided by 2 clock, differential
            core_n_clk <= not core_n_clk;
        end if;
    end process core_clock_gen_proc;
    -----------------------------------------------------------------------------------------------
    -- spi clk generator: generate spi_clk from core_clk depending on CPOL
    spi_sck_cpol_0_proc :  
        if CPOL = '0' generate
        begin
            spi_clk <= core_clk;            -- for CPOL=0, spi clk has idle LOW
        end generate;
    spi_sck_cpol_1_proc :  
        if CPOL = '1' generate
        begin
            spi_clk <= core_n_clk;          -- for CPOL=1, spi clk has idle HIGH
        end generate;
    -----------------------------------------------------------------------------------------------
    -- Sampling clock generation: generate 'samp_clk' from core_clk or core_n_clk depending on CPHA
    --                            always sample data at the half-cycle of the fsm update cell
    smp_cpha_0_proc :  
        if CPHA = '0' generate
        begin
            samp_clk <= core_clk;
        end generate;
    smp_cpha_1_proc :  
        if CPHA = '1' generate
        begin
            samp_clk <= core_n_clk;
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
    --  REGISTERED INPUTS
    --=============================================================================================
    -- rx bit flop: capture rx bit after SAMPLE edge of sck
    --
    --  ATTENTION:  REMOVING THE FLIPFLOP (DIRECT CONNECTION) WE GET HIGHER PERFORMANCE DUE TO 
    --              REDUCED DEMAND ON MISO SETUP TIME. 
    --
    rx_bit_proc : process (samp_clk, spi_miso_i) is
    begin
--        if samp_clk'event and samp_clk = '1' then           -- uncomment to have the input register
            rx_bit_reg <= spi_miso_i;
--        end if;                                             -- uncomment to have the input register
    end process rx_bit_proc;

    --=============================================================================================
    --  RTL REGISTER PROCESSES
    --=============================================================================================
    -- fsm state and data registers: synchronous to the spi base reference clock
    core_reg_proc : process (fsm_clk) is
    begin
        -- FFD registers clocked on rising edge and cleared on sync rst_i
        if fsm_clk'event and fsm_clk = '1' then
            if rst_i = '1' then                             -- sync reset
                state_reg <= 0;                             -- only provide local reset for the state machine
            else
                state_reg <= state_next;                    -- state register
            end if;
        end if;
        -- FFD registers clocked on rising edge
        if fsm_clk'event and fsm_clk = '1' then
            sh_reg <= sh_next;                              -- shift register
            ena_ssel_reg <= ena_ssel_next;                  -- spi select enable
            ena_sck_reg <= ena_sck_next;                    -- spi clock enable
            do_buffer_reg <= do_buffer_next;                -- registered output data buffer 
            do_transfer_reg <= do_transfer_next;            -- output data transferred to buffer
            di_req_reg <= di_req_next;                      -- input data request
            wren_ack_reg <= wren_ack_next;                  -- wren ack for data load synchronization
        end if;
    end process core_reg_proc;

    --=============================================================================================
    --  CROSS-CLOCK PIPELINE TRANSFER LOGIC
    --=============================================================================================
    -- do_valid_o and di_req_o strobe output logic
    -- this is a delayed pulse generator with a ripple-transfer FFD pipeline, that generates a 
    -- fixed-length delayed pulse for the output flags, at the parallel clock domain
    out_transfer_proc : process ( clk_i, do_transfer_reg, di_req_reg, 
                                  do_valid_A, do_valid_B, do_valid_D, 
                                  di_req_o_A, di_req_o_B, di_req_o_D) is
    begin
        if clk_i'event and clk_i = '1' then                     -- clock at parallel port clock
            -- do_transfer_reg -> do_valid_o_reg
            do_valid_A <= do_transfer_reg;                      -- the input signal must be at least 2 clocks long
            do_valid_B <= do_valid_A;                           -- feed it to a ripple chain of FFDs
            do_valid_C <= do_valid_B;
            do_valid_D <= do_valid_C;
            do_valid_o_reg <= do_valid_next;                    -- registered output pulse
            --------------------------------
            -- di_req_reg -> di_req_o_reg
            di_req_o_A <= di_req_reg;                           -- the input signal must be at least 2 clocks long
            di_req_o_B <= di_req_o_A;                           -- feed it to a ripple chain of FFDs
            di_req_o_C <= di_req_o_B;                               
            di_req_o_D <= di_req_o_C;                               
            di_req_o_reg <= di_req_o_next;                      -- registered output pulse
        end if;
        -- generate a 2-clocks pulse at the 3rd clock cycle
        do_valid_next <= do_valid_A and do_valid_B and not do_valid_D;
        di_req_o_next <= di_req_o_A and di_req_o_B and not di_req_o_D;
    end process out_transfer_proc;
    -- parallel load input registers: data register and write enable
    in_transfer_proc: process (clk_i, wren_i, wren_ack_reg) is
    begin
        -- registered data input, input register with clock enable
        if clk_i'event and clk_i = '1' then
            if wren_i = '1' then
                di_reg <= di_i;                                 -- parallel data input buffer register
            end if;
        end  if;
        -- stretch wren pulse to be detected by spi fsm (ffd with sync preset and sync reset)
        if clk_i'event and clk_i = '1' then
            if wren_i = '1' then                                -- wren_i is the sync preset for wren
                wren <= '1';
            elsif wren_ack_reg = '1' then                       -- wren_ack is the sync reset for wren
                wren <= '0';
            end if;
        end  if;
    end process in_transfer_proc;

    --=============================================================================================
    --  RTL COMBINATIONAL LOGIC PROCESSES
    --=============================================================================================
    -- state and datapath combinational logic
    core_combi_proc : process ( sh_reg, state_reg, rx_bit_reg, ena_ssel_reg, ena_sck_reg, do_buffer_reg, 
                                do_transfer_reg, di_reg, wren) is
    begin
        sh_next <= sh_reg;                                              -- all output signals are assigned to (avoid latches)
        ena_ssel_next <= ena_ssel_reg;                                  -- controls the slave select line
        ena_sck_next <= ena_sck_reg;                                    -- controls the clock enable of spi sck line
        do_buffer_next <= do_buffer_reg;                                -- output data buffer
        do_transfer_next <= do_transfer_reg;                            -- output data flag
        wren_ack_next <= '0';                                           -- remove data load ack for all but the load stages
        di_req_next <= '0';                                             -- prefetch data request: deassert when shifting data
        spi_mosi_o <= sh_reg(N-1);                                      -- shift out tx bit from the MSb
        state_next <= state_reg - 1;                                    -- update next state at each sck pulse
        case state_reg is
            when (N+1) =>                                               -- this state is to enable SSEL before SCK
                ena_ssel_next <= '1';                                   -- tx in progress: will assert SSEL
                ena_sck_next <= '1';                                    -- enable SCK on next cycle (stays off on first SSEL clock cycle)
            when (N) =>                                                 -- deassert 'di_rdy'
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);          -- shift inner bits
                sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
            when (N-1) downto (PREFETCH+3) =>                           -- if rx data is valid, raise 'do_valid'. remove 'do_transfer'
                do_transfer_next <= '0';                                -- reset transfer signal
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);          -- shift inner bits
                sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
            when (PREFETCH+2) downto 2 =>                               -- raise prefetch 'di_req_o_next' signal and remove 'do_valid'
                di_req_next <= '1';                                     -- request data in advance to allow for pipeline delays
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);          -- shift inner bits
                sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
            when 1 =>                                                   -- transfer rx data to do_buffer and restart if wren
                di_req_next <= '1';                                     -- request data in advance to allow for pipeline delays
                do_buffer_next(N-1 downto 1) <= sh_reg(N-2 downto 0);   -- shift rx data directly into rx buffer
                do_buffer_next(0) <= rx_bit_reg;                        -- shift last rx bit into rx buffer
                do_transfer_next <= '1';                                -- signal transfer to do_buffer
                if wren = '1' then                                      -- load tx register if valid data present at di_i
                    state_next <= N;                                  	-- next state is top bit of new data
                    sh_next <= di_reg;                                  -- load parallel data from di_reg into shifter
                    ena_sck_next <= '1';                                -- SCK enabled
                    wren_ack_next <= '1';                               -- acknowledge data in transfer
                else
                    ena_sck_next <= '0';                                -- SCK disabled: tx empty, no data to send
                end if;
            when 0 =>
                di_req_next <= '1';                                     -- will request data if shifter empty
                ena_sck_next <= '0';                                    -- SCK disabled: tx empty, no data to send
                if wren = '1' then                                      -- load tx register if valid data present at di_i
                    ena_ssel_next <= '1';                               -- enable interface SSEL
                    state_next <= N+1;                                  -- start from idle: let one cycle for SSEL settling
                    spi_mosi_o <= di_reg(N-1);                          -- special case: shift out first tx bit from the MSb (look ahead)
                    sh_next <= di_reg;                                  -- load bits from di_reg into shifter
                    wren_ack_next <= '1';                               -- acknowledge data in transfer
                else
                    ena_ssel_next <= '0';                               -- deassert SSEL: interface is idle
                    state_next <= 0;                                    -- when idle, keep this state
                end if;
            when others =>
                state_next <= 0;                                        -- state 0 is safe state
        end case; 
    end process core_combi_proc;

    --=============================================================================================
    --  OUTPUT LOGIC PROCESSES
    --=============================================================================================
    -- data output processes
    spi_ssel_o_proc:    spi_ssel_o <= not ena_ssel_reg;                 -- drive active-low slave select line 
    do_o_proc :         do_o <= do_buffer_reg;                          -- do_o always available
    do_valid_o_proc:    do_valid_o <= do_valid_o_reg;                   -- copy registered do_valid_o to output
    di_req_o_proc:      di_req_o <= di_req_o_reg;                       -- copy registered di_req_o to output
    -----------------------------------------------------------------------------------------------
    -- SCK out logic: output mux for the SPI sck
    --------------------------------------------
    -- This is modelled as a mux instead of a register because it requires a FDCPE (ffd with preset and clear),
    -- which generates very inneficient logic in Spartan-6. Instead, we have a mux that translates to a AND gate,
    -- and can be optimized to a fast CLB gate.
    spi_sck_gen_proc : process (ena_sck_reg, spi_clk) is                
    begin
        if ena_sck_reg = '1' then
            spi_sck_o <= spi_clk;                                       -- copy the selected clock polarity
        else
            spi_sck_o <= CPOL;                                          -- when clock disabled, set to idle polarity
        end if;
    end process spi_sck_gen_proc;

    --=============================================================================================
    --  DEBUG LOGIC PROCESSES
    --=============================================================================================
    -- these signals are useful for verification, and can be deleted or commented-out after debug.
    do_transfer_proc:   do_transfer_o <= do_transfer_reg;
    state_dbg_proc:     state_dbg_o <= std_logic_vector(to_unsigned(state_reg, 6)); -- export internal state to debug
    rx_bit_reg_proc:    rx_bit_reg_o <= rx_bit_reg;
    wren_o_proc:        wren_o <= wren;
    wren_ack_o_proc:    wren_ack_o <= wren_ack_reg;
    sh_reg_dbg_proc:    sh_reg_dbg_o <= sh_reg;                                     -- export sh_reg to debug
    core_clk_o_proc:    core_clk_o <= core_clk;
    core_n_clk_o_proc:  core_n_clk_o <= core_n_clk;

end architecture RTL;

