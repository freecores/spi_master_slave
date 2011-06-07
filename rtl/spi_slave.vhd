----------------------------------------------------------------------------------
-- Author:          Jonny Doin, jdoin@opencores.org
-- 
-- Create Date:     15:36:20 05/15/2011
-- Module Name:     SPI_SLAVE - RTL
-- Project Name:    SPI INTERFACE
-- Target Devices:  Spartan-6
-- Tool versions:   ISE 13.1
-- Description: 
--
--      This block is the SPI slave interface, implemented in one single entity.
--      All internal core operations are synchronous to the external SPI clock, and follows the general SPI de-facto standard.
--      The parallel read/write interface is synchronous to a supplied system master clock, 'clk_i'.
--      To avoid async glitches caused by setup violations between the core registers and the parallel i/o registers,
--      access to the parallel ports 'di_i' and 'do_o' must be synchronized with the 'di_rdi_o' and 'do_valid_o' signals.
--
--      The block is very simple to use, and has parallel inputs and outputs that behave like a synchronous memory i/o.
--      It is parameterizable for the data width ('N'), SPI mode via generics (CPHA and CPOL), and lookahead prefetch 
--      signaling ('PREFETCH').
--
--      PARALLEL WRITE INTERFACE
--      The parallel interface has a input port 'di_i' and an output port 'do_o'.
--      Parallel load is controlled using 3 signals: 'di_i', 'di_rdy_o' and 'wren_i'. 'di_rdy_o' is a look ahead data request line,
--      that is set 'PREFETCH' 'spi_sck_i' cycles in advance to synchronize a pipelined memory or fifo to present the 
--      next input data at 'di_i' in time to have continuous clock at the spi bus, to allow back-to-back continuous load.
--      The write to 'di_i' must occur at most one 'spi_sck_i' cycle before actual load to the core shift register, to avoid
--      race conditions at the register transfer.
--      For a pipelined sync RAM, a PREFETCH of 3 cycles allows an address generator to present the new adress to the RAM in one
--      cycle, and the RAM to respond in one more cycle, in time for 'di_i' to be latched by the interface one clock before transfer.
--      If the user sequencer needs a different value for PREFETCH, the generic can be altered at instantiation time.
--      The 'wren_i' write enable strobe must be valid at least one setup time before the rising edge of the last clock cycle,
--      if continuous transmission is intended. 
--      When the interface is idle ('spi_ssel_i' is HIGH), the top bit of the latched 'di_i' port is presented at port 'spi_miso_o'.
--
--      PARALLEL WRITE PIPELINED SEQUENCE
--      =================================
--                         __    __    __    __    __    __    __    __    __    __    __   
--      clk_i           __/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__...         -- parallel interface clock
--                               ___________________________________
--      di_rdy_o        ________/                                   \________________...                -- 'di_rdy_o' asserted on rising edge of 'clk_par_i'
--                      ______________ ______________________________________________...
--      di_i            __old_data____X__________new_data____________________________...                -- user circuit loads data on 'di_i' at next rising edge
--                                         ________________________________                             -- user circuit asserts 'wren_i' at next edge, and removes
--      wren_i          __________________/                                \_______...                  -- 'wren_i' after 'di_rdy_o' is removed
--                      
--
--      PARALLEL READ INTERFACE
--      An internal buffer is used to copy the internal shift register data to drive the 'do_o' port. When a complete word is received,
--      the core shift register is transferred to the buffer, at the rising edge of the spi clock, 'spi_sck_i'.
--      The signal 'do_valid_o' is set one 'spi_sck_i' clock after, to directly drive a synchronous memory or fifo write enable.
--      'do_valid_o' is synchronous to the parallel interface clock, and changes only on rising edges of 'clk_i'.
--      When the interface is idle, data at the 'do_o' port holds the last word received.
--
--      PARALLEL READ PIPELINED SEQUENCE
--      ================================
--                      ______        ______        ______        ______        _
--      clk_spi_i   ___/ bit1 \______/ bitN \______/bitN-1\______/bitN-2\______/b...   -- spi base clock
--                     __    __    __    __    __    __    __    __    __    __  
--      clk_par_i   __/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \__/  \_...   -- parallel interface clock
--                  _________________ ___________________________________________...   -- 1) received data is transferred to 'do_buffer_reg'
--      do_o        __old_data_______X__________new_data_________________________...   --    after last bit received, at 'clk_spi_i' rising edge.
--                                                         _________________           -- 2) 'do_valid_o' asserted on rising edge of 'clk_par_i',
--      do_valid_o  ______________________________________/                 \____...   --    at next bit (bit N-1) of the SPI transfer.
--                  
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
-- 2011/05/15   v0.10.0050  [JD]    created the slave logic, with 2 clock domains, from SPI_MASTER module.
-- 2011/05/15   v0.15.0055  [JD]    fixed logic for starting state when CPHA='1'.
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

entity spi_slave is
    Generic (   
        N : positive := 32;                                             -- 32bit serial word length is default
        CPOL : std_logic := '0';                                        -- SPI mode selection (mode 0 default)
        CPHA : std_logic := '0';                                        -- CPOL = clock polarity, CPHA = clock phase.
        PREFETCH : positive := 1);                                      -- prefetch lookahead cycles
    Port (  
        clk_i : in std_logic := 'X';                                    -- internal interface clock (clocks di/do registers)
        rst_i : in std_logic := 'X';                                    -- synchronous rst_i: clear registers at clk_i rising edge
        spi_ssel_i : in std_logic;                                      -- spi bus slave select line
        spi_sck_i : in std_logic;                                       -- spi bus sck clock (clocks the shift register core)
        spi_mosi_i : in std_logic;                                      -- spi bus mosi input
        spi_miso_o : out std_logic := 'X';                              -- spi bus spi_miso_o output
        di_i : in  std_logic_vector (N-1 downto 0) := (others => 'X');  -- parallel load data in (clocked in on rising edge of clk_i)
        do_o : out  std_logic_vector (N-1 downto 0);                    -- parallel output (clocked out on falling clk_i)
        di_rdy_o : out std_logic;                                       -- preload lookahead: HIGH when ready for new input data
        wren_i : in std_logic := 'X';                                   -- write enable (write di_i data at next rising clk_i edge) 
                                                                        -- wren_i starts transmission. must be valid 1 clk_i cycle 
                                                                        -- before current transmission ends.
        do_valid_o : out std_logic;                                     -- do_o data valid signal, valid during one clk_i rising edge.
        do_transfer_o : out std_logic;                                  -- debug: internal transfer driver
        state_dbg_o : out std_logic_vector (5 downto 0)                 -- debug: internal state register
--        sh_reg_dbg_o : out std_logic_vector (N-1 downto 0)            -- debug: internal shift register
    );                      
end spi_slave;

--================================================================================================================
-- this architecture is a pipelined register-transfer description.
-- the spi bus and core registers are synchronous to the 'spi_sck_i' clock.
-- the parallel write/read interface is synchronous to the 'clk_i' clock.
--================================================================================================================
architecture RTL of spi_slave is
    -- constants to control FlipFlop synthesis
    constant SAMPLE_EDGE : std_logic := (CPOL xnor CPHA);
    constant SAMPLE_LEVEL : std_logic := SAMPLE_EDGE;
    constant SHIFT_EDGE : std_logic := (CPOL xor CPHA);
    -- internal state signals for register and combinational stages
    signal state_reg : natural range N+1 downto 0 := 0;
    signal state_next : natural range N+1 downto 0 := 0;
    -- shifter signals for register and combinational stages
    signal sh_reg : std_logic_vector (N-1 downto 0);
    signal sh_next : std_logic_vector (N-1 downto 0);
    -- input bit sampled buffer
    signal rx_bit_reg : std_logic;
    signal di_reg : std_logic_vector (N-1 downto 0);
    -- buffered do_o data signals for register and combinational stages
    signal do_buffer_reg : std_logic_vector (N-1 downto 0);
    signal do_buffer_next : std_logic_vector (N-1 downto 0);
    -- internal signal to flag transfer to do_buffer_reg
    signal do_transfer_reg : std_logic;
    signal do_transfer_next : std_logic;
    -- internal registered do_valid_o
    signal do_valid_oreg : std_logic;
    signal do_valid_reg : std_logic;
    signal do_valid_next : std_logic;
    -- internal registered di_rdy_o
    signal di_rdy_oreg : std_logic;
    signal di_rdy_reg : std_logic;
    signal di_rdy_next : std_logic;
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
    --  RTL REGISTER PROCESSES
    --=============================================================================================
    -- capture rx bit at SAMPLE edge of sck
    rx_bit_proc : process (spi_sck_i, spi_mosi_i) is
    begin
        if spi_sck_i'event and spi_sck_i = SAMPLE_EDGE then
            rx_bit_reg <= spi_mosi_i;
        end if;
    end process rx_bit_proc;
    -- state and data registers change on SHIFT edge of sck (ffd with async clear)
    core_reg_proc : process (spi_sck_i, rst_i, spi_ssel_i) is
    begin
        -- registers cleared on reset
        if rst_i = '1' then                                                 -- async clr
            do_buffer_reg <= (others => '0');
            do_transfer_reg <= '0';
        elsif spi_sck_i'event and spi_sck_i = SHIFT_EDGE then               -- on SHIFT edge, update all core registers
            do_buffer_reg <= do_buffer_next;
            do_transfer_reg <= do_transfer_next;
        end if;
        -- registers cleared on idle (spi_ssel_i = 1)
        if spi_ssel_i = '1' then                                            -- async clr
            state_reg <= 0;
            sh_reg <= (others => '0');
        elsif spi_sck_i'event and spi_sck_i = SHIFT_EDGE then               -- on SHIFT edge, update all core registers
            state_reg <= state_next;
            sh_reg <= sh_next;
        end if;
    end process core_reg_proc;
    -- cross-clock registers change on half-cycle of sck (ffd with async clear)
    -- this is to prevent fsm state change glitches causing setup time artifacts at async clk_i edges
    cross_reg_proc : process (rst_i, spi_sck_i, spi_ssel_i) is
    begin
        if spi_ssel_i = '1' then
            di_rdy_reg <= '1';                                              -- di_rdy true during idle    
        elsif spi_sck_i'event and spi_sck_i = SAMPLE_EDGE then              -- on half-cycle edge, update cross registers
            di_rdy_reg <= di_rdy_next;
        end if;
        if rst_i = '1' then
            do_valid_reg <= '0';                                            -- async clear on do_valid
        elsif spi_sck_i'event and spi_sck_i = SAMPLE_EDGE then              -- on half-cycle edge, update cross registers
            do_valid_reg <= do_valid_next;
        end if;
    end process cross_reg_proc;
    -- parallel load input registers (to elliminate async clock glitches)
    par_reg_proc: process (clk_i, rst_i, wren_i, spi_sck_i, spi_ssel_i) is
    begin
        if clk_i'event and clk_i = '1' then
            -- output flags registers
            if rst_i = '1' then                                             -- sync rst for output flags
                di_rdy_oreg <= '0';
                do_valid_oreg <= '0';
            else
                di_rdy_oreg <= di_rdy_reg;                                  -- transfer buffer regs to out regs
                do_valid_oreg <= (do_valid_reg and not spi_ssel_i) or (do_transfer_reg and spi_ssel_i);
            end if;
            -- input register, with 'rst_i' sync reset and 'wren_i' clock enable
            if rst_i = '1' then                                             -- sync rst for di_reg
                di_reg <= (others => '0');
            elsif wren_i = '1' then                                         -- wren_i is the clock enable for di_reg
                di_reg <= di_i;                                             -- parallel data input buffer register
            end if;
        end  if;
    end process par_reg_proc;

    --=============================================================================================
    --  RTL COMBINATIONAL LOGIC PROCESSES
    --=============================================================================================
    -- state and datapath combinational logic
    core_combi_proc : process ( rst_i, sh_reg, state_reg, rx_bit_reg, do_buffer_reg, 
                                do_valid_reg, do_transfer_reg, di_reg, di_rdy_reg, wren_i ) is
    begin
        sh_next <= sh_reg;                                                  -- all output signals are assigned to (avoid latches)
        do_buffer_next <= do_buffer_reg;
        do_valid_next <= do_valid_reg;
        do_transfer_next <= do_transfer_reg;
        di_rdy_next <= di_rdy_reg;
        spi_miso_o <= '0';                                                  -- will output '0' when shifter is empty
        state_next <= state_reg - 1;                                        -- update next state at each sck pulse
        case state_reg is
            when (N) =>
                di_rdy_next <= '0';                                         -- deassert next-data request when start shifting
                spi_miso_o <= sh_reg(N-1);                                  -- shift out tx bit from the MSb
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);              -- shift inner bits
                sh_next(0) <= rx_bit_reg;                                   -- shift in rx bit into LSb
            when (N-1) downto (PREFETCH+3) =>
                di_rdy_next <= '0';                                         -- deassert next-data request when start shifting
                do_valid_next <= do_transfer_reg;                           -- assert valid rx data, with plenty of pipeline delay for 'do_buffer'
                do_transfer_next <= '0';                                    -- reset transfer signal
                spi_miso_o <= sh_reg(N-1);                                  -- shift out tx bit from the MSb
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);              -- shift inner bits
                sh_next(0) <= rx_bit_reg;                                   -- shift in rx bit into LSb
            when (PREFETCH+2) downto 2 =>
                -- raise prefetch 'di_rdy_next' signal and remove 'do_valid'
                di_rdy_next <= '1';                                         -- request data in advance to allow for pipeline delays
                do_valid_next <= '0';                                       -- make do_valid_o HIGH for one cycle only
                spi_miso_o <= sh_reg(N-1);                                  -- shift out tx bit from the MSb
                sh_next(N-1 downto 1) <= sh_reg(N-2 downto 0);              -- shift inner bits
                sh_next(0) <= rx_bit_reg;                                   -- shift in rx bit into LSb
            when 1 =>
                -- restart from state 'N' if more sck pulses come
                do_buffer_next(N-1 downto 1) <= sh_reg(N-2 downto 0);       -- shift rx data directly into rx buffer
                do_buffer_next(0) <= rx_bit_reg;                            -- shift last rx bit into rx buffer
                do_transfer_next <= '1';                                    -- signal transfer to do_buffer
                state_next <= N;                                  	        -- next state is top bit of new data
                spi_miso_o <= sh_reg(N-1);                                  -- shift out last tx bit from the MSb
                if wren_i = '1' then                                        -- load tx register if valid data present at di_reg
                    sh_next <= di_reg;                                      -- load parallel data from di_reg into shifter
                else
                    sh_next <= (others => '0');                             -- load null data (output '0' if no load)
                end if;
            when 0 =>
                do_transfer_next <= '0';                                    -- clear signal transfer to do_buffer
                do_valid_next <= do_transfer_reg;                           -- assert valid rx data after data received, when interface idle
                di_rdy_next <= '1';                                         -- will request data if shifter empty
                spi_miso_o <= di_reg(N-1);                                  -- shift out first tx bit from the MSb
                if CPHA = '0' then
                    -- initial state for CPHA=0, when slave interface is first selected or idle
                    state_next <= N-1;                                      -- next state is top bit of new data
                    sh_next(N-1 downto 1) <= di_reg(N-2 downto 0);          -- shift inner bits
                    sh_next(0) <= rx_bit_reg;                               -- shift in rx bit into LSb
                else
                    -- initial state for CPHA=1, when slave interface is first selected or idle
                    state_next <= N;                                        -- next state is top bit of new data
                    sh_next <= di_reg;                                      -- load parallel data from di_reg into shifter
                end if;
            when others =>
                null;
        end case; 
    end process core_combi_proc;

    --=============================================================================================
    --  RTL OUTPUT LOGIC PROCESSES
    --=============================================================================================
    -- data output processes
    do_proc :           do_o <= do_buffer_reg;                              -- do_o always available
    do_valid_proc:      do_valid_o <= do_valid_oreg;                        -- copy registered do_valid_o to output
    di_rdy_proc:        di_rdy_o <= di_rdy_oreg;                            -- copy registered di_rdy_o to output

    --=============================================================================================
    --  DEBUG LOGIC PROCESSES
    --=============================================================================================
    do_transfer_proc:   do_transfer_o <= do_transfer_reg;
    state_debug_proc:   state_dbg_o <= std_logic_vector(to_unsigned(state_reg, 6)); -- export internal state to debug
--    sh_reg_debug_proc:  sh_reg_dbg_o <= sh_reg;                                   -- export sh_reg to debug
end architecture RTL;

