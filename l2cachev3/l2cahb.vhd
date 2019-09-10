------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-------------------------------------------------------------------------------
-- Entity:      grpci2_ahb_mst
-- File:        grpci2_ahb_mst.vhd
-- Author:      Nils-Johan Wessman - Aeroflex Gaisler
-- Description: L2C AHB master interface 
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
library grlib;
use grlib.stdlib.all;
use grlib.amba.all;
use grlib.devices.all;

use work.l2clib.all;

entity l2cahb is
  generic(
    hindex      : integer := 0;
    venid   : integer := VENDOR_GAISLER;
    devid   : integer := 0;
    version : integer := 0;
    scantest: integer := 0
  );
  port(
    rst         : in  std_ulogic;
    clk         : in  std_ulogic;
    ahbmi       : in  ahb_mst_in_type;
    ahbmo       : out ahb_mst_out_type;
    dmai0       : in  l2c_dma_ahb_in_type;
    dmao0       : out l2c_dma_ahb_out_type;
    dmai1       : in  l2c_dma_ahb_in_type;
    dmao1       : out l2c_dma_ahb_out_type
  );
end entity;

architecture rtl of l2cahb is
  type reg_type is record
    bg     : std_ulogic; --bus granted
    bo     : std_ulogic; --bus owner, 0=dma0, 1=dma1
    ba     : std_ulogic; --bus active
    bb     : std_ulogic; --1kB burst boundary detected
    retry  : std_ulogic; 
  end record;
  
  constant hconfig : ahb_config_type := (
    0 => ahb_device_reg ( venid, devid, 0, version, 0),
    others => zero32);

  signal r, rin : reg_type;
begin
  comb : process(rst, r, dmai1, dmai0, ahbmi) is
  variable v       : reg_type;
  variable htrans  : std_logic_vector(1 downto 0);
  variable hbusreq : std_ulogic;
  variable hwrite  : std_ulogic; 
  variable haddr   : std_logic_vector(31 downto 0);
  variable hwdata  : std_logic_vector(31 downto 0);
  variable hwdata128  : std_logic_vector(127 downto 0);
  variable nbo     : std_ulogic; 
  variable retry1  : std_ulogic;
  variable retry0  : std_ulogic;
  variable ready0  : std_ulogic;
  variable ready1  : std_ulogic;
  variable error0  : std_ulogic;
  variable error1  : std_ulogic;
  variable grant1  : std_ulogic;
  variable grant0  : std_ulogic;
  variable hsize   : std_logic_vector(2 downto 0);
  variable hburst  : std_logic_vector(2 downto 0);
  begin
    v := r; htrans := HTRANS_IDLE; ready0 := '0'; ready1 := '0'; retry1 := '0';
    retry0 := '0'; error0 := '0'; error1 := '0'; grant1 := '0'; grant0 := '0';
    hsize := HSIZE_WORD;
    hburst := HBURST_INCR;

    if r.bo = '0' then hwdata128 := dmai0.wdata128;
    else hwdata128 := dmai1.wdata128; end if;
    
    hbusreq := dmai1.req or dmai0.req;
    if hbusreq = '1' then htrans := HTRANS_NONSEQ; end if;

    if r.retry = '0' then
      nbo := dmai1.req and not (dmai0.req and not r.bo);
    else
      nbo := r.bo;
    end if;

    if nbo = '0' then
      haddr := dmai0.addr; hwrite := dmai0.write; hsize := dmai0.size; --hsize := '0' & dmai0.size;
      hburst := dmai0.hburst;
      --if dmai0.burst = '0' then hburst := HBURST_SINGLE; end if;
      if (dmai0.req and r.ba
          and not r.bo and not r.retry) = '1' and dmai0.size(2 downto 1) /= "00" 
          and dmai0.hburst(0) = '1' and dmai0.noseq = '0' then
        htrans := HTRANS_SEQ; 
      end if;
      if (dmai0.req and r.bg and ahbmi.hready and not r.retry) = '1' 
      then grant0 := '1'; end if; 
      
    else
      haddr := dmai1.addr; hwrite := dmai1.write; hsize := dmai1.size; --hsize := '0' & dmai1.size;
      hburst := dmai1.hburst;
      --if dmai1.burst = '0' then hburst := HBURST_SINGLE; end if;
      if (dmai1.req and r.ba 
          and r.bo and not r.retry) = '1' and dmai1.size(2 downto 1) /= "00" 
          and dmai1.hburst(0) = '1' and dmai1.noseq = '0' then
        htrans := HTRANS_SEQ; 
      end if;
      if (dmai1.req and r.bg and ahbmi.hready and not r.retry) = '1' 
      then grant1 := '1'; end if; 
    
    end if;

    --1 kB burst boundary
    if ahbmi.hready = '1' then
      if haddr(9 downto 2) = "11111111" 
      --or haddr(4 downto 2) = "111" 
      then
        v.bb := '1';
        if htrans = HTRANS_SEQ then hbusreq := '0'; end if;
      elsif ((dmai0.noreq and grant0) or (dmai1.noreq and grant1)) = '1' then
        v.bb := '1';
        hbusreq := '0';
      else
        v.bb := '0';
      end if;
    end if;


    if (r.bb = '1') and (htrans /= HTRANS_IDLE) then
      htrans := HTRANS_NONSEQ;
    end if;
        
    if r.bo = '0' then
      if r.ba = '1' then
        if ahbmi.hready = '1' then
          case ahbmi.hresp is
          when HRESP_OKAY => ready0 := '1';
          when HRESP_SPLIT | HRESP_RETRY => retry0 := '1';
          when HRESP_ERROR => error0 := '1';
          when others => null;
          end case; 
        end if;
      end if;
    else
      if r.ba = '1' then
        if ahbmi.hready = '1' then
          case ahbmi.hresp is
          when HRESP_OKAY => ready1 := '1';
          when HRESP_SPLIT | HRESP_RETRY => retry1 := '1';
          when HRESP_ERROR => error1 := '1';
          when others => null;
          end case; 
        end if;
      end if;
    end if;

    if (r.ba = '1') and 
       ((ahbmi.hresp = HRESP_RETRY) or (ahbmi.hresp = HRESP_SPLIT) or (ahbmi.hresp = HRESP_ERROR))
    then v.retry := not ahbmi.hready; else v.retry := '0'; end if;
      
    if r.retry = '1' then htrans := HTRANS_IDLE; end if;
    
    if ahbmi.hready = '1' then
      v.bo := nbo; v.bg := ahbmi.hgrant(hindex);
      if (htrans = HTRANS_NONSEQ) or (htrans = HTRANS_SEQ) then
        v.ba := r.bg;
      else
        v.ba := '0';
      end if;
    end if;

    if rst = '0' then
      v.bg := '0'; v.ba := '0'; v.bo := '0'; v.bb := '0';
    end if;
    
    rin <= v;
    
    dmao0.rdata128(127 downto 96) <= ahbmi.hrdata(127 mod AHBDW downto 96 mod AHBDW);
    dmao0.rdata128(95 downto 64) <= ahbmi.hrdata(95 mod AHBDW downto 64 mod AHBDW);
    dmao0.rdata128(63 downto 32) <= ahbmi.hrdata(63 mod AHBDW downto 32 mod AHBDW);
    dmao0.rdata128(31 downto 0) <= ahbmi.hrdata(31 downto 0);
    
    dmao1.error    <= error1;
    dmao1.retry    <= retry1;
    dmao1.ready    <= ready1;
    dmao0.error    <= error0;
    dmao0.retry    <= retry0;
    dmao0.ready    <= ready0;
    dmao1.grant    <= grant1;
    dmao0.grant    <= grant0;
    dmao1.hirq     <= ahbmi.hirq;
    dmao0.hirq     <= ahbmi.hirq;
    ahbmo.htrans   <= htrans;
    ahbmo.hsize	   <= hsize;
    ahbmo.hbusreq  <= hbusreq;
    ahbmo.haddr	   <= haddr;
    ahbmo.hwrite   <= hwrite;
    ahbmo.hwdata   <= hwdata128(AHBDW-1 downto 0);
    ahbmo.hburst   <= hburst;

    ahbmo.hconfig <= hconfig;
    ahbmo.hindex  <= hindex;
  end process;

  regs : process(clk)
  begin
    if rising_edge(clk) then r <= rin; end if;
  end process; 
 
  ahbmo.hlock	 <= '0';
  ahbmo.hprot	 <= "0011";
  ahbmo.hirq   <= (others => '0');
end architecture; 
