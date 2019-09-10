------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-----------------------------------------------------------------------------
-- Entity:      l2c
-- File:        l2c.vhd
-- Author:      Nils Johan Wessman, Jiri Gaisler - Gaisler Research
-- Description: L2Cache top level (cache controller with memory and AHB interface)
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library techmap;
use techmap.gencomp.all;
library grlib;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
library gaisler;
use gaisler.l2clib.all;

entity l2c is
  generic (
    hslvidx     : integer := 0;
    hmstidx     : integer := 0;
    haddr       : integer := 16#F00#;
    hmask       : integer := 16#F00#;
    ioaddr      : integer := 16#000#;
    cached      : integer := 16#0000#;
    hirq        : integer := 0;
    cen         : integer range 0 to 1 := 0;
    hproten     : integer range 0 to 1 := 0;
    wp          : integer range 0 to 1 := 0;
    repl        : integer range 0 to 1 := 0;
    ways        : integer range 1 to 4 := 1;
    linesize    : integer range 16 to 64 := 32;
    waysize     : integer range 1 to 512 := 1;
    memtech     : integer range 0 to NTECH := 0;
    bbuswidth   : integer := 128;
    bioaddr     : integer := 16#000#;
    biomask     : integer := 16#000#;
    sbus        : integer := 0;
    mbus        : integer := 1;
    stat        : integer range 0 to 2 := 0;
    scantest    : integer := 0;
    arch        : integer := 0;
    mtrr        : integer := 0;
    edacen      : integer range 0 to 1 := 0;
    rmw         : integer range 0 to 1 := 0;
    ft          : integer range 0 to 1 := 0;
    fttiming    : integer range 0 to 1 := 0;
    wbmask      : integer range 0 to 16#FFFF# := 16#FFFF#;
    debug       : integer range 0 to 1 := 0
  );
  port (
    rst   : in  std_ulogic;
    clk   : in  std_ulogic;
    ahbsi : in  ahb_slv_in_type;
    ahbso : out ahb_slv_out_type;
    ahbmi : in  ahb_mst_in_type;
    ahbmo : out ahb_mst_out_type;
    ahbsov: in  ahb_slv_out_vector;
    sto   : out std_logic_vector(2 downto 0);
    debugo: out std_logic_vector(255*debug downto 0)
  );
end;

architecture rtl of l2c is
signal crami  : l2cram_in_type;
signal cramo  : l2cram_out_type;
signal dmai   : l2c_dma_ahb_in_type;
signal dmao   : l2c_dma_ahb_out_type;

signal fetop  : l2c_fe_to_pipe_type;
signal ptofe  : l2c_pipe_to_fe_type;
signal ptobe  : l2c_pipe_to_be_type;
signal betop  : l2c_be_to_pipe_type;
signal fetobe : l2c_fe_to_be_type;
signal betofe : l2c_be_to_fe_type;

begin

  -- Cache Frontend (AHB slave)
  fe : l2cfe
  --generic map()
  generic map(hslvidx => hslvidx, haddr => haddr, hmask => hmask, ioaddr => ioaddr, 
        cached => cached, hirq => hirq, cen => cen, hproten => hproten, wp => wp, repl => repl, 
        ways => ways, linesize => linesize, waysize => waysize, tech => memtech,
        bbuswidth => bbuswidth, bioaddr => bioaddr, biomask => biomask,
        sbus => sbus, mbus => mbus, stat => stat, mtrr => mtrr, edacen => edacen, rmw => rmw, 
        ft => ft, fttiming => fttiming, scantest => scantest, wbmask => wbmask, debug => debug)
  port map (
    rst     => rst,
    clk     => clk,
    ahbsi   => ahbsi, 
    ahbso   => ahbso,
    ahbsov  => ahbsov,
    fetop   => fetop,
    ptofe   => ptofe,
    fetobe  => fetobe,
    betofe  => betofe,
    debugo  => debugo
  );
  
  -- Cache PIPE
  pipe : l2cpipe
  --generic map()
  generic map(hslvidx => hslvidx, haddr => haddr, hmask => hmask, ioaddr => ioaddr, 
        cached => cached, hirq => hirq, cen => cen, hproten => hproten, wp => wp, repl => repl, 
        ways => ways, linesize => linesize, waysize => waysize, tech => memtech,
        bbuswidth => bbuswidth, bioaddr => bioaddr, biomask => biomask,
        sbus => sbus, mbus => mbus, stat => stat, mtrr => mtrr, edacen => edacen, rmw => rmw, 
        ft => ft, fttiming => fttiming, scantest => scantest, wbmask => wbmask, debug => debug)
  port map (
    rst     => rst,
    clk     => clk,
    fetop   => fetop,
    ptofe   => ptofe,
    ptobe   => ptobe,
    betop   => betop,
    cramo   => cramo, 
    crami   => crami
  );

  -- Cache Backend (AHB master)
  be : l2cbe
  --generic map()
  generic map(hslvidx => hslvidx, haddr => haddr, hmask => hmask, ioaddr => ioaddr, 
        cached => cached, hirq => hirq, cen => cen, hproten => hproten, wp => wp, repl => repl, 
        ways => ways, linesize => linesize, waysize => waysize, tech => memtech,
        bbuswidth => bbuswidth, bioaddr => bioaddr, biomask => biomask,
        sbus => sbus, mbus => mbus, stat => stat, mtrr => mtrr, edacen => edacen, rmw => rmw, 
        ft => ft, fttiming => fttiming, scantest => scantest, wbmask => wbmask, debug => debug)
  port map (
    rst     => rst,
    clk     => clk,
    ptobe   => ptobe,
    betop   => betop,
    fetobe  => fetobe,
    betofe  => betofe,
    dmai    => dmai,
    dmao    => dmao
  );



--  -- Cache controller (AHB slave)
--    ctrl : l2cctl 
--    generic map(hslvidx => hslvidx, haddr => haddr, hmask => hmask, ioaddr => ioaddr, 
--          cached => cached, hirq => hirq, cen => cen, hproten => hproten, wp => wp, repl => repl, 
--          ways => ways, linesize => linesize, waysize => waysize, tech => memtech,
--	        bbuswidth => bbuswidth, bioaddr => bioaddr, biomask => biomask,
--          sbus => sbus, mbus => mbus, stat => stat, mtrr => mtrr, edacen => edacen, rmw => rmw, 
--          ft => ft, fttiming => fttiming, scantest => scantest, wbmask => wbmask, debug => debug)
--    port map(rst => rst, clk => clk, cramo => cramo, crami => crami, ahbsi => ahbsi,
--             ahbso => ahbso, ahbsov => ahbsov, dmai => dmai, dmao => dmao, sto => sto, debugo => debugo);

  -- Cache memory
  mem : l2cmem 
  generic map(tech => memtech, cen => cen, repl => repl, ways => ways,
          linesize => linesize, waysize => waysize, scantest => scantest, arch => arch, ft => ft)
  port map(clk => clk, crami => crami, cramo => cramo);

  -- AHB master
  ahbm : l2cahb
  generic map(hindex => hmstidx, venid => VENDOR_GAISLER,
              devid => GAISLER_L2CACHE, version => 0)
  port map(rst => rst, clk => clk, ahbmi => ahbmi, ahbmo => ahbmo, 
           dmai0 => dmai, dmao0 => dmao, 
           dmai1 => l2c_dma_ahb_in_none, dmao1 => open);
end;
