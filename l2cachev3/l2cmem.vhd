------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-----------------------------------------------------------------------------
-- Entity:      l2cemem
-- File:        l2cmem.vhd
-- Author:      Nils Johan Wessman, Jiri Gaisler - Gaisler Research
-- Description: Contains ram cells for the L2Cache
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library gaisler;
use gaisler.l2clib.all;
library grlib;
use grlib.stdlib.all;
library techmap;
use techmap.gencomp.all;

entity l2cmem is
  generic (
    tech     : integer range 0 to NTECH := 0;
    cen      : integer range 0 to 1 := 0;
    repl     : integer range 0 to 2 := 0;
    ways     : integer range 1 to 4 := 1;
    linesize : integer range 16 to 64 := 32;
    waysize  : integer range 1 to 512 := 1;
    scantest : integer := 0;
    arch     : integer := 0;
    ft       : integer range 0 to 1 := 0
  );
  port (
    clk   : in  std_ulogic;
    crami : in  l2cram_in_type;
    cramo : out l2cram_out_type
  );
end;

architecture rtl of l2cmem is
-- pragma translate_off
constant low32 : std_logic_vector(31 downto 0) := (others => 'L');
-- pragma translate_on

constant memdebug   : boolean := false;
constant TMR        : integer := 2;
constant CRAMDW : integer := 128;
type way_bits_type is array (1 to 4) of integer;
constant way_bits : way_bits_type := (0, 1, 2, 2);
subtype TAGRANGE is natural range TAG_HIGH downto 10+log2(waysize);
subtype OFFSETRANGE is natural range log2(linesize)-1 downto 0;
subtype CLOFFSETRANGE is natural range log2(linesize)-1 downto log2(CRAMDW/8);  -- Cache Line offset range
subtype INDEXRANGE is natural range 10+log2(waysize)-1  downto log2(linesize);
subtype WAYRANGE is natural range INDEXRANGE'left+1+way_bits(ways)-1 downto INDEXRANGE'left+1;

subtype A0DADDRRANGE is natural range INDEXRANGE'left  downto CLOFFSETRANGE'right;
subtype A1DADDRRANGE is natural range WAYRANGE'left  downto CLOFFSETRANGE'right;
constant A0DDEPTH     : integer := A0DADDRRANGE'left - A0DADDRRANGE'right + 1;
constant A1DDEPTH     : integer := A1DADDRRANGE'left - A1DADDRRANGE'right + 1;

constant TAG_BITS   : integer := TAG_HIGH+1 - (10+log2(waysize));
constant VALID_BITS : integer := 2 + 2*(linesize/64);  
constant DIRTY_BITS : integer := 2 + 2*(linesize/64);
constant TAG_CB_BITS: integer := 7; -- Num of cb for tag
constant MAX_TWIDTH : integer := TAG_BITS + TAG_CB_BITS;
constant TWIDTH     : integer := TAG_BITS + TAG_CB_BITS*ft;
constant DWIDTH     : integer := MAX_DATA_BITS+MAX_DATA_CB_BITS;
constant TDEPTH     : integer := 10+log2(waysize) - log2(linesize);
constant LRU_BITS   : integer := l2c_lru_table(ways);

subtype ddata_vector is std_logic_vector(DWIDTH-1 downto 0);
type ddata_type is array (0 to MAX_WAYS-1) of ddata_vector;
subtype dcb_vector is std_logic_vector(MAX_DATA_CB_BITS-1 downto 0);
type dcb_type is array (0 to MAX_WAYS-1) of dcb_vector;
subtype tdata_vector is std_logic_vector(MAX_TWIDTH-1 downto 0);
type tdata_type is array (0 to MAX_WAYS-1) of tdata_vector;

subtype tddata_type is std_logic_vector(((DIRTY_BITS+VALID_BITS)*ways+LRU_BITS)-1 downto 0);

function orvb (d : l2cdctrl_type) return l2cdctrl_vector is
variable tmp : l2cdctrl_vector;
begin
  tmp := (others => '0');
  for i in 0 to ways-1 loop
    tmp := tmp or d(i);
  end loop;
  return (tmp);
end;

signal tdatain  : tdata_type;
signal tddatain : tddata_type;
signal dcbin    : dcb_type;
signal tdataout : tdata_type;
signal tddataout: tddata_type;
signal ddataout : ddata_type;
signal dcbout   : dcb_type;
signal twrite   : std_logic_vector(MAX_WAYS-1 downto 0);
signal tenable  : std_logic_vector(MAX_WAYS-1 downto 0);
signal tdenable : std_logic;
signal tdwrite  : std_logic;
signal dwrite   : l2cdctrl_type;
signal denable  : l2cdctrl_type;
signal ddatain  : ddata_type;
signal taddr    : std_logic_vector(TDEPTH-1 downto 0);
signal twaddr   : std_logic_vector(TDEPTH-1 downto 0);
signal q        : ddata_vector;
signal qx       : ddata_type;
signal dwritex  : l2cdctrl_vector;
signal denablex : l2cdctrl_vector;
signal enx      : std_logic_vector(MAX_WAYS-1 downto 0);

signal a1daddr  : std_logic_vector(A1DADDRRANGE);

signal testin   : std_logic_vector(3 downto 0);

begin
  -- Input signals
  insig : for i in 0 to ways-1 generate
    tdatain(i)(TAG_CB_BITS-1 + TAG_BITS downto TAG_BITS) <=   crami.way(i).tcb(TAG_CB_BITS-1 downto 0); 
    tdatain(i)(TAG_BITS-1 downto 0) <=   crami.way(i).tag(TAGRANGE); 
    tddatain((DIRTY_BITS+VALID_BITS)*(i+1)+LRU_BITS-1 downto (DIRTY_BITS+VALID_BITS)*i+LRU_BITS) <= crami.way(i).dirty(VALID_BITS-1 downto 0) & crami.way(i).valid(VALID_BITS-1 downto 0);
    ddatain(i)(155 downto 0) <= crami.cb(27 downto 0) & crami.data(127 downto 0);
  end generate;
  tddatain(LRU_BITS-1 downto 0) <= crami.lru(LRU_BITS-1 downto 0);
  tdenable <= orv(crami.tenable(ways-1 downto 0));
  tdwrite  <= crami.tdwrite;

  twrite  <= crami.twrite;
  tenable <= crami.tenable;
  dwrite  <= crami.dwrite;
  denable <= crami.denable;
  taddr   <= crami.taddr(TDEPTH-1 downto 0);
  twaddr  <= crami.twaddr(TDEPTH-1 downto 0);
  dwritex <= orvb(crami.dwrite);
  denablex<= orvb(crami.denable);
 
  a1daddr <= conv_std_logic_vector(crami.dway, way_bits(ways)) & crami.dindex(INDEXRANGE) & crami.doffset(CLOFFSETRANGE);

  testin <= (others => '0'); --crami.testen & "0" & crami.testin(TESTIN_WIDTH-3 downto 0);
  
  -- Memory blocks
    -- TAG memory
  tags0 : for i in 0 to ways-1 generate
      t : syncram
      generic map (tech, TDEPTH, TWIDTH, scantest)
      port map (clk, taddr, tdatain(i)(TWIDTH-1 downto 0), tdataout(i)(TWIDTH-1 downto 0), tenable(i), twrite(i), testin);
  end generate;

  ften : if ft /= 0 generate
    tdirty0 : syncram_2pft
      generic map (tech, TDEPTH, (DIRTY_BITS+VALID_BITS)*ways+LRU_BITS, 0, 1, TMR*ft, scantest) 
      port map (clk, tdenable, taddr, tddataout, clk, tdwrite, twaddr, tddatain, open, testin);
  end generate;

  noft : if ft = 0 generate
    tdirty0 : syncram_2p
      generic map (tech, TDEPTH, (DIRTY_BITS+VALID_BITS)*ways+LRU_BITS, 0, 1, scantest) 
      port map (clk, tdenable, taddr, tddataout, clk, tdwrite, twaddr, tddatain, testin);
  end generate;

    -- Data memory
  arch0 : if (arch = 0) or memdebug generate
    data0 : for i in 0 to ways-1 generate
      ft0 : if ft /= 0 generate
        m0 : syncram156bw generic map (tech, A0DDEPTH, scantest) 
             port map (clk, a1daddr(A0DADDRRANGE), ddatain(i)(155 downto 0), qx(i)(155 downto 0), denable(i)(15 downto 0), dwrite(i)(15 downto 0), testin);
        q0 : if not memdebug generate
             ddataout(i)(127 downto 0) <= qx(i)(127 downto 0);
             dcbout(i)(27 downto 0) <= qx(i)(155 downto 128);
        end generate;
      end generate;
      noft0 : if ft = 0 generate
        m0 : syncram128bw generic map (tech, A0DDEPTH, scantest) 
             port map (clk, a1daddr(A0DADDRRANGE), ddatain(i)(127 downto 0), qx(i)(127 downto 0), denable(i)(15 downto 0), dwrite(i)(15 downto 0), testin);
        q0 : if not memdebug generate
             ddataout(i)(127 downto 0) <= qx(i)(127 downto 0);
             dcbout(i) <= (others => '0');
        end generate;
      end generate;
    end generate;
  end generate;

  arch1 : if arch = 1 generate
    ft1 : if ft /= 0 generate 
      m0 : syncram156bw generic map (tech, A1DDEPTH, scantest) 
           port map (clk, a1daddr(A1DADDRRANGE), ddatain(0)(155 downto 0), q(155 downto 0), denablex(15 downto 0), dwritex(15 downto 0), testin);
      q0 : if not memdebug generate
           ddataout <= (others => zero32(27 downto 0) & q(127 downto 0));
           dcbout <= (others => q(155 downto 128));
      end generate;
    end generate;
    noft1 : if ft = 0 generate
      m0 : syncram128bw generic map (tech, A1DDEPTH, scantest) 
           port map (clk, a1daddr(A1DADDRRANGE), ddatain(0)(127 downto 0), q(127 downto 0), denablex(15 downto 0), dwritex(15 downto 0), testin);
      q0 : if not memdebug generate
           ddataout <= (others => zero32(27 downto 0) & q(127 downto 0));
           dcbout <= (others => (others => '0'));
      end generate;
    end generate;
  end generate;
  
  -- Output signals
  outsig : for i in 0 to ways-1 generate
    cramo.way(i).tcb(TAG_CB_BITS-1 downto 0) <= tdataout(i)(TAG_CB_BITS-1+TAG_BITS downto TAG_BITS) when ft = 1 else zero32(TAG_CB_BITS-1 downto 0);
    cramo.way(i).tag(TAGRANGE) <= tdataout(i)(TAG_BITS-1 downto 0);
    -- tmp zero on non used bits

    -- pragma translate_off
    cramo.way(i).tag(TAG_HIGH-TAG_BITS downto 0) <= low32(TAG_HIGH-TAG_BITS downto 0);
    -- pragma translate_on
    
    cramo.way(i).valid <= tddataout((DIRTY_BITS+VALID_BITS)*(i+1)+LRU_BITS-DIRTY_BITS-1 downto (DIRTY_BITS+VALID_BITS)*i+LRU_BITS) when linesize = 64 else
                          zero32(MAX_VALID_BITS-1 downto VALID_BITS) & tddataout((DIRTY_BITS+VALID_BITS)*(i+1)+LRU_BITS-DIRTY_BITS-1 downto (DIRTY_BITS+VALID_BITS)*i+LRU_BITS);
    cramo.way(i).dirty <= tddataout((DIRTY_BITS+VALID_BITS)*(i+1)+LRU_BITS-1 downto (DIRTY_BITS+VALID_BITS)*i+LRU_BITS+VALID_BITS) when linesize = 64 else
                          zero32(MAX_VALID_BITS-1 downto VALID_BITS) & tddataout((DIRTY_BITS+VALID_BITS)*(i+1)+LRU_BITS-1 downto (DIRTY_BITS+VALID_BITS)*i+LRU_BITS+VALID_BITS);
    
    cramo.data(i) <= ddataout(i)(127 downto 0);
    cramo.cb(i) <= dcbout(i)(27 downto 0);
  end generate;

  unusedway2 : if ways < 2 generate
    cramo.data(1) <= (others => '0');
    cramo.cb(1) <= (others => '0');
    cramo.way(1).valid <= (others => '0');
    cramo.way(1).dirty <= (others => '0');
    cramo.way(1).tag <= (others => '0');
    cramo.way(1).tcb <= (others => '0');
  end generate;
  unusedway3 : if ways < 3 generate
    cramo.data(2) <= (others => '0');
    cramo.cb(2) <= (others => '0');
    cramo.way(2).valid <= (others => '0');
    cramo.way(2).dirty <= (others => '0');
    cramo.way(2).tag <= (others => '0');
    cramo.way(2).tcb <= (others => '0');
  end generate;
  unusedway4 : if ways < 4 generate
    cramo.data(3) <= (others => '0');
    cramo.cb(3) <= (others => '0');
    cramo.way(3).valid <= (others => '0');
    cramo.way(3).dirty <= (others => '0');
    cramo.way(3).tag <= (others => '0');
    cramo.way(3).tcb <= (others => '0');
  end generate;
  
  -- pragma translate_off
  unusedlru : if ways < 4 generate
    cramo.lru(MAX_LRU_BITS-1 downto LRU_BITS) <= low32(MAX_LRU_BITS-1 downto LRU_BITS);
  end generate;
  -- pragma translate_on
  cramo.lru(LRU_BITS-1 downto 0) <= tddataout(LRU_BITS-1 downto 0);


-- for testing only
-- pragma translate_off
  dbg : if memdebug and (arch = 1) generate
      r1 : process (clk)
      begin
	if rising_edge(clk) then
	  for i in 0 to ways-1 loop
	    enx(i) <= orv(denable(i)) and not orv(dwrite(i));
	    if (enx(i) = '1') then
	      assert (to_x01(q) = to_x01(ddataout(i)))
	      report "cache RAM mismatch" severity failure;
	    end if;
	  end loop;
        end if;
      end process;
  end generate;
-- pragma translate_on

end;
