------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-----------------------------------------------------------------------------   
-- Entity:      l2cbe
-- File:        l2cbe.vhd
-- Author:      Nils Johan Wessman - Aeroflex Gaisler
-- Description: Back-end for the L2-cache PIPE
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

entity l2cbe is
  generic (
    hslvidx     : integer := 0;
    haddr       : integer := 16#F00#;
    hmask       : integer := 16#F00#;
    ioaddr      : integer := 16#000#;
    cached      : integer := 16#0000#;
    hirq        : integer := 0;
    cen         : integer range 0 to 1  := 0;
    hproten     : integer range 0 to 1  := 0;
    wp          : integer range 0 to 1  := 0;
    repl        : integer range 0 to 1  := 0;
    ways        : integer range 1 to 4  := 1;
    linesize    : integer range 16 to 64  := 32;
    waysize     : integer range 1 to 512 := 1;
    tech        : integer range 0 to NTECH := 0;
    bbuswidth   : integer := 128;
    bioaddr     : integer := 16#000#;
    biomask     : integer := 16#000#;
    sbus        : integer := 0;
    mbus        : integer := 1;
    stat        : integer range 0 to 2 := 0;
    mtrr        : integer range 0 to 32 := 0;
    edacen      : integer range 0 to 1 := 0;
    rmw         : integer range 0 to 1 := 0;
    ft          : integer range 0 to 1 := 0;
    fttiming    : integer range 0 to 1 := 0;
    scantest    : integer range 0 to 1 := 0;
    wbmask      : integer range 0 to 16#FFFF# := 16#FFFF#;
    debug       : integer range 0 to 1 := 0);
  port (
    rst     : in  std_ulogic;
    clk     : in  std_ulogic;
    ptobe   : in  l2c_pipe_to_be_type;
    betop   : out l2c_be_to_pipe_type;
    fetobe  : in  l2c_fe_to_be_type;
    betofe  : out l2c_be_to_fe_type;
    dmai    : out l2c_dma_ahb_in_type;
    dmao    : in  l2c_dma_ahb_out_type
);
end;

architecture rtl of l2cbe is
  subtype WAYRANGE is natural range ways-1 downto 0;
  subtype TAGRANGE is natural range TAG_HIGH downto 10+log2(waysize);
  subtype INDEXRANGE is natural range 10+log2(waysize)-1  downto log2(linesize);
  subtype OFFSETRANGE is natural range log2(linesize)-1 downto 0;
  subtype LRURANGE is natural range l2c_lru_table(ways)-1 downto 0;
  subtype VALIDRANGE is natural range (linesize/16)-1 downto 0;
  subtype DIRTYRANGE is natural range (linesize/16)-1 downto 0;
  subtype MTRBITRANGE is natural range 31 downto 18;
  subtype CLOFFSETRANGE is natural range log2(linesize)-1 downto log2(CRAMDW/8);  -- Cache Line offset range
  subtype DATARANGE is natural range CRAMDW-1 downto 0;
  subtype DATACBRANGE is natural range (CRAMDW/(8*4)*7)-1 downto 0;
  subtype DATAWAYRANGE is natural range 10+log2(waysize)+log2(ways)-1 downto 10+log2(waysize);
  subtype BENRANGE is natural range (CRAMDW/8)-1 downto 0;
  constant VALID_BITS   : integer := LINESIZE/16;
  constant DIRTY_BITS   : integer := LINESIZE/16;

  type sahb_acc_type is record
    valid : std_logic;
    blen  : std_logic_vector(15 downto 0);
    hburst: std_logic_vector(2 downto 0);
    offset: std_logic_vector(CLOFFSETRANGE);
    bsize : std_logic_vector(2 downto 0);
  end record;
  type sahb_acc_multi_type is array (0 to 1) of sahb_acc_type;
  type sahb_state is (saidle, saread, sawrite);

  function set_acc_hburst(blen : in std_logic_vector(15 downto 0)) return std_logic_vector is
    variable hburst : std_logic_vector(2 downto 0);
  begin
    if blen(0) = '1' then hburst := "111";              -- 16-beats
    elsif blen(8) = '1' then hburst := "101";           -- 8-beats
    elsif blen(12 downto 11) = "10" then hburst := "011"; -- 4-beats
    elsif blen(14) = '1' then hburst := "001";           -- inc
    else hburst := "000"; end if;                       -- single
    return hburst;
  end function;
  function set_acc_blen(bussize : in std_logic_vector(2 downto 0); 
                        line_sec: in integer range 0 to 4) return std_logic_vector is
    variable blen : std_logic_vector(15 downto 0);
  begin
    blen := (others => '0');
    case bussize is
      when "010" => -- 32-bit
        case line_sec is
          when 1 => blen(15 downto 12) := (others => '1');
          when 2 => blen(15 downto  8) := (others => '1');
          when 3 => blen(15 downto  4) := (others => '1');
          when 4 => blen(15 downto  0) := (others => '1'); 
          when others => 
        end case;
      when "011" => -- 64-bit
        case line_sec is
          when 1 => blen(15 downto 14) := (others => '1');
          when 2 => blen(15 downto 12) := (others => '1');
          when 3 => blen(15 downto 10) := (others => '1');
          when 4 => blen(15 downto  8) := (others => '1');
          when others => 
        end case;
      --when "100" => -- 128-bit
      when others =>
        case line_sec is
          when 1 => blen(15 downto 15) := (others => '1');
          when 2 => blen(15 downto 14) := (others => '1');
          when 3 => blen(15 downto 13) := (others => '1');
          when 4 => blen(15 downto 12) := (others => '1');
          when others => 
        end case;
    end case;
    return blen;
  end function;
  
  function get_acc_widebus(haddr  : in std_logic_vector(31 downto 0);
                           wbmask : in integer) return std_logic is
    variable ctbl   : std_logic_vector(15 downto 0);
    variable wbarea : std_logic;
  begin
    ctbl := conv_std_logic_vector(wbmask, 16);
    if wbmask = 16#FFFF# then wbarea := '1';
    else wbarea := ctbl(conv_integer(haddr(31 downto 28))); end if;
    return wbarea;
  end function;
  
  function set_acc_offset(linesec: in std_logic_vector(3 downto 0)) return std_logic_vector is
    variable offset : std_logic_vector(CLOFFSETRANGE);
  begin
    if    linesec(0) = '1' then offset := conv_std_logic_vector(0, CLOFFSETRANGE'left-CLOFFSETRANGE'right+1);
    elsif linesec(1) = '1' then offset := conv_std_logic_vector(1, CLOFFSETRANGE'left-CLOFFSETRANGE'right+1);
    elsif linesec(2) = '1' then offset := conv_std_logic_vector(2, CLOFFSETRANGE'left-CLOFFSETRANGE'right+1);
    else                        offset := conv_std_logic_vector(3, CLOFFSETRANGE'left-CLOFFSETRANGE'right+1); end if;
    return offset;
  end function;
  
  function set_acc(bussize : in std_logic_vector(2 downto 0); 
                   bypass  : in std_logic;
                   line_sec: in std_logic_vector(VALID_BITS-1 downto 0);
                   accsize : in std_logic_vector(2 downto 0)) return sahb_acc_multi_type is
    variable blen : std_logic_vector(15 downto 0);
    variable linesec : std_logic_vector(3 downto 0);
    variable a : sahb_acc_multi_type;
  begin
    blen := (others => '0');
    linesec := (others => '0'); linesec(line_sec'range) := line_sec;
  
    if bypass = '0' then
      a(0).bsize := bussize;
      a(1).bsize := bussize;
      case linesec is 
        when "0000" => 
          a(0).valid := '0'; a(0).blen := set_acc_blen(bussize, 0); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset(linesec);
          a(1).valid := '0'; a(1).blen := (others => '0'); a(1).hburst := "000"; a(1).offset := (others => '0'); 
        when "0001" | "0010" | "0100" | "1000" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 1); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset(linesec);
          a(1).valid := '0'; a(1).blen := (others => '0'); a(1).hburst := "000"; a(1).offset := (others => '0'); 
        when "0011" | "0110" | "1100" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 2); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset(linesec);
          a(1).valid := '0'; a(1).blen := (others => '0'); a(1).hburst := "000"; a(1).offset := (others => '0'); 
        when "0111" | "1110" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 3); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset(linesec);
          a(1).valid := '0'; a(1).blen := (others => '0'); a(1).hburst := "000"; a(1).offset := (others => '0'); 
        when "1111" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 4); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset("1111");
          a(1).valid := '0'; a(1).blen := (others => '0'); a(1).hburst := "000"; a(1).offset := (others => '0'); 
        when "0101" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 1); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset("0001");
          a(1).valid := '1'; a(1).blen := set_acc_blen(bussize, 1); 
          a(1).hburst := set_acc_hburst(a(1).blen); a(1).offset := set_acc_offset("0100");
        when "1001" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 1); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset("0001");
          a(1).valid := '1'; a(1).blen := set_acc_blen(bussize, 1); 
          a(1).hburst := set_acc_hburst(a(1).blen); a(1).offset := set_acc_offset("1000");
        when "1010" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 1); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset("0010");
          a(1).valid := '1'; a(1).blen := set_acc_blen(bussize, 1); 
          a(1).hburst := set_acc_hburst(a(1).blen); a(1).offset := set_acc_offset("1000");
        when "1011" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 2); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset("0011");
          a(1).valid := '1'; a(1).blen := set_acc_blen(bussize, 1); 
          a(1).hburst := set_acc_hburst(a(1).blen); a(1).offset := set_acc_offset("1000");
        when "1101" => 
          a(0).valid := '1'; a(0).blen := set_acc_blen(bussize, 1); 
          a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := set_acc_offset("0001");
          a(1).valid := '1'; a(1).blen := set_acc_blen(bussize, 2); 
          a(1).hburst := set_acc_hburst(a(1).blen); a(1).offset := set_acc_offset("1100");
        when others =>
      end case;
    else
      a(0).blen := (others => '0'); 
      if accsize < bussize then a(0).bsize := accsize; 
      else a(0).bsize := bussize; end if;
      a(1).bsize := bussize;
      case bussize is
        when "010" => -- 32-bit
          if    accsize = "100" then a(0).blen(15 downto 12) := (others => '1'); 
          elsif accsize = "011" then a(0).blen(15 downto 14) := (others => '1'); 
          else a(0).blen(15 downto 15) := (others => '1'); end if;
        when "011" => -- 64-bit
          if accsize = "100" then a(0).blen(15 downto 14) := (others => '1');
          else a(0).blen(15 downto 15) := (others => '1'); end if;
        --when "100" => -- 128-bit
        when others =>
          a(0).blen(15 downto 15) := (others => '1');
      end case;
      a(0).valid := '1'; a(0).hburst := set_acc_hburst(a(0).blen); a(0).offset := (others => '0');
      a(1).valid := '0'; a(1).blen := (others => '0'); a(1).hburst := "000"; a(1).offset := (others => '0'); 
    end if;
    return a;
  end function;
  
  type sahb_reg_type is record 
    state   : sahb_state;
    dmai0   : l2c_dma_ahb_in_type;
    done    : std_logic_vector(2 downto 0);
    stop    : std_logic;
    dma_hold: std_logic;
    active  : std_logic;
    noreq   : std_logic;
    updsize : std_logic_vector(3 downto 0);
    size    : std_logic_vector(2 downto 0);
    blen    : std_logic_vector(15 downto 0);
    faddr   : std_logic_vector(OFFSETRANGE);
    acc     : sahb_acc_multi_type;
    acc_bsize : std_logic_vector(2 downto 0);
    retry         : std_logic;
    retry_addr    : std_logic_vector(31 downto 0);
    retry_blen    : std_logic_vector(15 downto 0);
    retry_noseq   : std_logic;
  end record;
  
  type buf_data_type is array (0 to 3) of std_logic_vector(CRAMDW-1 downto 0);
  type wbuf_reg_type is record
    pending : std_logic;
    rep_pen : std_logic;
    addr    : std_logic_vector(31 downto 0);
    wdvalid : std_logic_vector(VALID_BITS-1 downto 0);
    dirty   : std_logic_vector(VALID_BITS-1 downto 0);
    wdata   : buf_data_type;
    err     : std_logic;
    merr    : std_logic;
    size    : std_logic_vector(2 downto 0);
    master  : std_logic_vector(3 downto 0);
    wt_pen  : std_logic;
    wt_addr : std_logic_vector(31 downto 0);
    wt_wdata: std_logic_vector(CRAMDW-1 downto 0);
  end record;
  
  type fbuf_reg_type is record
    pending : std_logic_vector(1 downto 0);
    wayv    : std_logic_vector(3 downto 0);
    bypass  : std_logic;
    write   : std_logic;
    addr    : std_logic_vector(31 downto 0);
    ben     : std_logic_vector((CRAMDW/8)-1 downto 0);
    wdata   : std_logic_vector(CRAMDW-1 downto 0);
    wdvalid : std_logic;
    fetch   : std_logic_vector(VALID_BITS-1 downto 0);
    rdfetch : std_logic_vector(VALID_BITS-1 downto 0);
    fdata   : buf_data_type;
    fdvalid : std_logic_vector(VALID_BITS-1 downto 0);
    err     : std_logic;
    size    : std_logic_vector(2 downto 0);
    master  : std_logic_vector(3 downto 0);
  end record;


  type l2c_be_reg_type is record
    accid   : std_logic_vector(3 downto 0);
    accdone : std_logic;
    sa      : sahb_reg_type;
    fvalid  : std_logic;
    fdone   : std_logic;
    fdata   : std_logic_vector(CRAMDW-1 downto 0);
    fdata_offset : std_logic_vector(MAX_OFFSETRANGE);
    fbuf    : fbuf_reg_type;
    wbuf    : wbuf_reg_type;
  end record;
  
  signal r,rin : l2c_be_reg_type;
begin

  comb : process(rst, r, ptobe, fetobe, dmao)
    variable v       : l2c_be_reg_type;
    variable addsize : std_logic_vector(4 downto 0);
    variable asfetch : std_logic_vector(3 downto 0);
    variable bbussize : std_logic_vector(2 downto 0);
  begin
    v := r;

    if ptobe.fetch_valid /= "00" or ptobe.store_valid = '1' then
      v.accid         := ptobe.accid;
      --
      v.fbuf.pending  := ptobe.fetch_valid;
      v.fbuf.addr     := ptobe.new_addr;
      v.fbuf.size     := ptobe.size;
      v.fbuf.fetch    := ptobe.fetch_slice(VALIDRANGE);
      --
      v.fbuf.bypass   := '0';
      v.fbuf.rdfetch  := (others => '0');
      v.fbuf.master   := ptobe.master;
      v.fbuf.write    := '0';
      v.fbuf.ben      := (others => '1');
      --
      v.wbuf.pending  := ptobe.store_valid;
      v.wbuf.rep_pen  := ptobe.store_valid;
      v.wbuf.dirty    := ptobe.store_slice(VALIDRANGE);
      v.wbuf.wdvalid  := (others => '0');
      v.wbuf.wt_pen   := '0';
      v.wbuf.wt_addr  := (others => '0');
      v.wbuf.addr     := ptobe.old_addr;
      v.wbuf.size     := (others => '0');
      v.wbuf.master   := ptobe.master;
    end if;
    
    if ptobe.bp_valid = '1' then
      v.accid         := ptobe.accid;
      --  
      v.fbuf.addr     := ptobe.new_addr;
      v.fbuf.size     := ptobe.size;
      v.fbuf.fetch    := (others => '0');
      v.fbuf.rdfetch  := (others => '0');
      v.fbuf.master   := ptobe.master;
      --
      v.wbuf.rep_pen  := '0';
      v.wbuf.dirty    := (others => '0');
      v.wbuf.wdvalid  := (others => '0');
      --
      v.wbuf.wt_addr  := ptobe.new_addr;
      v.wbuf.wt_wdata  := ptobe.bp_data;
      v.wbuf.addr     := ptobe.old_addr;
      v.wbuf.size     := ptobe.size;
      v.wbuf.master   := ptobe.master;
      
      if ptobe.bp_write = '1' then
        v.fbuf.pending  := "00";
        v.fbuf.fetch(conv_integer(ptobe.new_addr(CLOFFSETRANGE))) := '1';
        --
        v.fbuf.bypass   := '1';
        v.fbuf.rdfetch(conv_integer(ptobe.new_addr(CLOFFSETRANGE))) := '1';
        v.fbuf.write    := '0';
        v.fbuf.ben      := (others => '1');
        --
        v.wbuf.pending  := '1';
        v.wbuf.wt_pen   := '1';
      else
        v.fbuf.pending  := "10";
        v.fbuf.fetch    := (others => '0');
        v.fbuf.fetch(conv_integer(ptobe.new_addr(CLOFFSETRANGE))) := '1';
        --
        v.fbuf.bypass   := '1';
        v.fbuf.rdfetch(conv_integer(ptobe.new_addr(CLOFFSETRANGE))) := '1';
        v.fbuf.write    := '0';
        v.fbuf.ben      := (others => '1');
        --
        v.wbuf.pending  := '0';
        v.wbuf.wt_pen   := '0';
      end if;
    end if;
  
    if ptobe.data_valid = '1' then
      v.wbuf.wdata(conv_integer(ptobe.data_offset(CLOFFSETRANGE))) := ptobe.data;
      v.wbuf.merr := r.wbuf.err or ptobe.data_err; 
      v.wbuf.err := '0';
      v.wbuf.wdvalid(conv_integer(ptobe.data_offset(CLOFFSETRANGE))) := '1';
    end if;

    if (r.fbuf.fetch = r.fbuf.fdvalid or r.fbuf.err = '1') and r.fbuf.pending /= "00" then
      v.fbuf.pending := (others => '0');
      v.fbuf.fdvalid := (others => '0');
      v.fbuf.err := '0';
    end if;

    betop.hold  <= orv(r.fbuf.pending&r.wbuf.pending);
    if r.fbuf.pending(0) = '1' then
      betop.data_valid <= r.fvalid;
    else
      betop.data_valid <= '0';
    end if;
    betop.data  <= r.fdata;
    betop.data_offset <= r.fdata_offset;
    betop.accid <= r.accid;

    betop.done <= r.fdone;

    if r.fbuf.pending(1) = '1' then
      betofe.data_valid <= r.fvalid;
    else
      betofe.data_valid <= '0';
    end if;
    betofe.data  <= r.fdata;
    betofe.data_offset <= r.fdata_offset;
    betofe.accid <= r.accid;
    
    if r.fbuf.pending(1) = '1' and v.fbuf.pending = "00" then
      betofe.data_done <= '1';
    else
      betofe.data_done <= '0';
    end if;

    
    ----------------------------------------------------------------------------
    -- Sec AHB master interface
    ----------------------------------------------------------------------------
    v.sa.dmai0.noreq := '0';
    
    --if r.sa.size = "100" then addsize := "10000";
    --elsif r.sa.size = "011" then addsize := "01000";
    --else addsize := "00100"; end if;
    if r.sa.acc_bsize = "100" then addsize := "10000";
    elsif r.sa.acc_bsize = "011" then addsize := "01000";
    else addsize := "00100"; end if;

    asfetch := (others => '0');
    asfetch(VALID_BITS-1 downto 0) := r.fbuf.fetch(VALID_BITS-1 downto 0) or r.fbuf.rdfetch(VALID_BITS-1 downto 0);
    asfetch(0) := asfetch(0);
    asfetch(1) := asfetch(1) or (asfetch(0) and orv(asfetch(3 downto 2)));
    asfetch(2) := asfetch(2) or (orv(asfetch(1 downto 0)) and asfetch(3));
    asfetch(3) := asfetch(3);
 
    if linesize /= 64 then asfetch(3 downto 2) := (others => '0'); end if;

    -- AHB master state machine
    case r.sa.state is
      when saidle =>
        v.sa.done := (others => '0');
        v.sa.stop := '0';
        v.sa.dmai0.req := '0';
        v.sa.dmai0.hburst := "001";
        v.sa.dma_hold := '0';
        v.sa.active := '0';
        v.sa.noreq := '0';

        v.sa.acc_bsize := r.sa.size; -- Default bus size

        if ptobe.cfg_besize /= r.sa.size then
          v.sa.size := ptobe.cfg_besize;
        elsif r.fbuf.pending /= "00" and r.fbuf.fdvalid = zero32(r.fbuf.fdvalid'range) then    -- Read
          v.sa.state := saread;

          if wbmask /= 16#FFFF# then -- The hole area is defined as "widebus" 
            if get_acc_widebus(r.fbuf.addr, wbmask) = '0' then v.sa.acc_bsize := "010"; end if; -- Wide-bus access not alowed in this address range, Use 32-bit
          end if;

          --v.sa.acc := set_acc(r.sa.size, r.fbuf.bypass, asfetch(VALID_BITS-1 downto 0), r.fbuf.size);
          v.sa.acc := set_acc(v.sa.acc_bsize, r.fbuf.bypass, asfetch(VALID_BITS-1 downto 0), r.fbuf.size);
          v.sa.blen := v.sa.acc(0).blen;
          
          v.sa.dmai0.req := '1';
          v.sa.dmai0.write := '0';
          v.sa.dmai0.hburst := v.sa.acc(0).hburst;
          v.sa.dmai0.size := v.sa.acc(0).bsize;
          v.sa.dmai0.addr := r.fbuf.addr(31 downto CLOFFSETRANGE'left+1) & v.sa.acc(0).offset & zero32(CLOFFSETRANGE'right-1 downto 0);
          if r.fbuf.bypass = '1' then 
            v.sa.dmai0.addr := r.fbuf.addr;
          end if;
          v.sa.faddr := v.sa.dmai0.addr(OFFSETRANGE);
        elsif r.wbuf.pending = '1' and                                                                                            -- Write pending
              ( (r.wbuf.rep_pen = '1' and ((r.wbuf.wdvalid xor r.wbuf.dirty) and r.wbuf.dirty) = zero32(r.wbuf.wdvalid'range)) or -- Replace data ready 
                (r.wbuf.wt_pen = '1')) then                                                                                       -- Direct (bypass/write-through) data ready
          
          if wbmask /= 16#FFFF# then -- The hole area is defined as "widebus" 
            if r.wbuf.wt_pen = '1' then
              if get_acc_widebus(r.wbuf.wt_addr, wbmask) = '0' then v.sa.acc_bsize := "010"; end if; -- Wide-bus access not alowed in this address range, Use 32-bit
            else
              if get_acc_widebus(r.wbuf.addr, wbmask) = '0' then v.sa.acc_bsize := "010"; end if; -- Wide-bus access not alowed in this address range, Use 32-bit
            end if;
          end if;

          --v.sa.acc := set_acc(r.sa.size, r.wbuf.wt_pen, r.wbuf.dirty, r.wbuf.size);
          v.sa.acc := set_acc(v.sa.acc_bsize, r.wbuf.wt_pen, r.wbuf.dirty, r.wbuf.size);
          v.sa.blen := v.sa.acc(0).blen;

          v.sa.dmai0.write := '1';
          v.sa.dmai0.hburst := v.sa.acc(0).hburst;
          v.sa.dmai0.size := v.sa.acc(0).bsize;
          v.sa.dmai0.addr := r.wbuf.addr(31 downto CLOFFSETRANGE'left+1) & v.sa.acc(0).offset & zero32(CLOFFSETRANGE'right-1 downto 0);
          if r.wbuf.wt_pen = '1' then
            v.sa.dmai0.addr := r.wbuf.wt_addr;
          end if;
          v.sa.faddr := v.sa.dmai0.addr(OFFSETRANGE);
          
          if r.wbuf.merr = '1' and r.wbuf.wt_pen = '0' then
            v.sa.done := (others => '1');
            v.wbuf.pending := '0';
            
            -- Uncorrectable data-error (Cache RMW (write))
            --v.err.pending(1) := '1'; if r.err.valid = '1' then v.err.multi := '1'; end if;
            --if r.err.valid = '0' then
            --  v.err.valid := '1'; v.err.addr := v.sa.dmai0.addr; v.err.hmaster := r.wbuf.master;
            --  v.err.cor_ucor := '1'; v.err.tag_data := '1'; v.err.cr_cw_f_mw_wp_ar_aw := "011";
            --  v.err.scrub := '0';
            --end if;
          else
            v.sa.state := sawrite;
            v.sa.dmai0.req := '1';
          end if;
          
          if r.wbuf.err = '1' and r.wbuf.merr = '0' and r.wbuf.wt_pen = '0' then
            -- Correctable data-error (Cache RMW (write))
            --v.err.pending(0) := '1'; if r.err.valid = '1' then v.err.multi := '1'; end if;
            --v.err.cor_cnt := r.err.cor_cnt + 1;
            --if r.err.valid = '0' then
            --  v.err.valid := '1'; v.err.addr := v.sa.dmai0.addr; v.err.hmaster := r.wbuf.master;
            --  v.err.cor_ucor := '0'; v.err.tag_data := '1'; v.err.cr_cw_f_mw_wp_ar_aw := "011";
            --  v.err.scrub := '0';
            --end if;
          end if;
        end if;

      when saread =>
        if dmao.grant = '1' then
          v.sa.active := '1';
          v.sa.dmai0.addr := r.sa.dmai0.addr + addsize;
          v.sa.blen := r.sa.blen(r.sa.blen'left-1 downto 0) & "0";

          if r.sa.blen(r.sa.blen'left-1) = '0' then
            v.sa.dmai0.req := '0';
            v.sa.done(1) := '1';
          end if;
        elsif dmao.retry = '1' then
          v.sa.dmai0.req := '1';
          v.sa.dmai0.addr := r.sa.dmai0.addr - addsize;
          v.sa.blen := "1" & r.sa.blen(r.sa.blen'left downto 1);
          v.sa.done(1) := '0';
          
          if r.sa.dmai0.hburst /= "000" then v.sa.dmai0.hburst := "001"; end if;
        end if;
        
        if dmao.ready = '1' then 
          if dmao.grant = '0' then v.sa.active := '0'; end if;
          v.sa.faddr := r.sa.faddr + addsize;

          if r.sa.done(1) = '1' then -- Access done
            v.sa.state := saidle;
            v.sa.done(0) := '1';
            v.fbuf.fdvalid(conv_integer(r.sa.faddr(CLOFFSETRANGE))) := '1';
            v.sa.acc(0).valid := '0';
            v.sa.acc(0).blen := (others => '0');
            v.sa.acc(0).hburst := (others => '0');
            v.sa.acc(0).offset := (others => '0');
            v.sa.acc(0).bsize := (others => '0');
          end if;

          --if r.sa.size = "100" then -- 128-bit
          if r.sa.acc_bsize = "100" then -- 128-bit
            v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(127 downto 0) := dmao.rdata128(127 downto 0); 
            v.fbuf.fdvalid(conv_integer(r.sa.faddr(CLOFFSETRANGE))) := '1';
          --elsif r.sa.size = "011" then -- 64-bit
          elsif r.sa.acc_bsize = "011" then -- 64-bit
            if r.sa.faddr(3) = '0' then
              v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(127 downto 64) := dmao.rdata128(127 downto 64);
            else
              v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(63 downto 0) := dmao.rdata128(63 downto 0); 
              v.fbuf.fdvalid(conv_integer(r.sa.faddr(CLOFFSETRANGE))) := '1';
            end if;
          --else --if r.sa.size = "010" then -- 32-bit
          else --if r.sa.acc_bsize = "010" then -- 32-bit
            case r.sa.faddr(3 downto 2) is
              when "00" => v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(127 downto 96) := dmao.rdata128(127 downto 96);
              when "01" => v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(95 downto 64) := dmao.rdata128(95 downto 64);
              when "10" => v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(63 downto 32) := dmao.rdata128(63 downto 32);
              when others => v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)))(31 downto 0) := dmao.rdata128(31 downto 0); 
                             v.fbuf.fdvalid(conv_integer(r.sa.faddr(CLOFFSETRANGE))) := '1';
            end case;
          end if;

        elsif dmao.error = '1' then
          v.sa.active := '0';
          v.sa.dmai0.req := '0';
          v.sa.done(0) := '1';
          v.sa.state := saidle;
          v.fbuf.err := '1';
          v.fbuf.fdvalid := (others => '1');
          v.sa.acc(0).valid := '0';
          v.sa.acc(0).blen := (others => '0');
          v.sa.acc(0).hburst := (others => '0');
          v.sa.acc(0).offset := (others => '0');
          v.sa.acc(0).bsize := (others => '0');

          -- Backend Read error
          --v.err.pending(3) := '1'; if r.err.valid = '1' then v.err.multi := '1'; end if;
          --if r.err.valid = '0' or r.err.cor_ucor = '0' then
          --  v.err.valid := '1'; v.err.addr := r.sa.dmai0.addr; v.err.hmaster := r.fbuf.master;
          --  v.err.cor_ucor := '1'; v.err.tag_data := '1'; v.err.cr_cw_f_mw_wp_ar_aw := "101";
          --  v.err.scrub := '0';
          --end if;
        end if;
      when sawrite =>

        if dmao.grant = '1' then
          v.sa.active := '1';
          v.sa.retry := '0';
          v.sa.dmai0.noseq := '0';
          v.sa.dmai0.addr := r.sa.dmai0.addr + addsize;
        
          v.sa.blen := r.sa.blen(r.sa.blen'left-1 downto 0) & "0";
          
          if r.sa.blen(r.sa.blen'left-1) = '0' and (r.sa.retry = '0' or r.sa.retry_blen(r.sa.retry_blen'left-1) = '0') then
            if r.sa.acc(1).valid = '1' then
              v.sa.acc(0) := r.sa.acc(1);
              v.sa.acc(1).valid := '0';
              v.sa.acc(1).blen := (others => '0');
              v.sa.acc(1).hburst := (others => '0');
              v.sa.acc(1).offset := (others => '0');
              v.sa.acc(1).bsize := (others => '0');
              v.sa.blen := r.sa.acc(1).blen;
              v.sa.dmai0.noseq := '1';
              v.sa.dmai0.addr(OFFSETRANGE) := r.sa.acc(1).offset & zero32(CLOFFSETRANGE'right-1 downto 0);
              v.sa.dmai0.hburst := r.sa.acc(1).hburst;
              v.sa.dmai0.size := r.sa.acc(1).bsize;
            else
              v.sa.dmai0.req := '0';
              v.sa.done(1) := '1';
              v.sa.acc(0).valid := '0';
            end if;
          end if;
          
          if r.wbuf.merr = '1' and r.wbuf.wt_pen = '0' then
            v.sa.dmai0.req := '0';
            v.sa.done(1) := '1';
            v.sa.acc(0).valid := '0';
            v.sa.blen := (others => '0');
            
            -- Uncorrectable data-error (Cache RMW (write))
            --v.err.pending(0) := '1';
            --if r.err.valid = '0' or r.err.cor_ucor = '0' then
            --  v.err.valid := '1'; v.err.addr := v.sa.dmai0.addr; v.err.hmaster := r.wbuf.master;
            --  v.err.cor_ucor := '1'; v.err.tag_data := '1'; v.err.cr_cw_f_mw_wp_ar_aw := "011";
            --  v.err.scrub := '0';
            --end if;
          end if;
          
          if r.wbuf.err = '1' and r.wbuf.merr = '0' and r.wbuf.wt_pen = '0' then
            -- Correctable data-error (Cache RMW (write))
            --v.err.pending(0) := '1';
            --if r.err.valid = '0' then
            --  v.err.valid := '1'; v.err.addr := v.sa.dmai0.addr; v.err.hmaster := r.wbuf.master;
            --  v.err.cor_ucor := '0'; v.err.tag_data := '1'; v.err.cr_cw_f_mw_wp_ar_aw := "011";
            --  v.err.scrub := '0';
            --end if;
          end if;

          -- Save blen and offset for retry
          v.sa.retry_blen := r.sa.blen;
          v.sa.retry_addr := r.sa.dmai0.addr;
          v.sa.retry_noseq := r.sa.dmai0.noseq;
          
          -- Restore blen and offset for retry
          if r.sa.retry = '1' then
            v.sa.blen := r.sa.retry_blen;
            v.sa.dmai0.addr := r.sa.retry_addr;
            v.sa.dmai0.noseq := r.sa.retry_noseq;
          end if;
        elsif dmao.retry = '1' then
          v.sa.dmai0.req := '1';
          v.sa.done(1) := '0';
          
          if r.sa.dmai0.hburst /= "000" then v.sa.dmai0.hburst := "001"; end if;

          v.sa.retry := '1';
          
          -- Save blen and offset for retry
          v.sa.retry_blen := r.sa.blen;
          v.sa.retry_addr := r.sa.dmai0.addr;
          v.sa.retry_noseq := r.sa.dmai0.noseq;
          
          -- Restore blen and offset for retry
          v.sa.blen := r.sa.retry_blen;
          v.sa.dmai0.addr := r.sa.retry_addr;
          v.sa.dmai0.noseq := r.sa.retry_noseq;
        end if;

        if dmao.ready = '1' then 
          if dmao.grant = '0' and r.sa.dmai0.req = '0' then v.sa.active := '0'; end if;
          
          v.sa.faddr := r.sa.dmai0.addr(OFFSETRANGE);

          if r.sa.done(1) = '1' then -- Access done
            v.sa.state := saidle;
            v.sa.done(0) := '1';
            if r.wbuf.wt_pen = '1' and r.wbuf.rep_pen = '1' then
              v.wbuf.wt_pen := '0';
            else
              v.wbuf.pending := '0';
            end if;
          end if;


        elsif dmao.error = '1' then 
          v.sa.active := '0';
          v.sa.dmai0.req := '0';
          v.sa.done(0) := '1';
          v.sa.state := saidle;
          
          if r.wbuf.wt_pen = '1' and r.wbuf.rep_pen = '1' then
            v.wbuf.wt_pen := '0';
          else
            v.wbuf.pending := '0';
          end if;

          -- Backend write error
          --v.err.pending(3) := '1'; if r.err.valid = '1' then v.err.multi := '1'; end if;
          --if r.err.valid = '0' or r.err.cor_ucor = '0' then
          --  v.err.valid := '1'; v.err.addr := r.sa.dmai0.addr; v.err.hmaster := r.wbuf.master;
          --  v.err.cor_ucor := '1'; v.err.tag_data := '1'; v.err.cr_cw_f_mw_wp_ar_aw := "110";
          --  v.err.scrub := '0';
          --end if;
        end if;

        if r.wbuf.wt_pen = '1' then
          v.sa.dmai0.wdata128 := r.wbuf.wt_wdata;
        else
          v.sa.dmai0.wdata128 := r.wbuf.wdata(conv_integer(v.sa.faddr(CLOFFSETRANGE)));
        end if;
        
        --if r.sa.size = "100" then -- 128-bit
        if r.sa.acc_bsize = "100" then -- 128-bit
        --elsif r.sa.size = "011" then -- 64-bit
        elsif r.sa.acc_bsize = "011" then -- 64-bit
          if v.sa.faddr(3) = '0' then v.sa.dmai0.wdata128(63 downto 0) := v.sa.dmai0.wdata128(127 downto 64);
          else v.sa.dmai0.wdata128(127 downto 64) := v.sa.dmai0.wdata128(63 downto 0); end if;
        --else --if r.sa.size = "010" then -- 32-bit
        else --if r.sa.acc_bsize = "010" then -- 32-bit
          case v.sa.faddr(3 downto 2) is
            when "00" => v.sa.dmai0.wdata128 := v.sa.dmai0.wdata128(127 downto 96) & v.sa.dmai0.wdata128(127 downto 96) & 
                                                v.sa.dmai0.wdata128(127 downto 96) & v.sa.dmai0.wdata128(127 downto 96); 
            when "01" => v.sa.dmai0.wdata128 := v.sa.dmai0.wdata128(95 downto 64) & v.sa.dmai0.wdata128(95 downto 64) &
                                                v.sa.dmai0.wdata128(95 downto 64) & v.sa.dmai0.wdata128(95 downto 64);
            when "10" => v.sa.dmai0.wdata128 := v.sa.dmai0.wdata128(63 downto 32) & v.sa.dmai0.wdata128(63 downto 32) &
                                                v.sa.dmai0.wdata128(63 downto 32) & v.sa.dmai0.wdata128(63 downto 32);
            when others => v.sa.dmai0.wdata128 := v.sa.dmai0.wdata128(31 downto 0) & v.sa.dmai0.wdata128(31 downto 0) &
                                                  v.sa.dmai0.wdata128(31 downto 0) & v.sa.dmai0.wdata128(31 downto 0);
          end case;
        end if;
      when others =>
    end case;
          
    v.fvalid := '0';    
    if v.fbuf.fdvalid(conv_integer(r.sa.faddr(CLOFFSETRANGE))) = '1' and 
       r.fbuf.fdvalid(conv_integer(r.sa.faddr(CLOFFSETRANGE))) = '0' then
      v.fvalid := '1';    
      v.fdata := v.fbuf.fdata(conv_integer(r.sa.faddr(CLOFFSETRANGE)));
      v.fdata_offset := (others => '0');
      v.fdata_offset(CLOFFSETRANGE) := r.sa.faddr(CLOFFSETRANGE);
    end if;
    v.fdone := '0';
    if (r.fbuf.pending /= "00" and v.fbuf.pending = "00" and r.wbuf.pending = '0') or
       (r.wbuf.pending = '1' and v.wbuf.pending = '0' and r.fbuf.pending = "00") then
      v.fdone := '1';
    end if;




  -- Reset --------------------------------------------------------------------------
    if rst = '0' then
      v.sa.state := saidle;
      v.sa.dmai0.addr := (others => '0');
      v.sa.dmai0.write := '0';
      v.sa.dmai0.size := (others => '0');
      v.sa.done := (others => '0');
      v.sa.stop := '0';
      v.sa.dmai0.req := '0';
      v.sa.dmai0.noseq := '0';
      v.sa.active := '0';
      v.sa.noreq := '0';
      v.sa.retry := '0';
      v.sa.updsize := (others => '0');
      if bbuswidth = 32 then
        v.sa.size := "010";
      elsif bbuswidth = 64 then
        v.sa.size := "011";
      else --if bbuswidth = 128 then
        v.sa.size := "100";
      end if;
    
      v.fbuf.pending := (others => '0');
      v.fbuf.fdvalid := (others => '0');
      v.fbuf.err := '0'; 
      v.wbuf.pending := '0';
      v.wbuf.wdvalid := (others => '0');
      v.wbuf.err := '0'; 
      v.wbuf.merr := '0';

    end if;
  
    rin <= v;

    -- Output to pipe
    --betop.valid <= r.dvalid;
    --betop.accid <= r.accid;
    --betop.rdata <= r.rdata;
  
    -- Output to AHB-master
    dmai <= r.sa.dmai0;

  end process comb;

  regs : process(clk)
  begin
    if rising_edge(clk) then r <= rin; end if;
  end process regs;
end;



