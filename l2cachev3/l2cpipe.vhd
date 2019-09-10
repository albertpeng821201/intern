------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-----------------------------------------------------------------------------   
-- Entity:      l2cpipe
-- File:        l2cpipe.vhd
-- Author:      Nils Johan Wessman - Aeroflex Gaisler
-- Description: L2-cache PIPE
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

entity l2cpipe is
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
    fetop   : in  l2c_fe_to_pipe_type;
    ptofe   : out l2c_pipe_to_fe_type;
    ptobe   : out l2c_pipe_to_be_type;
    betop   : in  l2c_be_to_pipe_type;
    cramo   : in  l2cram_out_type;
    crami   : out l2cram_in_type
);
end;

architecture rtl of l2cpipe is

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
  subtype MTRRANGE is natural range 31 downto 18;

  constant TAG_BITS   : integer := TAG_HIGH+1 - (10+log2(waysize));
  constant LRU_BITS   : integer := l2c_lru_table(ways);
  constant VALID_BITS : integer := linesize/16;
  constant IOCONFBIT  : natural := 10 + 9; -- 512 bit(19)
  constant IODATABIT  : natural := 10 + 9 + 2; -- 512 bit(21), way=bit(20:19)
  subtype IODATAWAYRANGE is natural range IODATABIT-1 downto IODATABIT-2; -- way=bit(20:19)

  constant TDEPTH   : integer := 10+log2(waysize) - log2(linesize);
  
  type tag_edacdec_vector_type is array (0 to 3) of l2c_edacdectype;
  type tag_way_out_type is record 
    tag     : std_logic_vector(TAGRANGE);
    cb      : std_logic_vector(MAX_TAGCBRANGE);
    valid   : std_logic_vector(VALIDRANGE);
    dirty   : std_logic_vector(DIRTYRANGE);
  end record;
  --type tag_way_out_vector_type is array (0 to WAYRANGE'left) of tag_way_out_type;
  type tag_way_out_vector_type is array (MAX_WAYRANGE) of tag_way_out_type;
  type tag_out_type is record
    way : tag_way_out_vector_type;
    lru : std_logic_vector(LRURANGE);
  end record;
  type tag_edac_error_out_vector_type is record
    err     : std_logic;
    merr    : std_logic;
    decout  : tag_edacdec_vector_type;
  end record;
  
  type data_edac_error_out_type is record
    valid   : std_logic;
    err     : std_logic;
    merr    : std_logic;
    refetch : std_logic;
    data    : std_logic_vector(MAX_DATARANGE);
    cloffset: std_logic_vector(OFFSETRANGE);
    addr    : std_logic_vector(MAX_ADDRRANGE);
    wayv    : std_logic_vector(WAYRANGE);
  end record;
  constant unused_data_edac_error_out : data_edac_error_out_type := ('0', '0', '0', '0', (others => '0'), 
                                                                     (others => '0'), (others => '0'), (others => '0'));
  type data_edac_error_out_vector_type is array (0 to 3) of data_edac_error_out_type;
 
  type l2c_pipe_acc_flush_type is record
    pending : std_logic;
    lookup  : std_logic;
    fetch   : std_logic;
    tag_upd : std_logic;
    inv     : std_logic;
    wb      : std_logic;
    valid   : std_logic;
    dirty   : std_logic;
    wayv    : std_logic_vector(MAX_WAYRANGE);
  end record;
  constant l2c_pipe_acc_flush_none : l2c_pipe_acc_flush_type := (
   '0', '0', '0', '0', '0', '0', '0', '0',(others => '0'));
  
  
  type l2c_pipe_flush_type is record
    pending : std_logic;
    addr    : std_logic_vector(31 downto 0);
  end record;
  constant l2c_pipe_flush_none : l2c_pipe_flush_type := (
    '0',(others => '0'));
  
  type mtrr_arr_type is array (0 to 31) of std_logic_vector(31 downto 0);
  type flush_reg_type is record
    pending : std_logic;
    way     : integer range 0 to WAYRANGE'left;
    mode    : std_logic_vector(3 downto 0);
    addr    : std_logic_vector(31 downto 0);
    valid   : std_logic;
    dirty   : std_logic;
    fetch   : std_logic;
    rst_addr: std_logic;
  end record;
  type scrub_type is record
    en      : std_logic;
    pending : std_logic;
    wayv    : std_logic_vector(WAYRANGE);
    index   : std_logic_vector(INDEXRANGE);
    delay   : std_logic_vector(15 downto 0);
  end record;
  type error_ctrl is record
    rst           : std_logic;
    comp          : std_logic;
    diseresp      : std_logic;
    mask          : std_logic_vector(3 downto 0);
    inject_addr   : std_logic_vector(31 downto 2);
    inject_pending: std_logic;
  end record;
  type l2c_config_type is record
    cen         : std_logic;                                -- config: cache enable
    hproten     : std_logic;                                -- config: hprot enable
    hpbufen     : std_logic;
    hprhbypass  : std_logic;
    wp          : std_logic;                                -- config: write policy (0: copy-back, 1: write-through)
    edacen      : std_logic;
    rcb         : std_logic;
    xcb         : std_logic;
    selectcb    : std_logic_vector(1 downto 0);
    wcb         : std_logic_vector(27 downto 0);
    selecttcb   : std_logic_vector(1 downto 0);
    wtcb        : std_logic_vector(6 downto 0);
    err         : error_ctrl;
    stat_mode   : std_logic_vector(1 downto 0);             -- Access/Hit counter mode 
    
    lock        : std_logic_vector(3 downto 0);
  
    repl        : std_logic_vector(1 downto 0);
    index_rep   : std_logic_vector(3 downto 0);
    flush       : flush_reg_type;                           -- Cache flush
    mtr         : mtrr_arr_type;
    scrub       : scrub_type;                               -- Cache scrub
    besize      : std_logic_vector(2 downto 0);
    updbesize   : std_logic_vector(2 downto 0);
    split       : std_logic_vector(1 downto 0);
  end record;
  type stat_count_type is array (0 to 1) of std_logic_vector(31 downto 0);
  type stat_type is record
    ccount      : stat_count_type;                          -- clock counter
    ucount      : stat_count_type;                          -- bus usage counter
    acount      : stat_count_type;                          -- Access counter
    hcount      : stat_count_type;                          -- Hit counter
  end record;
  type error_status is record                                                                         
    valid         : std_logic;
    multi         : std_logic;
    addr          : std_logic_vector(31 downto 0);
    cr_cw_f_mw_wp_ar_aw : std_logic_vector(2 downto 0); -- 000: read, 001: cache write, 010: fetch, 011: mem write, 100: write-protection
    cor_ucor      : std_logic; -- 0: signle, 1: multi
    tag_data      : std_logic; -- 0: tag, 1: data
    output        : std_logic_vector(1 downto 0); -- bit[1]: multi, bit[0]: signle
    pending       : std_logic_vector(3 downto 0); -- write-protection, uncorrectable, correctable
    cor_cnt       : std_logic_vector(2 downto 0);
    hmaster       : std_logic_vector(3 downto 0);
    scrub         : std_logic;
  end record;

  
  type l2c_pipe_state_type is record
    acc     : l2c_access_type;
    
    -- type
    flush   : l2c_pipe_acc_flush_type;
    scrub   : std_logic;
    inject  : std_logic;
    diagt   : std_logic;
    diagd   : std_logic;
    reg     : std_logic;
    rmw     : std_logic;
  end record;
  

  -- PIPE registers
  type l2c_pipe_tag_lookup_type is record
    tag         : tag_way_out_type; 
    lru         : std_logic_vector(LRURANGE);
    hitv        : std_logic_vector(MAX_WAYRANGE);
    hit         : std_logic;
    wayv        : std_logic_vector(MAX_WAYRANGE);
    way         : integer range 0 to WAYRANGE'left;
    lock_bypass : std_logic;
    lock_hit    : std_logic;
  end record;
  
  type l2c_pipe_tag_vd_bits_type is record 
    valid   : std_logic_vector(VALIDRANGE);
    dirty   : std_logic_vector(DIRTYRANGE);
  end record;
  type l2c_pipe_tag_vd_bits_vector_type is array (MAX_WAYRANGE) of l2c_pipe_tag_vd_bits_type;
  type l2c_pipe_tag_update_type is record
    tag     : std_logic_vector(TAGRANGE);
    cb      : std_logic_vector(MAX_TAGCBRANGE);
    index   : std_logic_vector(INDEXRANGE);
    tag_upd : std_logic;
    tag_wayv: std_logic_vector(MAX_WAYRANGE);
    lru     : std_logic_vector(LRURANGE);
    vd_bits : l2c_pipe_tag_vd_bits_vector_type;
    vd_wayv : std_logic_vector(MAX_WAYRANGE);
    vdl_upd : std_logic;
    wayv    : std_logic_vector(MAX_WAYRANGE);
    way     : integer range 0 to WAYRANGE'left;
  end record;
  
  type fetch_acc_data_type is array (0 to 3) of std_logic_vector(CRAMDW-1 downto 0); 
  type fetch_acc_reg_type is record
    valid   : std_logic;
    done    : std_logic;
    tag     : std_logic_vector(TAGRANGE);
    index   : std_logic_vector(INDEXRANGE);
    offset  : std_logic_vector(OFFSETRANGE);
    wayv    : std_logic_vector(MAX_WAYRANGE);
    --
    accid   : std_logic_vector(3 downto 0);
    --
    pending : std_logic_vector(3 downto 0);
    rdindex : std_logic_vector(3 downto 0);
    wdindex : std_logic_vector(3 downto 0);
    wdata   : fetch_acc_data_type;
    --
    merge_slice : std_logic_vector(3 downto 0);
    merge_data  : std_logic_vector(DATARANGE);
    merge_ben   : std_logic_vector(BENRANGE);
  end record;

  type drw_reg_type is record
    pending : std_logic_vector(3 downto 0);
    wr      : std_logic_vector(3 downto 0);
    mwrite  : std_logic;
    addr    : std_logic_vector(31 downto 0);
    --roffset : std_logic_vector(CLOFFSETRANGE);
    wayv    : std_logic_vector(MAX_WAYRANGE);
    wdata   : std_logic_vector(CRAMDW-1 downto 0);
    ben     : std_logic_vector((CRAMDW/8)-1 downto 0);
    scrub   : std_logic;
    inject  : std_logic;
    dvalid  : std_logic;
    err     : std_logic;
    merr    : std_logic;
  end record;

  type l2c_pipe_data_acc_type is record
    wayv  : std_logic_vector(MAX_WAYRANGE);
    way   : std_logic_vector(DATAWAYRANGE);
    index : std_logic_vector(INDEXRANGE);
    offset: std_logic_vector(CLOFFSETRANGE);
    data  : std_logic_vector(DATARANGE);
    cb    : std_logic_vector(DATACBRANGE);
    ben   : std_logic_vector(BENRANGE);
    wen   : std_logic_vector(BENRANGE);
  end record;

  type p5_reg_type is record
    state : l2c_pipe_state_type;
    
    --valid   : std_logic;
    --err     : std_logic;
    --retry   : std_logic;
    --tag_valid : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    --haddr   : std_logic_vector(31 downto 0);
    --hrdata  : std_logic_vector(CRAMDW-1 downto 0);
    --rdvalid : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    --derr    : data_edac_error_out_type;
    --rdata   : data_edac_error_out_vector_type;
    --master  : std_logic_vector(3 downto 0);
    --bypass  : std_logic;
    --scrub   : std_logic;
    --rmw     : std_logic;
    --mwrite  : std_logic;
  end record;
  type p4_reg_type is record
    state       : l2c_pipe_state_type;
    data        : std_logic_vector(DATARANGE);
    data_slice  : std_logic_vector(VALIDRANGE);
    data_done   : std_logic;
    fe_dvalid   : std_logic;

    --valid     : std_logic;
    --newacc    : std_logic;
    --tag_valid : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    --tag_dirty : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    --addr      : std_logic_vector(31 downto 0);
    --wayv      : std_logic_vector(3 downto 0);
    --rdata     : dram_out_type;
    --rdvalid   : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    --mwvalid   : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    --rmw     : std_logic;
    --master  : std_logic_vector(3 downto 0);
    --diagd   : std_logic;
    --diagt   : std_logic;
    --reg     : std_logic;
    --bypass  : std_logic;
    --scrub   : std_logic;
  end record;
  type p3_reg_type is record
    state       : l2c_pipe_state_type;
    data        : std_logic_vector(DATARANGE);
    data_slice  : std_logic_vector(VALIDRANGE);
    fe_dvalid   : std_logic;
    tagrdata    : std_logic_vector(DATARANGE);
    
    --valid     : std_logic;
    --newacc    : std_logic;
    --addr      : std_logic_vector(31 downto 0);
    --write     : std_logic;
    --tag       : tag_way_out_type;
    --newtag    : tag_way_out_type;
    --lru       : std_logic_vector(MAX_LRUBITS-1 downto 0);
    --twen      : std_logic_vector(MAX_WAYS-1 downto 0);
    --vdlwen    : std_logic;
    --hit       : std_logic;
    --bypass    : std_logic;
    --mfetch    : std_logic;
    --mwrite    : std_logic;
    --way       : integer range 0 to MAX_WAYS-1;
    --hitv      : std_logic_vector(3 downto 0);
    --wayv      : std_logic_vector(3 downto 0);
    --wayvtag   : std_logic_vector(3 downto 0);
    --ben       : std_logic_vector((CRAMDW/8)-1 downto 0);
    --wen       : std_logic_vector((CRAMDW/8)-1 downto 0);
    --wdata     : std_logic_vector(CRAMDW-1 downto 0);
    --cb        : std_logic_vector((CRAMDW/(8*4)*7)-1 downto 0);
    --drw       : drw_reg_type;
    --terr      : tag_edac_error_out_vector_type;
    --diagd     : std_logic;
    --diagt     : std_logic;
    --reg       : std_logic;
    --rmw       : std_logic;
    --fmode     : std_logic_vector(6 downto 0);
    --wt        : std_logic;
    --master    : std_logic_vector(3 downto 0);
    --hprot     : std_logic_vector(1 downto 0);
    --hpcache   : std_logic;
    --hpbypass  : std_logic;
    --hpwt      : std_logic;
    --tagrdata: std_logic_vector(CRAMDW-1 downto 0);
    --scrub   : std_logic;
  end record;
  
  type p2_reg_type is record
    state   : l2c_pipe_state_type;
    tlookup : l2c_pipe_tag_lookup_type;
    newtag  : l2c_pipe_tag_update_type;
    dacc    : l2c_pipe_data_acc_type;
    terr    : tag_edac_error_out_vector_type;
    flushed : std_logic_vector(MAX_WAYRANGE);
    drw     : drw_reg_type;
    facc    : fetch_acc_reg_type;
    dread   : std_logic;            
    tagrdata: std_logic_vector(DATARANGE);
    --hold    : std_logic;
    --haddr   : std_logic_vector(31 downto 0);
  end record;
  type p1_reg_type is record
    state     : l2c_pipe_state_type;
    wdata     : std_logic_vector(DATARANGE);
    tags      : tag_out_type;
    deltags   : tag_out_type;
    deltvalid : std_logic;
    flush     : l2c_pipe_acc_flush_type; 
    wen       : std_logic_vector((CRAMDW/8)-1 downto 0);
    bypass    : std_logic;
    wt        : std_logic;
    wprot     : std_logic;
    --valid   : std_logic;
    --addr    : std_logic_vector(31 downto 0);
    --size    : std_logic_vector(2 downto 0);
    --master  : std_logic_vector(3 downto 0);
    --hprot   : std_logic_vector(1 downto 0);
    --mtrr    : std_logic_vector(3 downto 0);
    --write   : std_logic;
    --hwdata  : std_logic_vector(CRAMDW-1 downto 0);
    --ben     : std_logic_vector((CRAMDW/8)-1 downto 0);
    --rmw     : std_logic;
    --reg     : std_logic;
    --diagd   : std_logic;
    --diagt   : std_logic;
    --fmode   : std_logic_vector(6 downto 0);
    --scrub   : std_logic;
    --inject  : std_logic;
  end record;
  type p0_reg_type is record
    state         : l2c_pipe_state_type;
    delstate      : l2c_pipe_state_type;
    delaccdata    : std_logic_vector(127 downto 0);
    flush         : l2c_pipe_flush_type;
    --valid   : std_logic;
    --tren    : std_logic;
    --haddr   : std_logic_vector(31 downto 0);
    --hwrite  : std_logic;
    --hsize   : std_logic_vector(2 downto 0);
    --hmbsel  : std_logic_vector(0 to 2);
    --hmaster : std_logic_vector(3 downto 0);
    --hprot   : std_logic_vector(3 downto 0);
    --fmode   : std_logic_vector(6 downto 0);
    --delacc  : delacc_reg_type;
    --scrub   : std_logic;
    --inject  : std_logic;
  end record;
  type l2c_pipe_reg_type is record
    p5    : p5_reg_type;
    p4    : p4_reg_type;
    p3    : p3_reg_type;
    p2    : p2_reg_type;
    p1    : p1_reg_type;
    p0    : p0_reg_type;
    cfg   : l2c_config_type;
    stat  : stat_type;
    err   : error_status;
    prand : integer range 0 to WAYRANGE'left;
  end record;

  signal r,rin : l2c_pipe_reg_type;

  -- TAG functions
  function get_tags(cramo : in  l2cram_out_type) return tag_out_type is
    variable tags : tag_out_type;
  begin
    for i in WAYRANGE loop
      tags.way(i).tag(TAGRANGE)          := cramo.way(i).tag(TAGRANGE);
      tags.way(i).cb(MAX_TAGCBRANGE)    := cramo.way(i).tcb(MAX_TAGCBRANGE);
      tags.way(i).valid(VALIDRANGE)  := cramo.way(i).valid(VALIDRANGE);
      tags.way(i).dirty(VALIDRANGE)  := cramo.way(i).dirty(VALIDRANGE);
    end loop;
      tags.lru(LRURANGE) := cramo.lru(LRURANGE);
    return tags;
  end function;


  -- TAG lookup
  procedure tag_comp( addr  : in std_logic_vector(31 downto 0); 
                      tags  : in tag_out_type;
                      lock  : in std_logic_vector(3 downto 0);
                      fmode : in std_logic_vector(3 downto 0);
                      repl  : in std_logic_vector(1 downto 0);
                      prand     : in integer range 0 to ways-1;
                      hmaster   : in std_logic_vector(3 downto 0);
                      index_rep : in std_logic_vector(3 downto 0);
                      tag   : out tag_way_out_type; 
                      lru   : out std_logic_vector(LRURANGE);
                      hitv  : out std_logic_vector(MAX_WAYRANGE);
                      hit   : out std_logic;
                      wayv  : out std_logic_vector(MAX_WAYRANGE);
                      way   : out integer range 0 to WAYS-1;
                      lock_bypass : out std_logic;
                      lock_hit    : out std_logic) is
    variable hitv_tmp : std_logic_vector(MAX_WAYRANGE);
    variable way_tmp  : integer range 0 to WAYS-1;
    variable llock_hit : std_logic;
  begin
    hitv_tmp := (others => '0'); wayv := (others => '0');
    for i in 0 to WAYS-1 loop
      if addr(TAGRANGE) = tags.way(i).tag(TAGRANGE) and orv(tags.way(i).valid) = '1' and 
         (fmode(3) = '0' or i < WAYS-conv_integer(lock)) then
        hitv_tmp(i) := '1'; 
        wayv(i) := '1'; 
        way_tmp := i;
        tag := tags.way(i);
        tag.dirty := tags.way(i).valid and tags.way(i).dirty;
      end if;
    end loop;
      
    if orv(hitv_tmp) = '0' then
      if repl = "00" then
        way_tmp := conv_integer(lru_way(tags.lru, (WAYS-conv_integer(lock))));
      elsif repl = "01" then
        way_tmp := prand;
      elsif repl = "10" then
        if conv_integer(hmaster) < WAYS-conv_integer(lock) then
          way_tmp := conv_integer(hmaster);
        else
          way_tmp := conv_integer(index_rep);
        end if;
      else -- repl = "11" then
        if WAYS /= 1 then
          way_tmp := conv_integer(hmaster(log2x(WAYS)-1 downto 0));
        else
          way_tmp := 0;
        end if;
      end if;
      
      wayv(way_tmp) := '1';
      tag := tags.way(way_tmp);
      tag.dirty := tags.way(way_tmp).valid and tags.way(way_tmp).dirty;
    end if;
    
    hit := orv(hitv_tmp);
    hitv := hitv_tmp;
    way := way_tmp;
    
    lock_bypass := '0'; llock_hit := '0';
    if lock /= zero32(lock'range) then
      for i in 0 to WAYS-1 loop
        if hitv_tmp(i) = '1' and i >= WAYS-conv_integer(lock) then
          llock_hit := '1';
          lru := tags.lru;
        end if;
      end loop;
      --if conv_integer(lock) = WAYS and orv(hitv_tmp) = '0' then 
      if conv_integer(lock) = WAYS and orv(hitv_tmp) = '0' and fmode = zero32(fmode'range) then 
        lock_bypass := '1';
      end if; 
    end if;
    lock_hit := llock_hit;
  
    if llock_hit = '0' then
      if repl = "00" then           -- LRU
        lru := lru_calc(tags.lru, way_tmp, (WAYS-conv_integer(lock)));
      else
        lru := (others => '0');
      end if;
    end if;
  
  end procedure;
  function check_tags(tags        : in  tag_out_type; 
                      bypassedac  : in  std_logic;
                      ft          : in  integer) return tag_edac_error_out_vector_type is
    variable terr : tag_edac_error_out_vector_type;
  begin
    terr.err := '0'; terr.merr := '0';
    if ft /= 0 then -- EDAC implemented
      for i in 0 to WAYS-1 loop
        terr.decout(i) := l2c_edacdecode2(tags.way(i).tag&zero32(TAGRANGE'right-1 downto 0), tags.way(i).cb);   --decoder
        terr.err := terr.err or (orv(tags.way(i).valid) and terr.decout(i).err);
        terr.merr := terr.merr or (orv(tags.way(i).valid) and terr.decout(i).merr);
        
        if bypassedac = '1' then
          terr.decout(i).data := tags.way(i).tag&zero32(TAGRANGE'right-1 downto 0);
          terr.decout(i).err := '0';
          terr.decout(i).merr := '0';
          terr.err := '0';
          terr.merr := '0';
        end if;
      end loop;
    else
      for i in 0 to WAYS-1 loop
        terr.decout(i).data := tags.way(i).tag&zero32(TAGRANGE'right-1 downto 0);
        terr.decout(i).err := '0';
        terr.decout(i).merr := '0';
        terr.err := '0';
        terr.merr := '0';
      end loop;
    end if;
    return terr;
  end function;

  function wayv_to_dway(wayv  : std_logic_vector(MAX_WAYRANGE)) return std_logic_vector is
    variable dway : std_logic_vector(DATAWAYRANGE);
  begin
    for i in WAYRANGE loop
      if wayv(i) = '1' then
        dway := conv_std_logic_vector(i, (DATAWAYRANGE'left-DATAWAYRANGE'right+1));
      end if;
    end loop;
    return dway;
  end function;

  function reg_readout( haddr : in  std_logic_vector(31 downto 0);
                        r     : in  l2c_pipe_reg_type                  
                      ) return  std_logic_vector is
  variable hrdata   : std_logic_vector(CRAMDW-1 downto 0);
  variable scrubway : std_logic_vector(3 downto 0);
  variable bbussize : std_logic_vector(2 downto 0);
  begin
    
    scrubway := (others => '0'); scrubway(ways-1 downto 0) := r.cfg.scrub.wayv;
    case scrubway is
      when "0001" => scrubway := "0000";
      when "0010" => scrubway := "0001";
      when "0100" => scrubway := "0010";
      when others => scrubway := "0011";
    end case;
    
    -- To be compatible with l2cv1
    if    r.cfg.besize = "010" then bbussize := "100";          -- 32-bit
    elsif r.cfg.besize = "011" then bbussize := "010";          -- 64-bit
    else                            bbussize := "001"; end if;  -- 128-bit 
    
  
    hrdata := (others => '0');
    if haddr(7) = '0' then
      if haddr(5) = '0' then
        if haddr(4) = '0' then
          hrdata(127 downto  64) := r.cfg.cen & r.cfg.edacen & r.cfg.repl & zero32(27 downto 19) & r.cfg.besize & r.cfg.index_rep & r.cfg.lock -- Control
                                  & "00" & r.cfg.hprhbypass & r.cfg.hpbufen & r.cfg.stat_mode & r.cfg.wp & r.cfg.hproten
                                  & zero32(31 downto 26)                                                                                    -- Status
                                  & "0" & conv_std_logic(linesize=64)
                                  & conv_std_logic(fttiming=1) & conv_std_logic(ft=1)
                                  & conv_std_logic_vector(mtrr, 6) & bbussize
                                  & conv_std_logic_vector(waysize, 11) & conv_std_logic_vector(ways-1, 2);
        else
          hrdata(127 downto   0) := r.stat.acount(1) & r.stat.hcount(1) & r.stat.ccount(1) & r.stat.ucount(1);                              -- Counters
        end if;
      else  -- haddr(5) = '1'
        if haddr(4) = '0' then
          hrdata(127 downto   0) := r.err.hmaster&  r.err.scrub & r.err.cr_cw_f_mw_wp_ar_aw & r.err.tag_data & r.err.cor_ucor
                                  & r.err.multi & r.err.valid & r.cfg.err.diseresp & r.err.cor_cnt & r.err.pending & r.cfg.err.mask
                                  & r.cfg.selectcb & r.cfg.selecttcb &  r.cfg.xcb & r.cfg.rcb & r.cfg.err.comp & r.cfg.err.rst
                                  & r.err.addr & zero32(31 downto 7) & r.cfg.wtcb & zero32(3 downto 0) & r.cfg.wcb;
        else
          hrdata(127 downto   0) := zero32(31 downto r.cfg.scrub.index'left-r.cfg.scrub.index'right+1+16) & r.cfg.scrub.index
                                  & zero32(15 downto 6) & scrubway(1 downto 0) & zero32(3 downto 2) & r.cfg.scrub.pending & r.cfg.scrub.en 
                                  & zero32(31 downto 16) & r.cfg.scrub.delay & r.cfg.err.inject_addr & "0" & r.cfg.err.inject_pending 
                                  & zero32(31 downto 2) & r.cfg.split;
                                                                                        --zero32(31 downto 16) & r.cfg.debug_delay; --DEBUG
        end if;
      end if;
    else  -- haddr(7) = '1'
      hrdata(127 downto 0) := r.cfg.mtr(conv_integer(haddr(6 downto 4) & "00")) & r.cfg.mtr(conv_integer(haddr(6 downto 4) & "00")+1) &
                              r.cfg.mtr(conv_integer(haddr(6 downto 4) & "00")+2) & r.cfg.mtr(conv_integer(haddr(6 downto 4) & "00")+3);
    end if;
    return hrdata;
  end function;

  function bentoworden(ben : in  std_logic_vector((CRAMDW/8)-1 downto 0)) return std_logic_vector is
    variable worden : std_logic_vector((CRAMDW/8)-1 downto 0);
    variable tmp : integer range 0 to 3;
  begin
    for i in 0 to (CRAMDW/8)-1 loop
      tmp := i mod 4;
      if tmp = 0 then
        worden(i) := ben(i) or ben(i+1) or ben(i+2) or ben(i+3);
      elsif tmp = 1 then 
        worden(i) := ben(i-1) or ben(i) or ben(i+1) or ben(i+2);
      elsif tmp = 2 then 
        worden(i) := ben(i-2) or ben(i-1) or ben(i) or ben(i+1);
      else 
        worden(i) := ben(i-3) or ben(i-2) or ben(i-1) or ben(i);
      end if;
    end loop;
    return worden;
  end function;
  
  function mergewdata(wen   : in  std_logic_vector((CRAMDW/8)-1 downto 0);
                      wdata : in  std_logic_vector(CRAMDW-1 downto 0);
                      data  : in  std_logic_vector(CRAMDW-1 downto 0)) return std_logic_vector is
    variable newdata : std_logic_vector(CRAMDW-1 downto 0);
  begin
    for i in 0 to (CRAMDW/8)-1 loop
      if wen(i) = '1' then
        newdata(7+i*8 downto 0+i*8) := wdata(7+i*8 downto 0+i*8);
      else
        newdata(7+i*8 downto 0+i*8) := data(7+i*8 downto 0+i*8);
      end if;
    end loop;
    return newdata;
  end function;

  function get_mtrr(addr : std_logic_vector(MTRRANGE); mtr : mtrr_arr_type) return std_logic_vector is
    variable mtruc, mtrwt, mtrcb, mtrwp : std_logic;
  begin
    mtruc := '0'; mtrwt := '0'; mtrcb := '0'; mtrwp := '0';
    if MTRR /= 0 then
      for i in 0 to MTRR-1 loop
        if (addr and mtr(i)(15 downto 2)) = (mtr(i)(31 downto 18)  and mtr(i)(15 downto 2)) then
          if mtr(i)(0) = '1' then
            case mtr(i)(17 downto 16) is
              when "00" => mtruc := '1';  -- Uncached
              when "01" => mtrwt := '1';  -- Write-through
              when "10" => mtrcb := '1';
              when others =>
            end case;
          end if;
          if mtr(i)(1) = '1' then
            mtrwp := '1';                 -- Write protection
          end if;
        end if;
      end loop;
    end if;
  
    if mtruc = '1' then mtrwt := '0'; mtrcb := '0'; end if;
    if mtrwt = '1' then mtrcb := '0'; end if;
    if mtrwp = '1' then mtrwt := '0'; end if;
    
    return (mtruc&mtrwt&mtrcb&mtrwp);
  end function;
begin

  comb : process(rst, r, fetop, betop, cramo)
    variable v            : l2c_pipe_reg_type;
    
    variable p5_hold      : std_logic;
    
    variable p4_hold      : std_logic;
    
    variable p3_hold      : std_logic;
    
    variable p2_hold      : std_logic;
    variable p2_vdlwen    : std_logic;
    variable p2_vdlwindex : std_logic_vector(INDEXRANGE);
    variable p2_trep      : std_logic;
    variable p2_dread     : std_logic;
    variable p2_dwrite    : std_logic;
    variable p2_drmw      : std_logic;
    variable p2_dfetch    : std_logic;
    variable p2_dstore    : std_logic;
    variable p2_acc_state_valid : std_logic;
    variable p2_data_slice      : std_logic_vector(3 downto 0);
    variable p2_fetch_col : std_logic;
    variable p2_bypass    : std_logic;
    variable p2_reissue   : std_logic;

    variable p2_flush_wayv  : std_logic_vector(MAX_WAYRANGE); 
    variable p2_flush_wb_way_cnt : integer range 0 to WAYRANGE'left+1; 
    
    variable p2_flush_tag_wayv  : std_logic_vector(MAX_WAYRANGE);
    variable p2_flush_vd_wayv  : std_logic_vector(MAX_WAYRANGE);
    variable p2_flush_wb_wayv   : std_logic_vector(MAX_WAYRANGE);
    variable p2_terr_tag_wayv : std_logic_vector(MAX_WAYRANGE);
    variable p2_terr_vd_wayv  : std_logic_vector(MAX_WAYRANGE);

    variable p2_diagt_valid   : std_logic_vector(VALIDRANGE);
    variable p2_diagt_dirty   : std_logic_vector(DIRTYRANGE);
    variable p2_diagt_tag     : std_logic_vector(TAGRANGE);
    variable p2_diagt_lru     : std_logic_vector(LRURANGE);
    variable p2_diagt_tag_wayv: std_logic_vector(MAX_WAYRANGE);
    variable p2_diagt_vd_wayv : std_logic_vector(MAX_WAYRANGE);
   
    variable p2_wayv      : std_logic_vector(MAX_WAYRANGE);
    variable p2_dway      : std_logic_vector(DATAWAYRANGE);
    variable p2_dindex    : std_logic_vector(INDEXRANGE);
    variable p2_doffset   : std_logic_vector(CLOFFSETRANGE);
    variable p2_data      : std_logic_vector(DATARANGE);
    variable p2_dben      : std_logic_vector(BENRANGE);
    variable p2_dwen      : std_logic_vector(BENRANGE);

    variable p2_merge_wdata : std_logic_vector(DATARANGE);

    variable p1_hold      : std_logic;
    variable p1_reg_addr  : std_logic_vector(7 downto 0);
    variable p1_wdata     : std_logic_vector(DATARANGE);
    variable p1_scrubwayv : std_logic_vector(3 downto 0);
    variable p1_mtrr      : std_logic_vector(3 downto 0);

    variable p0_hold      : std_logic;
    variable p0_tren      : std_logic_vector(MAX_WAYRANGE);
    variable p0_twen      : std_logic_vector(MAX_WAYRANGE);
    variable p0_tindex    : std_logic_vector(INDEXRANGE);
    variable p0_newtag_tag: std_logic_vector(TAGRANGE);
    variable p0_newtag_cb : std_logic_vector(MAX_TAGCBRANGE);
    
    variable scanen       : std_logic;
    variable tlookup      : l2c_pipe_tag_lookup_type;

  begin
    v := r;

  -- TAG Lookup ---------------------------------------------------------------------
    tag_comp(r.p1.state.acc.addr, r.p1.tags, r.cfg.lock, r.cfg.flush.mode, r.cfg.repl, r.prand, r.p1.state.acc.master, r.cfg.index_rep, 
             tlookup.tag, tlookup.lru, tlookup.hitv, tlookup.hit, tlookup.wayv, tlookup.way, tlookup.lock_bypass, tlookup.lock_hit);


  -- P5 (Data to AHB) ---------------------------------------------------------------
    p5_hold := '0';
    
    if r.p4.state.acc.valid = '1' then
      -- store access
      v.p5.state  := r.p4.state;
    else
      v.p5.state.acc.valid := '0';
    end if;

    --if (r.p4.state.acc.valid and not r.p4.state.acc.write) = '1' then
    if (r.p4.state.acc.valid and not r.p4.state.rmw  and r.p4.fe_dvalid) = '1' then
      ptofe.data_valid  <= '1';
      ptofe.data_locked <= r.p4.state.acc.locked;
    else
      ptofe.data_valid  <= '0';
      ptofe.data_locked <= '0';
    end if;
    ptofe.data          <= r.p4.data;
    ptofe.accid         <= r.p4.state.acc.accid;
    ptofe.data_offset   <= r.p4.state.acc.addr(MAX_OFFSETRANGE);
    ptofe.data_done     <= r.p4.data_done;
    ptofe.data_reissued <= r.p4.state.acc.reissued;
    --ptofe.derr  <= r.p4.derr;

    if (r.p4.state.acc.valid and not r.p4.state.rmw and not r.p4.fe_dvalid) = '1' then
      ptobe.data_valid <= '1';
    else
      ptobe.data_valid <= '0';
    end if;
    ptobe.data        <= r.p4.data;
    ptobe.data_offset <= r.p4.state.acc.addr(MAX_OFFSETRANGE);
    ptobe.data_err    <= '0'; 
    

  -- P4 (Data reg.)   ---------------------------------------------------------------
    p4_hold := p5_hold;

    if r.p3.state.acc.valid = '1' and p5_hold = '0' then
      -- store access
      v.p4.state      := r.p3.state;
      v.p4.data_slice := r.p3.data_slice;
      v.p4.fe_dvalid  := r.p3.fe_dvalid;

      if (r.p3.state.reg or r.p3.state.diagt) = '1' or 
         r.p3.state.acc.addr(CLOFFSETRANGE) = one32(CLOFFSETRANGE) then
        v.p4.data_done := '1';
      else
        v.p4.data_done := '0';
      end if;

      if (r.p3.state.reg or r.p3.state.diagt) = '1' then
        v.p4.data       := r.p3.tagrdata; 
      else
        v.p4.data       := r.p3.data;   
      end if;
    elsif p5_hold = '0' then
      v.p4.state.acc.valid := '0';
      v.p4.fe_dvalid  := '0'; 
    end if;

  -- P3 (Data access) ---------------------------------------------------------------
    p3_hold := p4_hold;

    if r.p2.dread = '1' and p4_hold = '0' then
      -- store access
      v.p3.state      := r.p2.state;
      if r.p2.tlookup.hit = '0' then v.p3.state.rmw := '0'; end if; -- No Read-Modify-Write on cahce miss
      v.p3.tagrdata   := r.p2.tagrdata;
      if (r.p2.state.reg or r.p2.state.diagt) = '1' then
        v.p3.data_slice := (others => '0');
        v.p3.data_slice(conv_integer(r.p2.state.acc.addr(CLOFFSETRANGE))) := '1';
      else
        v.p3.data_slice := r.p2.tlookup.tag.valid;
      end if;
      v.p3.fe_dvalid := (r.p2.tlookup.hit or r.p2.state.reg or r.p2.state.diagt) and not r.p2.state.rmw and not r.p2.state.flush.pending; 
      for i in WAYRANGE loop
        if r.p2.dacc.wayv(i) = '1' then
          v.p3.data := cramo.data(i);  
        end if;
      end loop;
    elsif p4_hold = '0' then
      v.p3.state.acc.valid := '0';
      v.p3.fe_dvalid := '0';
    end if;


  -- P2 (TAG comp)    ---------------------------------------------------------------
    p2_hold := p3_hold;
    
    -- Check fo match to fetch address 
    p2_fetch_col := '0';
    if r.p2.facc.valid = '1' and r.p1.state.acc.valid = '1' and r.p1.state.acc.reissued = '0' and
       (r.p2.facc.valid and r.p2.facc.done and r.p1.state.acc.valid and not r.p1.state.acc.reissue and not r.p1.state.acc.reissued) = '0' then
      if r.p1.state.acc.addr(INDEXRANGE) = r.p2.facc.index(INDEXRANGE) then 
        p2_fetch_col := '1';
      end if;
    end if;

    -- Bypass
    p2_bypass := '0';
    if r.p1.bypass = '1' then
      if ((r.p2.facc.valid and r.p2.facc.done) = '1' or r.p2.facc.valid = '0') and 
          (r.p1.state.acc.reissued = '1' or r.p1.state.acc.reissue = '0') then
        p2_bypass := '1';
      end if;
    end if;

    if p2_fetch_col = '1' then tlookup.hit := '0'; end if;
    if r.p1.bypass = '1' then tlookup.hit := '0'; end if; 
    if r.p1.state.diagd = '1' then 
      tlookup.hitv := (others => '0'); tlookup.wayv := (others => '0');
      tlookup.hitv(conv_integer(r.p1.state.acc.addr(IODATAWAYRANGE))) := '1'; 
      tlookup.wayv(conv_integer(r.p1.state.acc.addr(IODATAWAYRANGE))) := '1'; 
      tlookup.tag.valid := (others => '1');
      tlookup.hit := '1'; 
    end if;
    if r.p1.state.diagt = '1' or r.p1.state.reg = '1' then 
      tlookup.hit := '1'; 
    end if;
    
    -- Access need to be reissued by FE
    p2_reissue := (not tlookup.hit) and (r.p1.state.acc.reissue or (r.p2.facc.valid and not r.p2.facc.done));

    -- Check edac for tags
    if r.p1.state.acc.valid = '1' and (r.p1.state.reg or r.p1.state.diagd or r.p1.state.diagt) = '0' then
      v.p2.terr := check_tags(r.p1.tags, (not r.cfg.edacen), ft);
    else
      v.p2.terr.err := '0'; v.p2.terr.merr := '0';
    end if;

    -- Correct tag error
    p2_terr_tag_wayv := (others => '0');
    p2_terr_vd_wayv := (others => '0');
    if r.p2.terr.err = '1' then
      for i in WAYRANGE loop
        if (orv(r.p1.tags.way(i).valid) and r.p2.terr.decout(i).merr) = '1' then    -- Uncorrectable TAG error
          p2_terr_vd_wayv(i) := '1';
        elsif (orv(r.p1.tags.way(i).valid) and r.p2.terr.decout(i).err) = '1' then  -- Correctable TAG error
          p2_terr_tag_wayv := (others => '0');
          p2_terr_tag_wayv(i) := '1';
        end if;
      end loop;

    end if;

    -- Determine which way to flush (old)
    p2_flush_tag_wayv := (others => '0');
    p2_flush_vd_wayv := (others => '0');
    p2_flush_wb_wayv := (others => '0');
    if r.p2.drw.pending(0) = '0' then
      if r.p1.state.flush.pending = '1' then
        if r.p1.state.flush.lookup = '1' then
          if tlookup.hit = '1' then
            if r.p1.state.flush.inv = '1' then
              p2_flush_vd_wayv := tlookup.wayv and r.p1.state.flush.wayv; -- do not flush locked ways (can be done in tag_comp)
            end if;
            if r.p1.state.flush.wb = '1' then
              p2_flush_wb_wayv := tlookup.wayv and r.p1.state.flush.wayv; -- do not flush locked ways
            end if; 
          end if;
        else
          for i in WAYRANGE loop
            if (r.p1.state.flush.wb and r.p1.state.flush.wayv(i) and 
               orv(r.p1.tags.way(i).valid and r.p1.tags.way(i).dirty)) = '1' then
              p2_flush_wb_wayv := (others => '0');
              p2_flush_wb_wayv(i)  := '1';
            end if;
            if r.p1.state.flush.inv = '1' and r.p1.state.flush.wayv(i) = '1' and
               (orv(r.p1.tags.way(i).valid) = '0' or orv(r.p1.tags.way(i).dirty) = '0') then
              p2_flush_vd_wayv(i) := '1';
            end if;
          end loop;
          
          if orv(p2_flush_wb_wayv) = '0' then
            if r.p1.state.flush.inv = '1' then
              p2_flush_vd_wayv := r.p1.state.flush.wayv;
            end if;
            if r.p1.state.flush.tag_upd = '1' then
              p2_flush_tag_wayv := r.p1.state.flush.wayv;
            end if; 
          else
            p2_flush_vd_wayv := p2_flush_vd_wayv or p2_flush_wb_wayv;
            if r.p1.state.flush.tag_upd = '1' then
              p2_flush_tag_wayv := p2_flush_wb_wayv;
            end if;
          end if;
        end if;
      end if;
    end if;
    if (r.p1.state.flush.pending and r.p1.state.flush.fetch) = '1' then
      tlookup.wayv := p2_flush_vd_wayv;
    end if;
    if orv(p2_flush_wb_wayv) = '1' then
      for i in WAYRANGE loop
        if p2_flush_wb_wayv(i) = '1' then
          tlookup.tag := r.p1.tags.way(i);
        end if;
      end loop;
    end if;

    -- TAG diag update
    p2_diagt_valid   := (others => '0');
    p2_diagt_dirty   := (others => '0');
    p2_diagt_tag     := (others => '0');
    p2_diagt_lru     := (others => '0');
    p2_diagt_tag_wayv:= (others => '0');
    p2_diagt_vd_wayv := (others => '0');
    if r.p1.state.diagt = '1' and r.p1.state.acc.write = '1' then -- Diagnostic tag update
      case r.p1.state.acc.addr(3 downto 2) is
        when "00" =>
          p2_diagt_tag_wayv(0):= '1';
          p2_diagt_vd_wayv(0) := '1';
          p2_diagt_tag := r.p1.wdata(127 downto 96+(TAGRANGE'right));
          p2_diagt_valid := r.p1.tags.way(0).valid;
          p2_diagt_dirty := r.p1.tags.way(0).dirty;
          if r.p1.state.acc.addr(CLOFFSETRANGE'left) = '1' and linesize = 64 then
            p2_diagt_valid(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(96+8+2-1 downto 96+8);
            p2_diagt_dirty(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(96+6+2-1 downto 96+6);
          else
            p2_diagt_valid(1 downto 0) := r.p1.wdata(96+8+2-1 downto 96+8);
            p2_diagt_dirty(1 downto 0) := r.p1.wdata(96+6+2-1 downto 96+6);
          end if;
          p2_diagt_lru := r.p1.wdata(96+LRU_BITS-1 downto 96);
        when "01" =>
          p2_diagt_tag_wayv(1):= '1';
          p2_diagt_vd_wayv(1) := '1';
          p2_diagt_tag := r.p1.wdata(95 downto 64+(TAGRANGE'right));
          p2_diagt_valid := r.p1.tags.way(1).valid;
          p2_diagt_dirty := r.p1.tags.way(1).dirty;
          if r.p1.state.acc.addr(CLOFFSETRANGE'left) = '1' and linesize = 64 then
            p2_diagt_valid(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(64+8+2-1 downto 64+8);
            p2_diagt_dirty(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(64+6+2-1 downto 64+6);
          else
            p2_diagt_valid(1 downto 0) := r.p1.wdata(64+8+2-1 downto 64+8);
            p2_diagt_dirty(1 downto 0) := r.p1.wdata(64+6+2-1 downto 64+6);
          end if;
          p2_diagt_lru := r.p1.wdata(64+LRU_BITS-1 downto 64);
        when "10" =>
          p2_diagt_tag_wayv(2):= '1';
          p2_diagt_vd_wayv(2) := '1';
          p2_diagt_tag := r.p1.wdata(63 downto 32+(TAGRANGE'right));
          p2_diagt_valid := r.p1.tags.way(2).valid;
          p2_diagt_dirty := r.p1.tags.way(2).dirty;
          if r.p1.state.acc.addr(CLOFFSETRANGE'left) = '1' and linesize = 64 then
            p2_diagt_valid(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(32+8+2-1 downto 32+8);
            p2_diagt_dirty(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(32+6+2-1 downto 32+6);
          else
            p2_diagt_valid(1 downto 0) := r.p1.wdata(32+8+2-1 downto 32+8);
            p2_diagt_dirty(1 downto 0) := r.p1.wdata(32+6+2-1 downto 32+6);
          end if;
          p2_diagt_lru := r.p1.wdata(32+LRU_BITS-1 downto 32);
        when others =>
          p2_diagt_tag_wayv(3):= '1';
          p2_diagt_vd_wayv(3) := '1';
          p2_diagt_tag := r.p1.wdata(31 downto 0+(TAGRANGE'right));
          p2_diagt_valid := r.p1.tags.way(3).valid;
          p2_diagt_dirty := r.p1.tags.way(3).dirty;
          if r.p1.state.acc.addr(CLOFFSETRANGE'left) = '1' and linesize = 64 then
            p2_diagt_valid(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(0+8+2-1 downto 0+8);
            p2_diagt_dirty(VALID_BITS-1 downto VALID_BITS-2) := r.p1.wdata(0+6+2-1 downto 0+6);
          else
            p2_diagt_valid(1 downto 0) := r.p1.wdata(0+8+2-1 downto 0+8);
            p2_diagt_dirty(1 downto 0) := r.p1.wdata(0+6+2-1 downto 0+6);
          end if;
          p2_diagt_lru := r.p1.wdata(0+LRU_BITS-1 downto 0);
      end case;
    end if;

    -- Valid, Dirty and LRU update
    p2_vdlwen     := r.p1.state.acc.valid and not (r.p1.state.reg or (r.p1.state.diagt and not r.p1.state.acc.write) or r.p1.state.diagd or r.p1.bypass or p2_reissue);
    p2_vdlwindex  := r.p1.state.acc.addr(INDEXRANGE);

    -- Hold
    if v.p2.terr.err = '1'                                                            -- TAG error
       --or (betop.hold and (not tlookup.hit)) = '1'                                  -- do not stall on miss, miss is reissued
       or r.p2.drw.pending /= zero32(r.p2.drw.pending'range)                          -- Data read/write
       or r.p2.facc.pending /= zero32(r.p2.facc.pending'range)                        -- Fetched data pending
       or (r.p2.state.flush.pending and r.p2.facc.valid and not r.p2.facc.done) = '1' -- Hold PIPE during flush
    then
      p2_hold := '1'; 
    end if;

    if r.p1.state.acc.valid = '1' 
       and r.p1.state.flush.pending = '0'
       and r.p1.state.scrub = '0'        
       and r.p1.state.inject = '0'
       and r.p1.state.diagt = '0'
       and r.p1.state.reg = '0'
       and p2_hold = '0' 
       and p2_bypass = '0'
       and p2_reissue = '0'
     then
      p2_trep   := not tlookup.hit;         -- replace line on miss
      p2_dread  := (tlookup.hit and not r.p1.state.acc.write) or (not tlookup.hit and orv(tlookup.tag.dirty));
      p2_dwrite := (tlookup.hit and r.p1.state.acc.write and not r.p1.state.rmw);
      p2_drmw   := (tlookup.hit and r.p1.state.rmw);
      p2_dfetch := not tlookup.hit;
      p2_dstore := (not tlookup.hit and orv(tlookup.tag.dirty));
    elsif r.p1.state.acc.valid = '1' and r.p1.state.flush.pending = '1' and p2_hold = '0' then  -- For flush
      p2_trep   := '0';
      p2_dwrite := '0';
      p2_drmw   := '0';
      if r.p1.state.flush.fetch = '1' then
        p2_dfetch := '1';
      end if;
      if orv(p2_flush_wb_wayv) = '1' then
        p2_dread  := '1'; 
        p2_dstore := '1';
      end if;
    else
      p2_trep   := '0';
      p2_dread  := '0';
      p2_dwrite := '0';
      p2_drmw   := '0';
      p2_dfetch := '0';
      p2_dstore := '0';
      if r.p1.state.flush.pending = '0' and r.p1.state.diagt = '0' then
        p2_vdlwen := '0';
      end if;
    end if;

    

    -- Valid & Dirty bits update
    for i in WAYRANGE loop
      v.p2.newtag.vd_bits(i).valid := r.p1.tags.way(i).valid;
      v.p2.newtag.vd_bits(i).dirty := r.p1.tags.way(i).dirty;
      -- TAG error
      if p2_terr_vd_wayv(i) = '1' then
        v.p2.newtag.vd_bits(i).valid := (others => '0');
      -- replace
      -- flush
      elsif p2_diagt_vd_wayv(i) = '1' then
        v.p2.newtag.vd_bits(i).valid := p2_diagt_valid;
        v.p2.newtag.vd_bits(i).dirty := p2_diagt_dirty;
      elsif (r.p1.state.flush.inv and p2_flush_vd_wayv(i)) = '1' then
        v.p2.newtag.vd_bits(i).valid := (others => r.p1.state.flush.valid);
        v.p2.newtag.vd_bits(i).dirty := (others => r.p1.state.flush.dirty);
      elsif p2_flush_wb_wayv(i) = '1' then
        v.p2.newtag.vd_bits(i).dirty := (others => r.p1.state.flush.dirty);
      elsif p2_trep = '1' and tlookup.wayv(i) = '1' then
        v.p2.newtag.vd_bits(i).valid := (others => '1');
        v.p2.newtag.vd_bits(i).dirty := (others => '0');
        if r.p1.state.acc.write = '1' then
          v.p2.newtag.vd_bits(i).dirty(conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE))) := '1';
        end if;
      elsif (p2_dwrite or p2_drmw) = '1' and tlookup.wayv(i) = '1' then
        v.p2.newtag.vd_bits(i).dirty(conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE))) := '1';
      end if;
    end loop;

    v.p2.newtag.lru := tlookup.lru;

    -- TAG update
    if r.p2.terr.err = '1' and r.p2.newtag.tag_upd = '0' then 
      for i in WAYRANGE loop
        if p2_terr_tag_wayv(i) = '1' then
          v.p2.newtag.tag := r.p1.tags.way(i).tag(TAGRANGE);
        end if;
      end loop;
      v.p2.newtag.tag_upd := '1';
      v.p2.newtag.tag_wayv:= p2_terr_tag_wayv;
      v.p2.newtag.vd_wayv := p2_terr_vd_wayv;
    elsif v.p2.terr.err = '1' then
      v.p2.newtag.tag_upd := '0';
      v.p2.newtag.tag_wayv:= (others => '0');
      v.p2.newtag.vd_wayv := (others => '0');
    elsif p2_diagt_tag_wayv /= zero32(p2_diagt_tag_wayv'range) then
      v.p2.newtag.tag_upd := '1';
      v.p2.newtag.tag_wayv:= p2_diagt_tag_wayv;
      v.p2.newtag.vd_wayv := p2_diagt_vd_wayv;
      v.p2.newtag.tag     := p2_diagt_tag;
      v.p2.newtag.lru     := p2_diagt_lru;
    else
      v.p2.newtag.tag := r.p1.state.acc.addr(TAGRANGE);
      if orv(p2_flush_tag_wayv) = '1' and p2_hold = '0' then 
        v.p2.newtag.tag_upd := '1';
        v.p2.newtag.tag_wayv:= p2_flush_tag_wayv;
        v.p2.newtag.vd_wayv := p2_flush_vd_wayv;
        v.p2.newtag.lru := (others => '0'); 
      elsif p2_trep = '1' then
        v.p2.newtag.tag_upd := '1';
        v.p2.newtag.tag_wayv:= tlookup.wayv;
        v.p2.newtag.vd_wayv := tlookup.wayv;
      else
        v.p2.newtag.tag_upd := '0';
        v.p2.newtag.tag_wayv:= (others => '0');
        if (p2_dwrite or p2_drmw) = '1' then
          v.p2.newtag.vd_wayv := tlookup.wayv;
        elsif orv(p2_flush_vd_wayv) /= '0' and p2_hold = '0' then
          v.p2.newtag.vd_wayv := p2_flush_vd_wayv;
        else
          v.p2.newtag.vd_wayv := (others => '0');
        end if;
      end if;
    end if;

    v.p2.newtag.index   := r.p1.state.acc.addr(INDEXRANGE);

    -- Generate TAG check-bits
    if ft /= 0 then
      if r.cfg.selecttcb = "01" then                        -- replace generated cb with wcb
        v.p2.newtag.cb := r.cfg.wtcb;
      elsif r.cfg.selecttcb = "10" or r.cfg.xcb = '1' then  -- xor cb with wcb
        v.p2.newtag.cb := l2c_edacencode(v.p2.newtag.tag(TAGRANGE)&zero32(TAGRANGE'right-1 downto 0)) xor r.cfg.wtcb;
      else                                                  -- use generated cb
        v.p2.newtag.cb := l2c_edacencode(v.p2.newtag.tag(TAGRANGE)&zero32(TAGRANGE'right-1 downto 0));
      end if;
    else
      v.p2.newtag.cb := (others => '0');
    end if;

    
    -- DATA access
      -- DATA error
        -- merr
        -- err
      -- Delayed DATA access
      -- New access
    
    -- Exec all delayed cache data accesses
    if (r.p2.drw.pending /= zero32(r.p2.drw.pending'range) or r.p2.facc.pending /= zero32(r.p2.facc.pending'range)) and p3_hold = '0' then
      v.p2.drw.pending(3 downto 0) := '0' & r.p2.drw.pending(3 downto 1);
      v.p2.drw.wr(3 downto 0) := '0' & r.p2.drw.wr(3 downto 1);
      v.p2.drw.addr(CLOFFSETRANGE) := r.p2.drw.addr(CLOFFSETRANGE) + 1; 
      if r.p2.drw.pending(2 downto 1) = "00" and p2_drmw = '0' and r.p2.facc.pending(conv_integer(r.p2.facc.rdindex)) = '1' then
        v.p2.drw.pending(0) := '1';
        v.p2.drw.wr(0)      := '1';
        v.p2.drw.addr       := r.p2.facc.tag & r.p2.facc.index & r.p2.facc.offset;
        v.p2.drw.wayv       := r.p2.facc.wayv;
        v.p2.drw.ben        := (others => '1');
        v.p2.drw.wdata      := r.p2.facc.wdata(conv_integer(r.p2.facc.rdindex));
        v.p2.facc.offset(CLOFFSETRANGE) := r.p2.facc.offset(CLOFFSETRANGE) + 1; 
        v.p2.facc.pending(conv_integer(r.p2.facc.rdindex)) := '0';
        v.p2.facc.rdindex   := r.p2.facc.rdindex + 1;
      end if;
    end if;

    -- merge be-data
    if r.p2.facc.valid = '1' and r.p2.facc.merge_slice(conv_integer(betop.data_offset(CLOFFSETRANGE))) = '1' then
      p2_merge_wdata := mergewdata(r.p2.facc.merge_ben, r.p2.facc.merge_data, betop.data);
    else
      p2_merge_wdata := betop.data;
    end if;

    if betop.data_valid = '1' then 
      if orv(p2_flush_wb_wayv) = '0' and p2_dread = '0' and r.p2.drw.pending(2 downto 1) = "00" and p2_drmw = '0' and r.p2.facc.pending = zero32(r.p2.facc.pending'range) then 
        v.p2.drw.pending(0)             := '1';
        v.p2.drw.wr(0)                  := '1';
        v.p2.drw.addr                   := r.p2.facc.tag & r.p2.facc.index & r.p2.facc.offset;
        v.p2.drw.wayv                   := r.p2.facc.wayv;
        v.p2.drw.ben                    := (others => '1');
        v.p2.drw.wdata                  := p2_merge_wdata;
        v.p2.facc.offset(CLOFFSETRANGE) := r.p2.facc.offset(CLOFFSETRANGE) + 1; 
      else
        v.p2.facc.pending(conv_integer(r.p2.facc.wdindex)) := '1';
        v.p2.facc.wdata(conv_integer(r.p2.facc.wdindex)) := p2_merge_wdata;
        v.p2.facc.wdindex := r.p2.facc.wdindex + 1;
      end if;
    end if;

    -- Add delayed cache data accesses
    if v.p2.terr.err = '0' then
      if p2_dread = '1' and (r.p1.state.acc.addr(CLOFFSETRANGE) /= one32(CLOFFSETRANGE) or p2_trep = '1') then 
        v.p2.drw.pending(0 downto 0)  := (others => '1'); 
        v.p2.drw.ben                  := (others => '1');
        v.p2.drw.wr                   := (others => '0');
        v.p2.drw.addr                 := r.p1.state.acc.addr;
        v.p2.drw.addr(CLOFFSETRANGE)  := r.p1.state.acc.addr(CLOFFSETRANGE) + 1; 
        if (p2_trep and p2_dread) = '1' then
          v.p2.drw.addr(CLOFFSETRANGE)  := zero32(CLOFFSETRANGE) + 1; 
        end if;
        if orv(p2_flush_wb_wayv) = '1' then                           -- Flush
          v.p2.drw.wayv                 := p2_flush_wb_wayv; 
        else
          v.p2.drw.wayv                 := tlookup.wayv;
        end if;
      elsif p2_drmw = '1' then
        v.p2.drw.pending(2)           := '1';
        v.p2.drw.wr(2)                := '1';
        v.p2.drw.addr                 := r.p1.state.acc.addr;
        v.p2.drw.wayv                 := tlookup.wayv;
        v.p2.drw.ben                  := r.p1.wen;
        v.p2.drw.wdata                := r.p1.wdata;
      end if;
    end if;

    p2_data_slice := (others => '0'); 
    if (r.p1.state.reg or r.p1.state.diagt) = '1' then
      p2_data_slice(conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE))) := '1';
    else
      p2_data_slice(VALIDRANGE) := tlookup.tag.valid;
    end if;
    p2_acc_state_valid := '0';
    v.p2.dread := '0';
    if r.p1.state.acc.valid = '1' and p2_hold = '0' then
      -- store access
      v.p2.state := r.p1.state;
      v.p2.tlookup := tlookup;
      
      if (p2_trep and p2_dread) = '1' then
        v.p2.state.acc.addr(OFFSETRANGE) := zero32(OFFSETRANGE);
      end if;
      
      -- Access status to FE
      if p2_hold = '0' and r.p1.state.flush.pending = '0' then
        p2_acc_state_valid := '1';
      end if;
    
      v.p2.dread := p2_dread or p2_drmw or ((r.p1.state.reg or r.p1.state.diagt) and not r.p1.state.acc.write);
      
    elsif p3_hold = '0' and p2_hold = '0' then
      v.p2.state.acc.valid := '0';
    elsif p3_hold = '0' and r.p2.drw.pending(0) = '1' then
      v.p2.state.acc.write := r.p2.drw.wr(0);
      v.p2.state.acc.addr  := r.p2.drw.addr;
      v.p2.dread := not r.p2.drw.wr(0);
    elsif p3_hold = '0' and r.p2.state.flush.pending = '1' then 
      v.p2.state.acc.valid := '0';                            
    end if;

    -- Store address until fetch is done
    if r.p2.facc.done = '1' and 
       r.p1.state.acc.valid = '1' and r.p1.state.acc.reissue = '0' and r.p1.state.acc.reissued = '0' -- clear facc on the first acc not marked "reissue on miss" when facc is done. 
                                                                                                     -- facc done need to take at least 2 cycles to let all pending accesses in PIPE to go through
    then
      v.p2.facc.valid := '0';
    end if;
    if (r.p2.facc.valid and betop.done) = '1' then
      v.p2.facc.done := '1';
    end if;
    if p2_dfetch = '1' or p2_bypass = '1' or p2_dstore = '1' then
      v.p2.facc.valid   := '1';
      v.p2.facc.done    := '0';
      v.p2.facc.wayv    := tlookup.wayv;
      v.p2.facc.tag     := r.p1.state.acc.addr(TAGRANGE);
      v.p2.facc.index   := r.p1.state.acc.addr(INDEXRANGE);
      v.p2.facc.offset  := (others => '0'); --r.p1.state.acc.addr(OFFSETRANGE);
      v.p2.facc.accid   := r.p1.state.acc.accid;
      -- merge data
      v.p2.facc.merge_data  := r.p1.wdata;
      v.p2.facc.merge_ben   := r.p1.wen;
      v.p2.facc.merge_slice := (others => '0');
      if r.p1.state.acc.write = '1' then
        v.p2.facc.merge_slice(conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE))) := '1';
      end if;
      --
      v.p2.facc.pending := (others => '0');
      v.p2.facc.wdindex := (others => '0');
      v.p2.facc.rdindex := (others => '0');
    end if;

    -- DATA RAM access mux
    if r.p2.drw.pending = zero32(r.p2.drw.pending'range) then 
      p2_dindex   := r.p1.state.acc.addr(INDEXRANGE);
      p2_doffset  := r.p1.state.acc.addr(CLOFFSETRANGE);
      if (p2_trep and p2_dread) = '1' then
        p2_doffset  := zero32(CLOFFSETRANGE);
      end if;
      p2_data     := r.p1.wdata;
      p2_dben     := (others => (p2_dread or p2_drmw or p2_dwrite or orv(p2_flush_wb_wayv)));
      if p2_dwrite = '1' then
        p2_dwen := r.p1.wen;
      else
        p2_dwen := (others => '0');
      end if;
      if orv(p2_flush_wb_wayv) = '1' then
        p2_dway := wayv_to_dway(p2_flush_wb_wayv);  
        p2_wayv := p2_flush_wb_wayv;  
      else
        p2_dway := wayv_to_dway(tlookup.wayv);  
        p2_wayv := tlookup.wayv;  
      end if;
    elsif r.p2.drw.pending(0) = '1' then
      p2_dindex   := r.p2.drw.addr(INDEXRANGE);
      p2_doffset  := r.p2.drw.addr(CLOFFSETRANGE);
      if r.p2.state.rmw = '1' and r.p2.drw.wr(0) = '1' then
        p2_data     := mergewdata(r.p2.drw.ben, r.p2.drw.wdata, r.p4.data); 
        p2_dben     := bentoworden(r.p2.drw.ben);
        p2_dwen     := bentoworden(r.p2.drw.ben);
      else
        p2_data     := r.p2.drw.wdata;
        p2_dben     := r.p2.drw.ben;
        p2_dwen     := (others => r.p2.drw.wr(0)); 
      end if;
      p2_dway     := wayv_to_dway(r.p2.drw.wayv);  
      p2_wayv     := r.p2.drw.wayv;  
    else
      p2_dben     := (others => '0');
      p2_dwen     := (others => '0');
    end if;

    -- DATA RAM signals
    v.p2.dacc.index   := p2_dindex;
    v.p2.dacc.offset  := p2_doffset;
    v.p2.dacc.data    := p2_data;
    v.p2.dacc.way     := p2_dway;
    v.p2.dacc.wayv    := p2_wayv;
    v.p2.dacc.ben     := p2_dben;
    v.p2.dacc.wen     := p2_dwen;

    -- Generate data check-bits
    if ft /= 0 then
      for i in 0 to 3 loop 
        if r.cfg.selectcb = "01" then                   -- replace generated cb with wcb
          v.p2.dacc.cb(6 + (i)*7 downto (i)*7) := r.cfg.wcb(6 + (i)*7 downto (i)*7);
        elsif r.cfg.selectcb = "10" or r.cfg.xcb = '1' or (r.p2.drw.pending(0) and r.p2.drw.wr(0) and r.p2.drw.inject) = '1' then -- xor cb with wcb
          v.p2.dacc.cb(6 + (i)*7 downto (i)*7) := l2c_edacencode(v.p2.dacc.data(31 + i*32 downto i*32)) xor r.cfg.wcb(6 + (i)*7 downto (i)*7);
        else                                        -- use generated cb
          v.p2.dacc.cb(6 + (i)*7 downto (i)*7) := l2c_edacencode(v.p2.dacc.data(31 + i*32 downto i*32));
        end if;
      end loop;
    else
      v.p2.dacc.cb := (others => '0');
    end if;
    
  
    -- Diagnostic Tag and register read out
    if r.p1.state.reg = '1' then
      v.p2.tagrdata := reg_readout(r.p1.state.acc.addr, r);
    elsif r.p1.state.acc.addr(CLOFFSETRANGE'right) = zero32(CLOFFSETRANGE'right) and r.p2.drw.pending = zero32(r.p2.drw.pending'range) then 
      if linesize = 64 then
        v.p2.tagrdata := 
            r.p1.tags.way(0).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(0).valid(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(0).valid(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(0).dirty(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(0).dirty(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0) 
          & r.p1.tags.way(1).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(1).valid(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(1).valid(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(1).dirty(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(1).dirty(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0) 
          & r.p1.tags.way(2).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(2).valid(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(2).valid(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(2).dirty(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(2).dirty(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0) 
          & r.p1.tags.way(3).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(3).valid(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(3).valid(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(3).dirty(0+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & r.p1.tags.way(3).dirty(1+2*conv_integer(r.p1.state.acc.addr(CLOFFSETRANGE'left))) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0);
      else
        v.p2.tagrdata := 
            r.p1.tags.way(0).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(0).valid(0) & r.p1.tags.way(0).valid(1) & r.p1.tags.way(0).dirty(0) & r.p1.tags.way(0).dirty(1) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0) 
          & r.p1.tags.way(1).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(1).valid(0) & r.p1.tags.way(1).valid(1) & r.p1.tags.way(1).dirty(0) & r.p1.tags.way(1).dirty(1) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0) 
          & r.p1.tags.way(2).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(2).valid(0) & r.p1.tags.way(2).valid(1) & r.p1.tags.way(2).dirty(0) & r.p1.tags.way(2).dirty(1) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0) 
          & r.p1.tags.way(3).tag(TAGRANGE) & zero32(21-TAG_BITS downto 0) & r.p1.tags.way(3).valid(0) & r.p1.tags.way(3).valid(1) & r.p1.tags.way(3).dirty(0) & r.p1.tags.way(3).dirty(1) & zero32(5 downto LRU_BITS) & r.p1.tags.lru(LRU_BITS-1 downto 0);
      end if;
    else
      v.p2.tagrdata(CRAMDW-1 downto CRAMDW-32) := '0' & r.p1.tags.way(0).cb & '0' & r.p1.tags.way(1).cb & '0' & r.p1.tags.way(2).cb & '0' & r.p1.tags.way(3).cb; 
    end if;

    -- FE access status
    ptofe.p2_valid    <= p2_acc_state_valid;
    ptofe.p2_accid    <= r.p1.state.acc.accid;
    ptofe.p2_slice    <= p2_data_slice;
    ptofe.p2_hit      <= tlookup.hit or (r.p1.state.reg or r.p1.state.diagt or r.p1.state.diagd);
    ptofe.p2_reissue  <= p2_reissue;
    ptofe.p2_fdone    <= betop.done or (r.p1.state.acc.valid and r.p1.state.acc.reissued and tlookup.hit);
    ptofe.p2_fbusy    <= r.p2.facc.valid and not (r.p2.facc.done or betop.done);
    ptofe.p2_locked   <= r.p1.state.acc.valid and r.p1.state.acc.locked;
    ptofe.p2_bypass   <= p2_bypass;
    --
    ptofe.cfg_cen     <= r.cfg.cen;

    -- BE access
    ptobe.accid       <= r.p1.state.acc.accid;
    ptobe.new_addr    <= r.p1.state.acc.addr;
    ptobe.old_addr    <= tlookup.tag.tag & r.p1.state.acc.addr(INDEXRANGE) & zero32(OFFSETRANGE);
    ptobe.size        <= r.p1.state.acc.size;
    ptobe.master      <= r.p1.state.acc.master;

    ptobe.fetch_valid <= (p2_dfetch and not (r.p1.state.acc.write or r.p1.state.flush.pending)) & p2_dfetch;
    ptobe.fetch_slice <= (others => '1');  
    
    ptobe.store_valid <= p2_dstore;         
    ptobe.store_slice <= (others => '1'); 

    ptobe.bp_valid    <= p2_bypass;
    ptobe.bp_write    <= r.p1.state.acc.write;
    ptobe.bp_data     <= r.p1.wdata;

  -- P1 (TAG reg.)    ---------------------------------------------------------------
    p1_hold := (p2_hold or v.p2.newtag.tag_upd or r.p1.state.reg) and r.p1.state.acc.valid;

    -- MTRR
    p1_mtrr := get_mtrr(r.p0.state.acc.addr(MTRRANGE), r.cfg.mtr);

    -- Flush way-loop
    if r.p1.state.acc.valid = '1' and p2_hold = '0' and r.p1.state.flush.lookup = '0' and 
       r.p1.state.flush.pending = '1' and v.p2.newtag.vd_wayv /= zero32(v.p2.newtag.vd_wayv'range) then
      v.p1.state.flush.wayv := (r.p1.state.flush.wayv and (r.p1.state.flush.wayv xor v.p2.newtag.vd_wayv));
      if v.p1.state.flush.wayv /= zero32(v.p1.state.flush.wayv'range) then
        p1_hold := '1';
      end if;
    end if;

    if r.p0.state.acc.valid = '1' and p1_hold = '0' then
      v.p1.state  := r.p0.state;
      
      v.p1.wdata := fetop.wdata;

      v.p1.wen := ben128(r.p0.state.acc.size, r.p0.state.acc.addr(3 downto 0), false);
 
      -- Access control
      v.p1.bypass := p1_mtrr(3) or not r.cfg.cen or not ahb_slv_dec_cache(r.p0.state.acc.addr, fetop.ahbsov, cached) or r.p0.state.acc.mbsel(2);
      v.p1.wt     := p1_mtrr(2);
      v.p1.wprot  := p1_mtrr(0);

      -- Register access (start read and write) and diagnostic access
      v.p1.state.diagd := '0'; v.p1.state.diagt := '0'; v.p1.state.reg := '0';
      if r.p0.state.acc.valid = '1' and r.p0.state.acc.mbsel(1) = '1' then 
        if r.p0.state.acc.addr(IODATABIT) = '0' then
          if r.p0.state.acc.addr(IOCONFBIT) = '0' then -- Reg (cancle cache access)
            v.p1.state.reg := '1';
            v.p1.bypass := '0';
          else                                -- Tag
            v.p1.state.diagt := '1';
            v.p1.bypass := '0';
          end if;
        else                                  -- Data
          v.p1.state.diagd := '1';
          v.p1.bypass := '0';
        end if;
      end if;

      -- RMW
      if (r.p0.state.acc.write = '1' and r.p0.state.acc.size(2 downto 1) = "00" and 
         --(r.cfg.edacen = '1' or rmw = 1) and r.p0.fmode = zero32(r.p0.fmode'range)) 
         (r.cfg.edacen = '1' or rmw = 1))                                            
         or r.p0.state.inject = '1' then -- dummy RMW
        v.p1.state.rmw := '1';
      else
        v.p1.state.rmw := '0';
      end if;
      

      -- TAG read out
      if r.p1.deltvalid = '0' then
        v.p1.tags := get_tags(cramo);
      else
        if r.p1.state.acc.addr(INDEXRANGE) /= r.p0.state.acc.addr(INDEXRANGE) then -- No update if access to the same TAGs
          v.p1.tags := r.p1.deltags;
        end if;
        v.p1.deltvalid := '0';
      end if;
    elsif p2_hold = '0' or v.p2.newtag.tag_upd = '1' then
      if r.p1.state.flush.pending = '0' or v.p1.state.flush.wayv = zero32(v.p1.state.flush.wayv'range) or r.p1.state.flush.lookup = '1' then
        v.p1.state.acc.valid := '0';
      end if;
      v.p1.state.diagd := '0'; v.p1.state.diagt := '0'; v.p1.state.reg := '0';
      v.p1.state.rmw := '0';
      v.p1.bypass := '0';
      v.p1.wt     := '0';
      v.p1.wprot  := '0';
    end if;

    -- Store TAG read out for later use
    if r.p0.state.acc.valid = '1' and p1_hold = '1' 
       and r.p1.deltvalid = '0' and r.p2.newtag.tag_upd = '0' then
      v.p1.deltvalid := '1';
      v.p1.deltags := get_tags(cramo);
    end if;
    
    -- TAG update
    if r.p1.state.acc.valid = '1' and 
       r.p1.state.acc.addr(INDEXRANGE) = v.p2.newtag.index(INDEXRANGE) then 
      for i in WAYRANGE loop
        if v.p2.newtag.tag_wayv(i) = '1' then
          v.p1.tags.way(i).tag    := v.p2.newtag.tag; 
          v.p1.tags.way(i).cb     := v.p2.newtag.cb; 
        end if;
      end loop;
    end if;
    if r.p0.state.acc.valid = '1' and (
       (r.p0.state.acc.addr(INDEXRANGE) = v.p2.newtag.index(INDEXRANGE)) or
       (r.p1.state.flush.pending = '1' and p1_hold = '1')
    ) then 
      for i in WAYRANGE loop
        if v.p2.newtag.vd_wayv(i) = '1' then
          v.p1.tags.way(i).valid  := v.p2.newtag.vd_bits(i).valid; 
          v.p1.tags.way(i).dirty  := v.p2.newtag.vd_bits(i).dirty; 
          v.p1.tags.lru := v.p2.newtag.lru; 
        end if;
      end loop;
    end if;

    -- Configuration register write
    p1_wdata      := r.p1.wdata;
    p1_reg_addr   := r.p1.state.acc.addr(7 downto 0);
    p1_scrubwayv  := (others => '0'); 
    if r.p1.state.reg = '1' and r.p1.state.acc.write = '1' and r.p1.state.acc.size = "010" then -- Only accept 32-bit write
      --reg_update(r.p1.addr, r.p1.hwdata, r, v);
      if p1_reg_addr(7) = '0' then
        if p1_reg_addr(5) = '0' then
          case p1_reg_addr(4 downto 2) is 
            when "000" => -- control
              v.cfg.cen := p1_wdata(127);
              if (ft+fttiming) /= 0 then v.cfg.edacen := p1_wdata(126); end if;
              v.cfg.repl := p1_wdata(125 downto 124);
              v.cfg.updbesize := p1_wdata(114 downto 112);
              v.cfg.index_rep := p1_wdata(111 downto 108);
              v.cfg.lock := p1_wdata(107 downto 104);
              v.cfg.hprhbypass := p1_wdata(101);
              v.cfg.hpbufen := p1_wdata(100);
              if stat >= 1 and stat /= 2 then
                v.cfg.stat_mode(0) := p1_wdata(98);
              elsif stat = 2 then
                v.cfg.stat_mode := p1_wdata(99 downto 98);
              end if;
              v.cfg.wp := p1_wdata(97);
              v.cfg.hproten := p1_wdata(96);
            when "001" => -- status
            when "010" => -- flush (mem)
              v.cfg.flush.pending := p1_wdata(32) or p1_wdata(33);
              v.cfg.flush.rst_addr := p1_wdata(2); -- reset index address in for flush all lines in cache
              v.cfg.flush.addr(31 downto 5) := p1_wdata(63 downto 37);
              v.cfg.flush.addr(4 downto 0) := (others => '0');
              v.cfg.flush.fetch := '0'; v.cfg.flush.valid := '0'; v.cfg.flush.dirty := '0';
              v.cfg.cen := r.cfg.cen and not p1_wdata(35); -- if set, desable cache 
              v.cfg.flush.mode := "1" & p1_wdata(34 downto 32);
            when "011" => -- flush (direct)
              v.cfg.flush.pending := p1_wdata(0) or p1_wdata(1);
              v.cfg.flush.rst_addr := '1'; -- reset index address in for flush all lines in way
              v.cfg.flush.addr(31 downto 10) := p1_wdata(31 downto 10);
              v.cfg.flush.addr(9 downto 0) := (others => '0');
              v.cfg.flush.addr(3 downto 2) := p1_wdata(5 downto 4);
              v.cfg.flush.fetch := p1_wdata(2) and p1_wdata(9);
              v.cfg.flush.valid := p1_wdata(2) and p1_wdata(8);
              v.cfg.flush.dirty := p1_wdata(2) and p1_wdata(7);
              v.cfg.flush.way := conv_integer(p1_wdata(5 downto 4));
              v.cfg.cen := r.cfg.cen and not p1_wdata(3); -- if set, desable cache
              v.cfg.flush.mode := "0" & p1_wdata(2 downto 0); -- 001: invalidate, 010: write back, 011: invalidate & write back, 1..: flush all
            when "100" => -- access counter
              if stat >= 1 then
                v.stat.acount(1) := r.stat.acount(0); v.stat.acount(0) := (others => '0');
                v.stat.hcount(1) := r.stat.hcount(0); v.stat.hcount(0) := (others => '0');
              end if;
            when "101" => -- hit counter
              if stat >= 1 then
                v.stat.acount(1) := r.stat.acount(0); v.stat.acount(0) := (others => '0');
                v.stat.hcount(1) := r.stat.hcount(0); v.stat.hcount(0) := (others => '0');
              end if;
            when "110" => -- bus cycle counter
              if stat = 2 then
                v.stat.ccount(1) := r.stat.ccount(0); v.stat.ccount(0) := (others => '0');
                v.stat.ucount(1) := r.stat.ucount(0); v.stat.ucount(0) := (others => '0');
              end if;
            when "111" => -- bus usage counter
              if stat = 2 then
                v.stat.ccount(1) := r.stat.ccount(0); v.stat.ccount(0) := (others => '0');
                v.stat.ucount(1) := r.stat.ucount(0); v.stat.ucount(0) := (others => '0');
              end if;
            when others =>
          end case;
        else  -- p1_reg_addr(5) = '1'
          case p1_reg_addr(4 downto 2) is
            when "000" => -- err ctrl/status
              v.cfg.err.diseresp := p1_wdata(115);
              v.cfg.err.mask := p1_wdata(107 downto 104);
              v.cfg.selectcb := p1_wdata(103 downto 102); 
              v.cfg.selecttcb := p1_wdata(101 downto 100); 
              v.cfg.xcb := p1_wdata(99); 
              v.cfg.rcb := p1_wdata(98); 
              v.cfg.err.comp := p1_wdata(97); 
              v.cfg.err.rst := p1_wdata(96);
            when "010" => -- Tag cb
              v.cfg.wtcb := p1_wdata(38 downto 32);
            when "011" => -- Data cb
              v.cfg.wcb := p1_wdata(27 downto 0);
            when "100" => -- Scrub ctrl
              v.cfg.scrub.index := p1_wdata(r.cfg.scrub.index'left-r.cfg.scrub.index'right+112 downto 112);
              p1_scrubwayv(conv_integer(p1_wdata(101 downto 100))) := '1';
              v.cfg.scrub.wayv := p1_scrubwayv(v.cfg.scrub.wayv'left downto v.cfg.scrub.wayv'right);
              v.cfg.scrub.pending := p1_wdata(97);
              v.cfg.scrub.en := p1_wdata(96);
            when "101" => -- Scrub delay
              v.cfg.scrub.delay := p1_wdata(79 downto 64);
            when "110" => -- Error injection
              v.cfg.err.inject_addr := p1_wdata(63 downto 34);
              v.cfg.err.inject_pending := p1_wdata(32);
            when "111" => -- Access control
              v.cfg.split := p1_wdata(1 downto 0);
            --when "111" => -- DEBUG
            --  v.cfg.debug_delay := r.p1.hwdata(15 downto 0);
            when others =>
          end case;
        end if;
      else   -- p1_reg_addr(7) = '1'
        case p1_reg_addr(3 downto 2) is
          when "00" =>
            v.cfg.mtr(conv_integer(p1_reg_addr(6 downto 4) & "00") + 0) := p1_wdata(127 downto 96);
          when "01" =>
            v.cfg.mtr(conv_integer(p1_reg_addr(6 downto 4) & "00") + 1) := p1_wdata(95 downto 64);
          when "10" =>
            v.cfg.mtr(conv_integer(p1_reg_addr(6 downto 4) & "00") + 2) := p1_wdata(63 downto 32);
          when others =>
            v.cfg.mtr(conv_integer(p1_reg_addr(6 downto 4) & "00") + 3) := p1_wdata(31 downto 0);
        end case;
      end if;
    end if;
    
    -- Update BE bus size: check for valid size
    if r.cfg.updbesize = "100" or r.cfg.updbesize = "011" or r.cfg.updbesize(2 downto 0) = "010" then 
      v.cfg.besize := r.cfg.updbesize;
    end if;
              



  -- P0 (TAG readout) ---------------------------------------------------------------
    p0_hold := p1_hold; 

    if p1_hold = '0' then
     
      if r.p0.flush.pending = '1' then

        v.p0.delstate.acc.valid := '1';
        v.p0.delstate.acc.addr := r.p0.flush.addr;
        v.p0.delstate.flush.pending := '1';
        v.p0.delstate.flush.lookup  := conv_std_logic(r.cfg.flush.mode(3 downto 2) = "10");
        v.p0.delstate.flush.fetch   := r.cfg.flush.fetch;
        v.p0.delstate.flush.tag_upd := r.cfg.flush.fetch; 
        v.p0.delstate.flush.inv     := r.cfg.flush.mode(0);
        v.p0.delstate.flush.wb      := r.cfg.flush.mode(1);
        v.p0.delstate.flush.valid   := r.cfg.flush.valid;
        v.p0.delstate.flush.dirty   := r.cfg.flush.dirty;
        
        v.p0.delstate.acc.accid   := (others => '0');
        v.p0.delstate.acc.size    := (others => '0');
        v.p0.delstate.acc.master  := (others => '0');
        v.p0.delstate.acc.mbsel   := (others => '0');
        v.p0.delstate.acc.reissued:= '0';
        v.p0.delstate.acc.reissue := '0';
        v.p0.delstate.acc.locked  := '0';
        v.p0.delstate.acc.write   := '0';

        -- flush ways
        v.p0.delstate.flush.wayv := (others => '0');
        for i in WAYRANGE loop
          if i < ways-conv_integer(r.cfg.lock) then -- Do not flush locked way with "flush all"
            if r.cfg.flush.mode(2) = '1' or r.cfg.flush.mode(3) = '1' or i = r.cfg.flush.way then
              v.p0.delstate.flush.wayv(i) := '1';
            else
              v.p0.delstate.flush.wayv(i) := '0';
            end if;
          else
            v.p0.delstate.flush.wayv(i) := '0';
          end if;
        end loop;
        
        -- Flush all counter
        v.p0.flush.addr(INDEXRANGE) := r.p0.flush.addr(INDEXRANGE) + 1;
        if ((r.p0.flush.addr(INDEXRANGE'left) and not v.p0.flush.addr(INDEXRANGE'left)) = '1') or
           (r.cfg.flush.mode(2) = '0')   
        then
          v.p0.flush.pending := '0';
        end if;
      
      else
        v.p0.delstate.acc.valid := '0';
      end if;               
    
    --elsif r.p0.scrub.pending = '1' then

    --elsif r.p0.inject.pending = '1' then
    
    end if;                 
        
    if r.p0.delstate.acc.valid = '1' then
      p0_hold := '1';
    end if;

    -- store access
    if p1_hold = '0' and r.p0.delstate.acc.valid = '1' then
      v.p0.state := r.p0.delstate;
    elsif p0_hold = '0' then
      v.p0.state.acc  := fetop.acc;
      v.p0.state.flush.pending  := '0';
      v.p0.state.flush.lookup   := '0';
      v.p0.state.flush.fetch    := '0';
      v.p0.state.flush.tag_upd  := '0';
      v.p0.state.flush.inv      := '0';
      v.p0.state.flush.wb       := '0';
      v.p0.state.flush.valid    := '0';
      v.p0.state.flush.dirty    := '0';
      v.p0.state.flush.wayv     := (others => '0');
    end if;

    -- FLUSH controller (Setup)
    if r.cfg.flush.pending = '1' then
      v.cfg.flush.pending := '0';
      v.p0.flush.pending  := '1';
      v.p0.flush.addr     := r.cfg.flush.addr;
    end if;
    -- SCRUB controller
    -- Error Injection

    -- TAG RAM access
    if r.p0.delstate.acc.valid = '1' or v.p2.newtag.tag_upd = '1' then
      if v.p2.newtag.tag_upd = '1' then         -- TAG update
        p0_tindex := v.p2.newtag.index;
        p0_twen   := v.p2.newtag.tag_wayv;
        p0_tren   := v.p2.newtag.tag_wayv;
      else                                      -- delayed acc: flush, scrub, inject
        p0_tindex := r.p0.delstate.acc.addr(INDEXRANGE);
        p0_tren   := (others => r.p0.delstate.acc.valid);
        p0_twen   := (others => '0');
      end if;      
    else                                        -- AHB access
      p0_tindex := fetop.acc.addr(INDEXRANGE);
      p0_tren   := (others => fetop.acc.valid);
      p0_twen   := (others => '0');
    end if;
    p0_newtag_tag := v.p2.newtag.tag;
    p0_newtag_cb  := v.p2.newtag.cb;
  
  -- Cache RAM connection -----------------------------------------------------------
    -- Scan test support
    if scantest = 1 then
      crami.testen <= fetop.testen;
      crami.scanen <= fetop.scanen;
      scanen := fetop.testen and fetop.scanen;
    else
      crami.testen <= '0';
      crami.scanen <= '0';
      scanen := '0';
    end if;
    crami.testin <= fetop.testin;

    -- TAG 
    crami.twrite <= (others => '0');
    if scanen = '0' then
      crami.tenable(MAX_WAYRANGE) <= p0_tren;
      crami.twrite(MAX_WAYRANGE)  <= p0_twen;
    end if;
    crami.tdwrite <= (p2_vdlwen and not scanen);
    -- pragma translate_off
    crami.lru <= (others => 'L');
    -- pragma translate_on
    crami.lru(LRURANGE) <= v.p2.newtag.lru(LRURANGE);
    -- pragma translate_off
    crami.taddr <= (others => 'L');
    -- pragma translate_on
    --if orv(v.p3.twen) = '1' then 
      crami.taddr(TDEPTH-1 downto 0) <= p0_tindex; -- v.p3.addr(INDEXRANGE);
    --else
    --  crami.taddr(TDEPTH-1 downto 0) <= v.p0.haddr(INDEXRANGE);
    --end if;
    -- pragma translate_off
    crami.twaddr <= (others => 'L');
    -- pragma translate_on
    crami.twaddr(TDEPTH-1 downto 0) <= p2_vdlwindex; --v.p3.addr(INDEXRANGE);
    for i in 0 to ways-1 loop
      -- pragma translate_off
      crami.way(i).tag <= (others => 'L');
      -- pragma translate_on
      crami.way(i).tag(TAGRANGE) <= p0_newtag_tag;
      crami.way(i).tcb <= p0_newtag_cb;

      crami.way(i).valid(VALIDRANGE) <= v.p2.newtag.vd_bits(i).valid(VALIDRANGE);
      crami.way(i).dirty(DIRTYRANGE) <= v.p2.newtag.vd_bits(i).dirty(DIRTYRANGE);

      if linesize /= 64 then
        crami.way(i).valid(3 downto 2) <= (others => '0');
        crami.way(i).dirty(3 downto 2) <= (others => '0');
      end if;
    end loop;

    -- DATA
    crami.dway  <= 0;
    for i in 0 to ways-1 loop
      if v.p2.dacc.wayv(i) = '1' and scanen = '0' then
        crami.denable(i) <= v.p2.dacc.ben;
        crami.dwrite(i) <= v.p2.dacc.wen;
        crami.dway  <= i;
      else
        crami.denable(i) <= (others => '0');
        crami.dwrite(i) <= (others => '0');
      end if;
    end loop;
    
    crami.daddr           <= zero32(MAX_ADDR_BITS-1 downto INDEXRANGE'left-INDEXRANGE'right+1) & v.p2.dacc.index(INDEXRANGE);
    crami.dindex          <= zero32(TAGRANGE) & v.p2.dacc.index(INDEXRANGE) & zero32(OFFSETRANGE);
    crami.doffset         <= zero32(31 downto OFFSETRANGE'left+1) & v.p2.dacc.offset & zero32(CLOFFSETRANGE'right-1 downto 0);
    crami.data(DATARANGE) <= v.p2.dacc.data;
    crami.cb(DATACBRANGE) <= v.p2.dacc.cb;

  -- Reset --------------------------------------------------------------------------
    if rst = '0' then
      v.p5.state.acc.valid    := '0'; 
      v.p5.state.flush.pending:= '0';
      v.p5.state.scrub        := '0'; 
      v.p5.state.inject       := '0';
      v.p5.state.diagt        := '0';
      v.p5.state.diagd        := '0';
      v.p5.state.reg          := '0';
      v.p5.state.rmw          := '0';
      v.p4.state.acc.valid    := '0'; 
      v.p4.state.flush.pending:= '0';
      v.p4.state.scrub        := '0'; 
      v.p4.state.inject       := '0';
      v.p4.state.diagt        := '0';
      v.p4.state.diagd        := '0';
      v.p4.state.reg          := '0';
      v.p4.state.rmw          := '0';
      v.p4.fe_dvalid          := '0';
      v.p3.state.acc.valid    := '0'; 
      v.p3.state.flush.pending:= '0';
      v.p3.state.scrub        := '0'; 
      v.p3.state.inject       := '0';
      v.p3.state.diagt        := '0';
      v.p3.state.diagd        := '0';
      v.p3.state.reg          := '0';
      v.p3.state.rmw          := '0';
      v.p2.state.acc.valid    := '0'; 
      v.p2.state.flush.pending:= '0';
      v.p2.state.scrub        := '0'; 
      v.p2.state.inject       := '0';
      v.p2.state.diagt        := '0';
      v.p2.state.diagd        := '0';
      v.p2.state.reg          := '0';
      v.p2.state.rmw          := '0';
      v.p2.drw.pending        := (others => '0');
      v.p2.facc.valid         := '0';
      v.p2.facc.done          := '0';
      v.p2.facc.pending       := (others => '0');
      v.p1.state.acc.valid    := '0'; 
      v.p1.state.flush.pending:= '0';
      v.p1.state.scrub        := '0'; 
      v.p1.state.inject       := '0';
      v.p1.state.diagt        := '0';
      v.p1.state.diagd        := '0';
      v.p1.state.reg          := '0';
      v.p1.state.rmw          := '0';
      v.p1.deltvalid          := '0';
      v.p0.state.acc.valid    := '0'; 
      v.p0.state.flush.pending:= '0';
      v.p0.state.scrub        := '0'; 
      v.p0.state.inject       := '0';
      v.p0.state.diagt        := '0';
      v.p0.state.diagd        := '0';
      v.p0.state.reg          := '0';
      v.p0.state.rmw          := '0';
      v.p0.delstate.acc.valid := '0'; 
      v.p0.delstate.flush.pending:= '0';
      v.p0.delstate.scrub        := '0';
      v.p0.delstate.inject       := '0';
      v.p0.delstate.diagt        := '0';
      v.p0.delstate.diagd        := '0';
      v.p0.delstate.reg          := '0';
      v.p0.delstate.rmw          := '0';

      v.p0.flush.pending := '0';
      v.p0.flush.addr := (others => '0');
      
      v.cfg.cen           := conv_std_logic(cen = 1);
      v.cfg.hproten       := '0';
      v.cfg.hpbufen       := '0';
      v.cfg.hprhbypass    := '0';
      v.cfg.wp            := '0';
      v.cfg.edacen        := '1';
      v.cfg.rcb           := '0';
      v.cfg.xcb           := '0';
      v.cfg.selectcb      := (others => '0');
      v.cfg.wcb           := (others => '0');
      v.cfg.selecttcb     := (others => '0');
      v.cfg.wtcb          := (others => '0');
      v.cfg.err.rst       := '1';
      v.cfg.stat_mode     := (others => '0');
      v.cfg.lock          := (others => '0');
      v.cfg.repl          := (others => '0');
      v.cfg.index_rep     := (others => '0');
      v.cfg.flush.pending := '0';
      for i in 0 to mtrr loop 
        v.cfg.mtr(i)(1 downto 0) := (others => '0'); 
        
        v.cfg.mtr(i) := (others => '0');
      end loop;
      v.cfg.scrub.pending := '0';
      v.cfg.scrub.en      := '0';
      
      if bbuswidth = 32 then
        v.cfg.besize := "010";
        v.cfg.updbesize := "010";
      elsif bbuswidth = 64 then
        v.cfg.besize := "011";
        v.cfg.updbesize := "110";
      else 
        v.cfg.besize := "100";
        v.cfg.updbesize := "100";
      end if;

      v.cfg.split := "00";
    end if;

  
  
    rin <= v;

    -- Frontend output
    ptofe.hold      <= p0_hold;
    ptofe.cfg_split <= r.cfg.split;

    -- Backend output
    ptobe.cfg_besize <= r.cfg.besize;

  end process comb;

  regs : process(clk)
  begin
    if rising_edge(clk) then r <= rin; end if;
  end process regs;
end;


