------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-----------------------------------------------------------------------------   
-- Entity:      l2cfe
-- File:        l2cfe.vhd
-- Author:      Nils Johan Wessman - Aeroflex Gaisler
-- Description: Front-end for the L2-cache PIPE
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

entity l2cfe is
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
    ahbsi   : in  ahb_slv_in_type;
    ahbso   : out ahb_slv_out_type;
    ahbsov  : in  ahb_slv_out_vector;
    fetop   : out l2c_fe_to_pipe_type;
    ptofe   : in  l2c_pipe_to_fe_type;
    fetobe  : out l2c_fe_to_be_type;
    betofe  : in  l2c_be_to_fe_type;
    debugo  : out std_logic_vector(255*debug downto 0)
);
end;

architecture rtl of l2cfe is
  constant REVISION : integer := 0;
  constant iomask : integer := 16#FFC#;
  constant bioarea : integer := bioaddr;
  constant ffact : integer := 1;
  constant UP : boolean := true;
  constant hconfig : ahb_config_type := (
    0 => ahb_device_reg ( VENDOR_GAISLER, GAISLER_L2CACHE, 0, REVISION, hirq),
    1 => (zero32(31 downto 9) & conv_std_logic(UP) & conv_std_logic_vector(ffact, 4) & conv_std_logic_vector(mbus, 2) & conv_std_logic_vector(sbus, 2)),
    2 => (conv_std_logic_vector(bioarea, 12) & zero32(19 downto 0)),
    4 => ahb_membar(haddr, '1', '1', hmask),
    5 => ahb_membar(ioaddr, '0', '0', iomask),
    6 => ahb_membar(bioaddr, '0', '0', biomask),
    others => zero32);

  subtype CLOFFSETRANGE is natural range log2(linesize)-1 downto log2(CRAMDW/8);  -- Cache Line offset range
  subtype DATARANGE is natural range CRAMDW-1 downto 0;
  subtype VALIDRANGE is natural range (linesize/16)-1 downto 0;
  constant LINEBITS : integer := log2(linesize);
  constant ACC_FIFO_DEPTH     : integer := 4;
  constant DEL_ACC_FIFO_DEPTH : integer := 4;
  constant MISS_ACC_FIFO_DEPTH: integer := 4;
  constant HIT_ACC_FIFO_DEPTH : integer := 4;
  constant MAX_AHBMST         : integer := 4;
  
  type l2c_fe_rbuf_vector_type is array (natural range <> ) of std_logic_vector(DATARANGE);
  type l2c_fe_data_buf_type is record
    valid  : std_logic_vector(3 downto 0); 
    err    : std_logic_vector(3 downto 0);
    offset : std_logic_vector(CLOFFSETRANGE);
    data   : l2c_fe_rbuf_vector_type(3 downto 0);
  end record;
  type l2c_fe_data_buf_vector is array (natural range <> ) of l2c_fe_data_buf_type;

  type l2c_fe_acc_type is record
    valid       : std_logic;
    accid       : std_logic_vector(3 downto 0);
    acc_hold    : std_logic;
    acc_split   : std_logic_vector(1 downto 0);
    haddr       : std_logic_vector(31 downto 0);
    hsize       : std_logic_vector(2 downto 0);
    hmaster     : std_logic_vector(3 downto 0);
    hmastlock   : std_logic;
    hwrite      : std_logic;
    hprot       : std_logic_vector(3 downto 0);
    hmbsel      : std_logic_vector(0 to 2);
    reissue     : std_logic;
    reissued    : std_logic;
    hwdata      : std_logic_vector(DATARANGE);
  end record;
  type l2c_fe_acc_vector_type is array (natural range <> ) of l2c_fe_acc_type; 
  type l2c_fe_cur_acc_type is record
    valid       : std_logic;
    accid       : std_logic_vector(3 downto 0);
    split       : std_logic;
    active      : std_logic;
    haddr       : std_logic_vector(31 downto 0);
    hmaster     : std_logic_vector(3 downto 0);
    nxt         : std_logic;                            -- reissue next in miss_acc
    bypass      : std_logic;
    -- Data
    data        : l2c_fe_data_buf_type;
    data_src    : std_logic;
    data_done   : std_logic;
    data_index  : std_logic_vector(3 downto 0);
    data_slice  : std_logic_vector(3 downto 0);
  end record;
  type l2c_fe_hit_acc_type is record
    valid       : std_logic;
    accid       : std_logic_vector(3 downto 0);
    acc_split   : std_logic;
    hmaster     : std_logic_vector(3 downto 0);
    --
    haddr       : std_logic_vector(31 downto 0);
    --
    data_index  : std_logic_vector(3 downto 0);
    data_slice  : std_logic_vector(3 downto 0);
  end record;
  type l2c_fe_hit_acc_vector_type is array (natural range <> ) of l2c_fe_hit_acc_type; 
  type l2c_fe_split_buf_type is record
    valid   : std_logic;
    hmaster : std_logic_vector(3 downto 0);
  end record;
  type l2c_fe_split_buf_vector_type is array (0 to MAX_AHBMST-1) of l2c_fe_split_buf_type; 
  type l2c_fe_reg_type is record
    -- AHB
    hready  : std_logic;
    htrans  : std_logic_vector(1 downto 0);
    haddr   : std_logic_vector(31 downto 0);
    hsize   : std_logic_vector(2 downto 0);
    hmaster : std_logic_vector(3 downto 0);
    hwrite  : std_logic;
    hresp   : std_logic_vector(1 downto 0);
    hirq    : std_logic_vector(31 downto 0);
    hsplit  : std_logic_vector(15 downto 0);
    hwdata  : std_logic_vector(127 downto 0);

    acc_active  : std_logic;
    acc_hold    : std_logic;

    acc_index     : std_logic_vector(3 downto 0);
    acc           : l2c_fe_acc_vector_type(ACC_FIFO_DEPTH-1 downto 0);
    acc_0_int     : l2c_fe_acc_type; 
    del_acc_index : std_logic_vector(3 downto 0);
    del_acc       : l2c_fe_acc_vector_type(DEL_ACC_FIFO_DEPTH-1 downto 0);
    miss_acc_index: std_logic_vector(3 downto 0);
    miss_acc      : l2c_fe_acc_vector_type(MISS_ACC_FIFO_DEPTH-1 downto 0);
    miss_acc_done : std_logic;
    miss_acc_wait : std_logic;
    
    hit_acc_index : std_logic_vector(3 downto 0);
    hit_acc_nxt_index : std_logic_vector(3 downto 0);
    hit_acc_rbuf_index : std_logic_vector(3 downto 0);
    hit_acc       : l2c_fe_hit_acc_vector_type(HIT_ACC_FIFO_DEPTH-1 downto 0);
    
    cur_acc       : l2c_fe_cur_acc_type; 
    cur_miss_acc  : l2c_fe_cur_acc_type; -- current missed access
    cur_lock_acc  : l2c_fe_cur_acc_type; -- current locked access

    hit_dbuf      : l2c_fe_data_buf_vector(3 downto 0);
   
    split_buf     : l2c_fe_split_buf_vector_type;
    usplit_valid  : std_logic;

    lock_pen      : std_logic;

    resp_sec    : std_logic;
    newacc      : std_logic;

    valid   : std_logic;
    accid   : std_logic_vector(3 downto 0);
  end record;

  signal r,rin : l2c_fe_reg_type;

begin

  comb : process(rst, r, ahbsi, ptofe, betofe)
  variable v       : l2c_fe_reg_type;
  variable slvacc  : std_logic;
  variable split   : std_logic_vector(1 downto 0);
  variable hrdata  : std_logic_vector(127 downto 0);

  variable pipe_acc  : l2c_access_type;
  variable pipe_wdata: std_logic_vector(127 downto 0);
  
  variable pipe_hold     : std_logic;  -- PIPE busy
  variable hit_hold      : std_logic;  -- Hit buffer full
  variable miss_hold     : std_logic;  -- Miss buffer full
  variable issue_hold    : std_logic;  -- Hold issuing of new accesses to PIPE
  variable acc_hold      : std_logic;  -- Hold issuing new AHB accesses
  variable del_hold      : std_logic;  -- Access in delay-buffer
  variable del_full      : std_logic;  -- Delay-buffer full
  variable split_hold    : std_logic;  -- Access in split-buffer

  variable miss_done        : std_logic;  -- 
  variable pipe_facc_busy   : std_logic;  -- 
  variable reissue_miss     : std_logic;  -- PIPE should reissue a miss
  variable miss_acc_shift   : std_logic;
  variable miss_hold_cnt    : integer range 0 to MISS_ACC_FIFO_DEPTH;
  
  variable pipe_acc_done    : std_logic;  -- The access has been determine to be a cahce hit or a miss
  variable pipe_acc_hit     : std_logic;  -- The access status (cache hit or miss)
  variable pipe_acc_accid   : std_logic_vector(3 downto 0);  --
  variable pipe_acc_slice   : std_logic_vector(3 downto 0);  -- Valid data slices
  variable pipe_acc_reissue : std_logic;  -- The access need to be reissued
  variable pipe_acc_locked  : std_logic;  -- Is a locked access
  variable pipe_acc_bypass  : std_logic;  -- Is a locked access
  --
  variable pipe_data          : std_logic_vector(DATARANGE);  -- 
  variable pipe_data_valid    : std_logic;  -- Cache read out data valid
  variable pipe_data_done     : std_logic;  -- Cache read out data done (last data)
  variable pipe_data_offset   : std_logic_vector(CLOFFSETRANGE);  -- Cache read out data done (last data)
  variable pipe_data_err      : std_logic;  -- Cache data error
  variable pipe_data_reissued : std_logic;  -- Cache data for a reissued (earlier miss) access
  variable pipe_data_locked   : std_logic;  -- Cache data for a locked access
  
  variable be_data          : std_logic_vector(DATARANGE);  -- 
  variable be_data_valid    : std_logic;  -- The backend data has been stored ( or beeing read out)
  variable be_data_done     : std_logic;  -- Cache read out data done (last data)
  variable be_data_offset   : std_logic_vector(CLOFFSETRANGE);  -- Cache read out data done (last data)
  variable be_data_err      : std_logic;  -- Backend data error
  variable be_data_locked   : std_logic;  -- Backend data for a locked access

  variable hit_acc_rbuf_index : std_logic_vector(3 downto 0);
  variable hit_acc_full  : std_logic;
  variable hit_acc_afull : std_logic;
  variable hit_acc_cnt   : integer range 0 to HIT_ACC_FIFO_DEPTH;
  
  variable data_available       : std_logic;  -- Read data available
  variable hit_data_available   : std_logic;  -- Read data available
  variable miss_data_available  : std_logic;  -- Read data available

  variable store_cache_acc : std_logic;
  variable store_locked_acc: std_logic;

  variable store_del_acc   : std_logic;
  variable issue_del_acc   : std_logic;
  variable store_hit_acc   : std_logic;
  variable store_miss_acc  : std_logic;
  variable issue_miss_acc  : std_logic;

  variable split_buf_shift  : std_logic;
  variable split_buf_match  : std_logic;

  variable last                 : std_logic;
  variable switch, switch_miss  : std_logic;
  variable hwdata128            : std_logic_vector(127 downto 0); 

  begin
    v := r;
  -- AHB slave interface(s) ---------------------------------------------------------
    v.hready := '1'; v.hresp := HRESP_OKAY; v.hsplit := (others => '0');
    v.resp_sec := '0';
    v.newacc := '0';
    slvacc := '0';
    -- split = "10": split on miss, "01": split always (not supported), "00": wait-states; 
    split := ptofe.cfg_split; 
    
    pipe_acc_done     := ptofe.p2_valid;
    pipe_acc_hit      := ptofe.p2_hit;
    pipe_acc_reissue  := ptofe.p2_reissue;
    pipe_acc_accid    := ptofe.p2_accid;
    pipe_acc_slice    := ptofe.p2_slice;
    pipe_acc_locked   := ptofe.p2_locked;
    pipe_acc_bypass   := ptofe.p2_bypass;
    
    pipe_data         := ptofe.data;
    pipe_data_valid   := ptofe.data_valid;
    pipe_data_done    := ptofe.data_done and ptofe.data_valid;
    pipe_data_offset  := ptofe.data_offset(CLOFFSETRANGE);
    pipe_data_err     := ptofe.data_err;
    pipe_data_reissued:= ptofe.data_reissued;
    pipe_data_locked  := ptofe.data_locked;

    be_data           := betofe.data;
    be_data_valid     := betofe.data_valid;
    be_data_done      := betofe.data_done and betofe.data_valid;
    be_data_offset    := betofe.data_offset(CLOFFSETRANGE);
    be_data_err       := betofe.data_err;
    be_data_locked    := betofe.data_locked;

    -- PIPE busy
    pipe_hold         := ptofe.hold;
    
    -- Hit buffer full
    hit_hold          := (r.acc(conv_integer(r.acc_index)).valid and r.hit_acc(1).valid) or r.hit_acc(2).valid;     --        extend, 
                                                                                                                    --        depends on the number of accesses already issued to the PIPE, 
                                                                                                                    --        use correct hit-buffer-full    
    -- Miss buffer full
    miss_hold_cnt := 0;
    for i in 0 to MISS_ACC_FIFO_DEPTH-1 loop
      if r.miss_acc(i).valid = '1' then miss_hold_cnt := miss_hold_cnt + 1; end if;
    end loop;
    miss_hold         := (r.acc(conv_integer(r.acc_index)).valid and conv_std_logic(miss_hold_cnt >= 2)) or conv_std_logic(miss_hold_cnt >= 3);   -- extend, 
                                                                                                                    --        depends on the number of accesses already issued to the PIPE, 
                                                                                                                    --        use correct miss-buffer-full

    -- Hold new PIPE accesses
    issue_hold        := pipe_hold or hit_hold or miss_hold or 
                         (r.acc(0).valid and r.acc(0).acc_hold and r.acc_0_int.valid);

    -- Hold due to delayed accesses
    del_hold          := r.del_acc(0).valid;
    del_full          := r.del_acc(0).valid;--r.del_acc(2).valid;

    split_hold        := '0';
    for i in 0 to MAX_AHBMST-1 loop
      split_hold := split_hold or r.split_buf(i).valid;
    end loop;

    if ptofe.p2_fdone = '1' then 
      v.miss_acc_done := '1';
      v.miss_acc_wait := '0';
    end if;
    miss_done         := (ptofe.p2_fdone or r.miss_acc_done);
    pipe_facc_busy    := ptofe.p2_fbusy;

    issue_miss_acc := '0';
    if (miss_done = '1' or r.miss_acc(0).valid = '1') and r.miss_acc_wait = '0' and (r.cur_miss_acc.nxt or not r.cur_miss_acc.valid) = '1' then
      if r.miss_acc(0).valid = '1' then
        if pipe_hold = '0' and pipe_facc_busy = '0' then
          issue_miss_acc := '1';
          v.miss_acc_done := '0';
        end if;
      elsif miss_hold_cnt = 0 then
        v.miss_acc_done := '0';
      end if;
    end if;

    -- Hold new AHB accesses
    acc_hold          := issue_hold or del_hold or (r.acc(0).valid and r.acc(0).acc_hold) or
                         (issue_miss_acc); -- issue_miss_acc
    v.acc_hold := acc_hold;
    
    store_hit_acc := '0';
    if (pipe_acc_done and pipe_acc_hit) = '1' and           
       r.acc(conv_integer(r.acc_index)).hwrite = '0' and    
       r.acc(conv_integer(r.acc_index)).reissued = '0' --and
    then 
      store_hit_acc := '1'; 
    end if;
    --
    store_miss_acc := '0';
    if (pipe_acc_done and pipe_acc_reissue) = '1'
    then
      store_miss_acc := '1';
    end if;
    
    store_locked_acc := '0';
    if (pipe_acc_done and pipe_acc_locked) = '1' 
    then
      store_locked_acc := '1';
    end if;
    --
    store_del_acc := '0';
    if r.acc(0).valid = '1' and r.acc(0).acc_hold = '1' and 
       r.acc(0).acc_split /= "11" and
       (issue_hold = '1' or del_hold = '1' or issue_miss_acc = '1' or r.acc_0_int.valid = '1') then 
      store_del_acc := '1';
    end if;
    --
    issue_del_acc := '0';
    if r.del_acc(0).valid = '1' and issue_hold = '0' and issue_miss_acc = '0' 
    then
      issue_del_acc := '1';
    end if;
    --
    store_cache_acc := '0';
    if (r.acc(0).valid = '1' and r.acc(0).acc_split /= "11" and             
        (r.acc(0).acc_hold = '0' or issue_hold = '0') and 
        (r.acc(0).acc_hold and (issue_del_acc or issue_miss_acc)) = '0') or 
       r.acc_0_int.valid = '1'
    then
      store_cache_acc := '1';
    end if;

    if linesize = 64 then 
      -- Last (word) in cacheline 
      if r.acc(0).hsize = "100" then   -- 128 bit
        last := r.acc(0).haddr(LINEBITS-1) and r.acc(0).haddr(LINEBITS-2); 
      elsif r.acc(0).hsize= "011" then --  64 bit
        last := r.acc(0).haddr(LINEBITS-1) and r.acc(0).haddr(LINEBITS-2) and r.acc(0).haddr(LINEBITS-3);  
      else                      -- not 128-bit
        last := r.acc(0).haddr(LINEBITS-1) and r.acc(0).haddr(LINEBITS-2) and r.acc(0).haddr(LINEBITS-3) and r.acc(0).haddr(LINEBITS-4);  
      end if;
      
      -- Last (word) in cacheline 
      if r.acc(0).hsize = "100" then   -- 128 bit
        switch := '1'; 
      elsif r.acc(0).hsize= "011" then --  64 bit
        switch := r.acc(0).haddr(LINEBITS-1-2);  
      else                      -- not 128-bit
        switch := r.acc(0).haddr(LINEBITS-1-2) and r.acc(0).haddr(LINEBITS-1-3);  
      end if;
    else -- linesize = 32
      -- Last (word) in cacheline 
      if r.acc(0).hsize = "100" then   -- 128 bit
        last := r.acc(0).haddr(LINEBITS-1); 
      elsif r.acc(0).hsize= "011" then --  64 bit
        last := r.acc(0).haddr(LINEBITS-1) and r.acc(0).haddr(LINEBITS-2);  
      else                      -- not 128-bit
        last := r.acc(0).haddr(LINEBITS-1) and r.acc(0).haddr(LINEBITS-2) and r.acc(0).haddr(LINEBITS-3);  
      end if;
      
      -- Last (word) in cacheline 
      if r.acc(0).hsize = "100" then   -- 128 bit
        switch := '1'; 
      elsif r.acc(0).hsize= "011" then --  64 bit
        switch := r.acc(0).haddr(LINEBITS-2);  
      else                      -- not 128-bit
        switch := r.acc(0).haddr(LINEBITS-2) and r.acc(0).haddr(LINEBITS-3);  
      end if;
    end if;

    if switch = '1' and r.cur_acc.data_slice( conv_integer( (r.acc(0).haddr(CLOFFSETRANGE) + 1) ) ) = '0' then
      switch_miss := '1';
    else
      switch_miss := '0';
    end if;

    if ptofe.cfg_cen = '0' or r.acc(0).hmbsel(2) = '1' or (r.cur_miss_acc.active and r.cur_miss_acc.bypass) = '1' then
      switch := '1';
      switch_miss := '1';
    end if;

    -- AHB write data mux
    hwdata128(127 downto 96) := ahbsi.hwdata(127 mod AHBDW downto 96 mod AHBDW);
    hwdata128( 95 downto 64) := ahbsi.hwdata( 95 mod AHBDW downto 64 mod AHBDW);
    hwdata128( 63 downto 32) := ahbsi.hwdata( 63 mod AHBDW downto 32 mod AHBDW);
    hwdata128( 31 downto  0) := ahbsi.hwdata( 31           downto  0);

    if r.newacc = '1' and (r.cur_acc.active or r.cur_miss_acc.active) = '0'  then
      v.accid := r.accid + 1;
    end if;
  
    
    -- Current access done (move to next)
    v.cur_acc.nxt := '0';
    if (ahbsi.htrans(0) = '0' or (last or switch_miss) = '1') and r.hready = '1' then
      if (r.cur_acc.valid and r.cur_acc.active) = '1' then
        v.cur_acc.valid := '0';
        v.cur_acc.active := '0';
        v.cur_acc.nxt := '1';
      end if;
      if (r.cur_miss_acc.valid and r.cur_miss_acc.active) = '1' then
        v.cur_miss_acc.valid := '0';
        v.cur_miss_acc.active := '0';
        v.cur_miss_acc.nxt := '1';
      end if;
    end if;

    reissue_miss := (r.cur_miss_acc.valid and not (r.cur_miss_acc.nxt or v.cur_miss_acc.nxt)) or r.miss_acc_wait or (pipe_acc_done and pipe_acc_reissue);
    for i in 0 to MISS_ACC_FIFO_DEPTH-1 loop
      if r.miss_acc(i).valid = '1' then reissue_miss := '1'; end if;
    end loop;

    -- AHB slave access
    split_buf_match := '0';
    if (ahbsi.hready and ahbsi.hsel(hslvidx) and ahbsi.htrans(1)) = '1' then
      slvacc := '1';     
      if ahbsi.htrans(0) = '0' or ahbsi.hwrite = '1' or (last or switch_miss) = '1' then
        v.newacc := '1';
      end if;
      v.acc(0).haddr    := ahbsi.haddr;
      v.acc(0).hsize    := ahbsi.hsize;
      v.acc(0).hmaster  := ahbsi.hmaster;
      v.acc(0).hwrite   := ahbsi.hwrite;
      v.acc(0).hmastlock:= ahbsi.hmastlock;
      v.acc(0).hprot    := ahbsi.hprot;
      v.acc(0).hmbsel   := ahbsi.hmbsel(0 to 2);
    end if;
  
    -- sample AHB bus
    v.acc(0).valid := '0';
    if v.newacc = '1' then
      v.acc(0).valid    := '1';
      v.acc(0).accid    := v.accid;
      v.acc(0).reissued := '0';
      v.acc(0).reissue  := reissue_miss;
      v.acc(0).acc_hold := acc_hold;
      
      
      -- UN-SPLITed master match
      if ahbsi.hmaster = r.cur_acc.hmaster and r.cur_acc.valid = '1' and r.cur_acc.active = '0' then
        v.cur_acc.active := '1';
        v.acc(0).valid    := '0';
      else
        v.cur_acc.active := '0';
      end if;
      
      if ahbsi.hmaster = r.cur_miss_acc.hmaster and r.cur_miss_acc.valid = '1' and 
         r.cur_miss_acc.active = '0' and r.cur_miss_acc.nxt = '0' then
        v.cur_miss_acc.active := '1';
        v.acc(0).valid    := '0';
      else
        v.cur_miss_acc.active := '0';
      end if;

      if ahbsi.hmaster = r.split_buf(0).hmaster and r.split_buf(0).valid = '1' and r.usplit_valid = '1' then
        v.split_buf(0).valid := '0';
        v.usplit_valid := '0';
        split_buf_match := '1';
      end if;
      
      if split /= "00" and (del_full = '1' or (split_hold and not split_buf_match) = '1') and ahbsi.hmastlock = '0' then -- Cache access buffer full, SPLIT
        v.acc(0).acc_split := "11";
      elsif split = "10" then
        v.acc(0).acc_split := "10";
      else
        v.acc(0).acc_split := "00";
      end if;

      -- AHB response
      -- SPLIT
      if (
          (v.cur_acc.active or v.cur_miss_acc.active) = '0' or 
          (((r.cur_acc.active and not v.cur_acc.nxt) and (r.cur_miss_acc.active and not v.cur_miss_acc.nxt)) and (last or switch_miss)) = '1') and
         (
          del_full = '1' or                                   -- Delay-buffer full
          (split_hold and not split_buf_match) = '1' or       -- SPLIT-buffer not empty
          ahbsi.hwrite = '0'                                  -- Reads
         ) then
       if split = "01" or v.acc(0).acc_split = "11" then -- Always split
          v.hready := '0';                                    
          v.hresp := HRESP_SPLIT;
          v.resp_sec := '1';
        else -- Wait-states
          v.hready := '0';
        end if;
      end if;
    end if;
    --hit_data_available  := (r.cur_acc.valid and conv_std_logic(r.cur_acc.data.valid /= zero32(r.cur_acc.data.valid'range))) and (v.cur_acc.active or r.cur_acc.active);
    hit_data_available  := (r.cur_acc.valid and 
                            (conv_std_logic(r.cur_acc.data.valid /= zero32(r.cur_acc.data.valid'range)) or 
                             (pipe_data_valid and not pipe_data_reissued))) and
                           (v.cur_acc.active or r.cur_acc.active);
    --miss_data_available := (r.cur_miss_acc.valid and r.cur_miss_acc.data_done) and (v.cur_miss_acc.active or r.cur_miss_acc.active);
    miss_data_available := (r.cur_miss_acc.valid and 
                            (r.cur_miss_acc.data_done or
                             ((be_data_done and not r.cur_miss_acc.data_src) or (pipe_data_done and r.cur_miss_acc.data_src)))) and 
                           (v.cur_miss_acc.active or r.cur_miss_acc.active);
    data_available      := hit_data_available or miss_data_available;

    if r.hready = '0' then
      if split /= "01" and -- Wait-states
         ((r.acc(0).hwrite = '0' and data_available = '1') or -- Read data available
          (r.acc(0).hwrite = '1' and acc_hold = '0')) then    -- write accepted 
        v.hready := '1';
      elsif (r.acc(conv_integer(r.acc_index)).acc_split = "10" and r.acc(conv_integer(r.acc_index)).hwrite = '0' and (pipe_acc_done and not pipe_acc_hit) = '1') or
            (r.acc(0).acc_split = "10" and r.acc(0).hwrite = '0' and store_del_acc = '1')
      then -- SPLIT on miss
        v.hready := '0';
        v.hresp := HRESP_SPLIT;
        v.resp_sec := '1';
      else
        v.hready := '0';
      end if;
    end if;
    
    if r.resp_sec = '1' then  -- Second response cycle
      v.hready := '1';
      if r.hresp = HRESP_SPLIT then v.hresp := HRESP_SPLIT;
      elsif r.hresp = HRESP_RETRY then v.hresp := HRESP_RETRY;
      else v.hresp := HRESP_ERROR;
      end if;
    end if;

    if (r.acc(0).valid and r.acc(0).hmastlock) = '1' then
      v.lock_pen := '1';
    elsif r.acc(conv_integer(r.acc_index)).hmastlock = '1' and pipe_acc_done = '1' and pipe_acc_reissue = '0' then
      v.lock_pen := '0';
    end if;

    -- Internal acc(0) for reissued and delayed accesses
    v.acc_0_int.valid := '0';
    if issue_miss_acc = '1' and (r.lock_pen = '0' or (r.miss_acc(0).valid and (r.miss_acc(0).hmastlock or r.miss_acc(0).hwrite)) = '1') then -- issue missed access
      v.acc_0_int := r.miss_acc(0);
      v.acc_0_int.reissue := '0';
      v.miss_acc_wait := '1';
    elsif issue_del_acc = '1' then 
      v.acc_0_int := r.del_acc(conv_integer(r.del_acc_index));
      v.acc_0_int.reissue := reissue_miss;
    end if;

    -- cache access buffer
    if store_cache_acc = '1' then
      for i in 1 to ACC_FIFO_DEPTH-1 loop
        v.acc(i) := r.acc(i-1);  
      end loop;

      v.acc(1).hwdata := hwdata128;

      if r.resp_sec = '1' and r.hresp = HRESP_SPLIT then v.acc(1).acc_split := "01"; end if;

      if r.acc_0_int.valid = '1' then
        v.acc(1) := r.acc_0_int;
      end if;

      if pipe_acc_done = '0' then
        v.acc_index := r.acc_index + 1;
      else
        if r.acc_index /= "0011" then 
          v.acc(conv_integer(r.acc_index + 1)).valid := '0';
        end if;
      end if;
    elsif pipe_acc_done = '1' then
      if r.acc_index /= "0000" then
        v.acc_index := r.acc_index - 1;
      end if;
      v.acc(conv_integer(r.acc_index)).valid := '0';
    end if;

    -- delayed access buffer
    if store_del_acc = '1' and (r.acc(0).hwrite = '1' or r.acc(0).hmastlock = '1' or r.acc(0).acc_split /= "10") then
      v.del_acc(0)        := r.acc(0);
      v.del_acc(0).hwdata := hwdata128; 
      if r.acc(0).acc_split = "10" and r.acc(0).hwrite = '0' then -- Mark delayed reads (when split on miss) as splited
        v.del_acc(0).acc_split := "01";
      end if;
      for i in 1 to DEL_ACC_FIFO_DEPTH-1 loop
        v.del_acc(i) := r.del_acc(i-1);  
      end loop;
      if issue_del_acc = '0' and r.del_acc(0).valid = '1' then
        v.del_acc_index := r.del_acc_index + 1;
      else
        v.del_acc(conv_integer(r.del_acc_index + 1)).valid := '0';
      end if;
    elsif issue_del_acc = '1' then
      if r.del_acc_index /= "0000" then
        v.del_acc_index := r.del_acc_index - 1;
      end if;
      v.del_acc(conv_integer(r.del_acc_index)).valid := '0';
    end if;

    -- SPLIT-buffer
    split_buf_shift := '0';
    for i in 1 to MAX_AHBMST-1 loop
      split_buf_shift := split_buf_shift or not r.split_buf(i-1).valid;
      if split_buf_shift = '1' then
        v.split_buf(i-1) := r.split_buf(i);
        v.split_buf(i).valid := '0';
      end if;
    end loop;
    if r.acc(0).valid = '1' and (r.acc(0).acc_split = "11" or (store_del_acc = '1' and r.acc(0).hwrite = '0' and r.acc(0).hmastlock = '0' and r.acc(0).acc_split = "10")) then
      v.split_buf(MAX_AHBMST-1).valid := '1';
      v.split_buf(MAX_AHBMST-1).hmaster := r.acc(0).hmaster;
    end if;
    if del_full = '0' and (miss_hold or hit_hold) = '0' and r.split_buf(0).valid = '1' and r.usplit_valid = '0' then
      v.hsplit(conv_integer(r.split_buf(0).hmaster)) := '1';
      v.usplit_valid := '1';
    end if;

    -- cahce miss access buffer
    miss_acc_shift := issue_miss_acc;
    for i in 1 to MISS_ACC_FIFO_DEPTH-1 loop
      miss_acc_shift := miss_acc_shift or not r.miss_acc(i-1).valid;
      if miss_acc_shift = '1' then
        v.miss_acc(i-1) := r.miss_acc(i);
        v.miss_acc(i).valid := '0';
      end if;
    end loop;
    if r.lock_pen = '1' and issue_miss_acc = '1' and (r.miss_acc(0).valid and not (r.miss_acc(0).hmastlock or r.miss_acc(0).hwrite)) = '1' then
        if (r.miss_acc(0).acc_split(0) xor r.miss_acc(0).acc_split(1)) = '1' then 
          v.hsplit(conv_integer(r.miss_acc(0).hmaster)) := '1';
        end if;
    end if;
    if store_miss_acc = '1' then
      v.miss_acc(MISS_ACC_FIFO_DEPTH-1) := r.acc(conv_integer(r.acc_index));
      v.miss_acc(MISS_ACC_FIFO_DEPTH-1).reissued := '1';
      if r.acc(conv_integer(r.acc_index)).acc_split /= "00" then
        v.miss_acc(MISS_ACC_FIFO_DEPTH-1).acc_split := "01";
      end if;
    end if;

    -- Current missed access
    if     (r.cur_miss_acc.valid = '0' or (miss_done = '1' and r.cur_miss_acc.nxt = '1')) 
       and (((pipe_acc_done and not pipe_acc_hit) = '1' and pipe_acc_reissue = '0') or (pipe_acc_done and r.acc(conv_integer(r.acc_index)).reissued) = '1')
    then
      if r.acc(conv_integer(r.acc_index)).hwrite = '1' then -- write miss
        if pipe_acc_done = '1' and pipe_acc_hit = '0' then
          v.miss_acc_wait := '1'; 
        end if;
      else
        v.cur_miss_acc.valid   := '1';
        v.cur_miss_acc.active  := not split(0) and not split(1);
        v.cur_miss_acc.split   := r.acc(conv_integer(r.acc_index)).acc_split(0) xor r.acc(conv_integer(r.acc_index)).acc_split(1);
        v.cur_miss_acc.nxt     := '0';
        v.cur_miss_acc.bypass  := pipe_acc_bypass;
        v.cur_miss_acc.accid   := r.acc(conv_integer(r.acc_index)).accid;
        v.cur_miss_acc.hmaster := r.acc(conv_integer(r.acc_index)).hmaster;
        v.cur_miss_acc.haddr   := r.acc(conv_integer(r.acc_index)).haddr; 
        v.cur_miss_acc.data_src    := pipe_acc_hit;
        v.cur_miss_acc.data_done   := '0';
        v.cur_miss_acc.data.valid  := (others => '0');
        v.cur_miss_acc.data.offset := v.cur_miss_acc.haddr(CLOFFSETRANGE);
        v.cur_miss_acc.data.err    := (others => '0');
      end if;
    elsif miss_done = '1' and (r.cur_miss_acc.nxt = '1' or r.cur_miss_acc.valid = '0') then
      v.cur_miss_acc.valid    := '0';
      v.cur_miss_acc.active  := '0';
      v.cur_miss_acc.nxt     := '0';
    end if;

    -- cache hit access buffer
    if store_hit_acc = '1' then
      v.hit_acc(0).valid        := '1';
      v.hit_acc(0).accid        := r.acc(conv_integer(r.acc_index)).accid;
      v.hit_acc(0).acc_split    := r.acc(conv_integer(r.acc_index)).acc_split(0);
      v.hit_acc(0).hmaster      := r.acc(conv_integer(r.acc_index)).hmaster;
      v.hit_acc(0).haddr        := r.acc(conv_integer(r.acc_index)).haddr;
      v.hit_acc(0).data_index   := r.hit_acc_nxt_index;
      v.hit_acc(0).data_slice   := pipe_acc_slice;

      v.hit_dbuf(conv_integer(r.hit_acc_nxt_index)).offset := v.hit_acc(0).haddr(CLOFFSETRANGE);
      v.hit_dbuf(conv_integer(r.hit_acc_nxt_index)).valid  := (others => '0');

      for i in 1 to HIT_ACC_FIFO_DEPTH-1 loop
        v.hit_acc(i) := r.hit_acc(i-1);  
      end loop;
      if r.cur_acc.nxt = '0' and (r.hit_acc(conv_integer(r.hit_acc_index)).valid and not r.cur_acc.valid) = '0' and r.hit_acc(0).valid = '1' then
        v.hit_acc_index := r.hit_acc_index + 1;
      else
        v.hit_acc(conv_integer(r.hit_acc_index + 1)).valid := '0';
      end if;
    elsif r.cur_acc.nxt = '1' or (r.hit_acc(conv_integer(r.hit_acc_index)).valid and not r.cur_acc.valid) = '1' then
      if r.hit_acc_index /= "0000" then
        v.hit_acc_index := r.hit_acc_index - 1;
      end if;
      v.hit_acc(conv_integer(r.hit_acc_index)).valid := '0';
    end if;
    
    -- current access
    if (r.cur_acc.valid = '0' or r.cur_acc.nxt = '1') and r.hit_acc(conv_integer(r.hit_acc_index)).valid = '1' then
      v.cur_acc.valid   := r.hit_acc(conv_integer(r.hit_acc_index)).valid;
      v.cur_acc.haddr   := r.hit_acc(conv_integer(r.hit_acc_index)).haddr;
      v.cur_acc.accid   := r.hit_acc(conv_integer(r.hit_acc_index)).accid;
      v.cur_acc.hmaster := r.hit_acc(conv_integer(r.hit_acc_index)).hmaster;
      
      v.cur_acc.data.data   := r.hit_dbuf(conv_integer(r.hit_acc(conv_integer(r.hit_acc_index)).data_index)).data;
      v.cur_acc.data.valid  := r.hit_dbuf(conv_integer(r.hit_acc(conv_integer(r.hit_acc_index)).data_index)).valid;
      v.cur_acc.data.offset := r.hit_dbuf(conv_integer(r.hit_acc(conv_integer(r.hit_acc_index)).data_index)).offset;
      v.cur_acc.data.err    := r.hit_dbuf(conv_integer(r.hit_acc(conv_integer(r.hit_acc_index)).data_index)).err;

      v.cur_acc.split   := '1';

      v.cur_acc.data_index := r.hit_acc(conv_integer(r.hit_acc_index)).data_index;
      v.cur_acc.data_slice := r.hit_acc(conv_integer(r.hit_acc_index)).data_slice;
      
      if r.hit_acc(conv_integer(r.hit_acc_index)).acc_split = '1' then
        v.hsplit(conv_integer(v.cur_acc.hmaster)) := '1';
      else
        v.cur_acc.active := '1';
      end if;
    end if;

    -- Hit access data
    hit_acc_rbuf_index := r.hit_acc_rbuf_index;
    if pipe_data_valid = '1' and pipe_data_reissued = '0' then
      v.hit_dbuf(conv_integer(hit_acc_rbuf_index)).data(  conv_integer(pipe_data_offset)) := pipe_data;
      v.hit_dbuf(conv_integer(hit_acc_rbuf_index)).valid( conv_integer(pipe_data_offset)) := '1';
      v.hit_dbuf(conv_integer(hit_acc_rbuf_index)).err(   conv_integer(pipe_data_offset)) := pipe_data_err;
      
      if pipe_data_done = '1' then
      end if;
      
      if v.cur_acc.data_index = r.hit_acc_rbuf_index then
        v.cur_acc.data.data ( conv_integer(pipe_data_offset)) := pipe_data;
        v.cur_acc.data.valid( conv_integer(pipe_data_offset)) := '1';
        v.cur_acc.data.err(   conv_integer(pipe_data_offset)) := pipe_data_err;
      end if;
    end if;
  
    -- Missed access data
    if r.cur_miss_acc.valid = '1' and r.cur_miss_acc.data_done = '0' and (be_data_valid or (pipe_data_valid and pipe_data_reissued)) = '1' then
      if be_data_valid = '1' and r.cur_miss_acc.data_src = '0' then
        v.cur_miss_acc.data.data( conv_integer(be_data_offset)) := be_data;
        v.cur_miss_acc.data.valid(conv_integer(be_data_offset)) := '1';
        v.cur_miss_acc.data.err(  conv_integer(be_data_offset)) := be_data_err;
      elsif pipe_data_valid = '1' and r.cur_miss_acc.data_src = '1' then
        v.cur_miss_acc.data.data( conv_integer(pipe_data_offset)) := pipe_data;
        v.cur_miss_acc.data.valid(conv_integer(pipe_data_offset)) := '1';
        v.cur_miss_acc.data.err(  conv_integer(pipe_data_offset)) := pipe_data_err;
      end if;

      --if r.cur_miss_acc.split = '1' then -- Un-SPLIT master
      if ((be_data_done and not r.cur_miss_acc.data_src) or (pipe_data_done and r.cur_miss_acc.data_src)) = '1' then -- Un-SPLIT master 
        if r.cur_miss_acc.split = '1' then 
          v.hsplit(conv_integer(v.cur_miss_acc.hmaster)) := '1';
          v.cur_miss_acc.split := '0';
        end if;
        v.cur_miss_acc.data_done := '1';
      end if;
    end if;

    if r.lock_pen = '1' and r.cur_miss_acc.valid = '1' and r.cur_miss_acc.data_done = '1' then
      v.cur_miss_acc.valid := '0';
      v.cur_miss_acc.data_done := '0';
    end if;

  -- PIPE access mux ----------------------------------------------------------------
    if (r.acc(0).valid = '1' and r.acc(0).acc_hold = '1' and r.acc_0_int.valid = '0') or 
       r.del_acc(conv_integer(r.del_acc_index)).valid = '1' or
       (issue_miss_acc = '1')
    then
      if issue_miss_acc = '1' and (r.lock_pen = '0' or (r.miss_acc(0).valid and (r.miss_acc(0).hmastlock or r.miss_acc(0).hwrite)) = '1') then -- do not issue miss read when loced acc pending
        pipe_acc.valid  := r.miss_acc(0).valid;
        pipe_acc.accid  := r.miss_acc(0).accid;
        pipe_acc.addr   := r.miss_acc(0).haddr;
        pipe_acc.size   := r.miss_acc(0).hsize;
        pipe_acc.write  := r.miss_acc(0).hwrite;
        pipe_acc.master := r.miss_acc(0).hmaster;
        --pipe_acc.hprot  := r.miss_acc(0).hprot;              
        pipe_acc.mbsel  := r.miss_acc(0).hmbsel(0 to 2);
        pipe_acc.reissued := '1';
        pipe_acc.reissue  := '0';
        pipe_acc.locked   := '0';
      elsif issue_del_acc = '1' then -- locked accesses is issued immediately 
        pipe_acc.valid  := r.del_acc(conv_integer(r.del_acc_index)).valid;
        pipe_acc.accid  := r.del_acc(conv_integer(r.del_acc_index)).accid;
        pipe_acc.addr   := r.del_acc(conv_integer(r.del_acc_index)).haddr;
        pipe_acc.size   := r.del_acc(conv_integer(r.del_acc_index)).hsize;
        pipe_acc.write  := r.del_acc(conv_integer(r.del_acc_index)).hwrite;
        pipe_acc.master := r.del_acc(conv_integer(r.del_acc_index)).hmaster;
        --pipe_acc.hprot  := r.del_acc(conv_integer(r.del_acc_index)).hprot;  
        pipe_acc.mbsel  := r.del_acc(conv_integer(r.del_acc_index)).hmbsel(0 to 2);
        pipe_acc.reissued := '0';
        pipe_acc.reissue  := reissue_miss;
        pipe_acc.locked   := '0';
      else
        pipe_acc.valid  := r.acc(0).valid and not issue_hold and not conv_std_logic(r.acc(0).acc_split = "11"); 
        pipe_acc.accid  := r.acc(0).accid;                                                                      
        pipe_acc.addr   := r.acc(0).haddr;
        pipe_acc.size   := r.acc(0).hsize;
        pipe_acc.write  := r.acc(0).hwrite;
        pipe_acc.master := r.acc(0).hmaster;
        --pipe_acc.hprot  := r.acc(0).hprot;           
        pipe_acc.mbsel  := r.acc(0).hmbsel(0 to 2);
        pipe_acc.reissued := '0';
        pipe_acc.reissue  := reissue_miss;
        pipe_acc.locked   := r.acc(0).hmastlock;
      end if;
    else
      pipe_acc.valid  := v.acc(0).valid and not issue_hold and not conv_std_logic(v.acc(0).acc_split = "11"); 
      pipe_acc.accid  := v.acc(0).accid;                                                                      
      pipe_acc.addr   := v.acc(0).haddr;
      pipe_acc.size   := v.acc(0).hsize;
      pipe_acc.write  := v.acc(0).hwrite;
      pipe_acc.master := v.acc(0).hmaster;
      --pipe_acc.hprot  := v.acc(0).hprot;        
      pipe_acc.mbsel  := v.acc(0).hmbsel(0 to 2);
      pipe_acc.reissued := '0';
      pipe_acc.reissue  := reissue_miss;
      pipe_acc.locked   := v.acc(0).hmastlock;
    end if;

    if r.acc_0_int.valid = '1' then
      pipe_wdata := r.acc_0_int.hwdata;
    elsif r.acc_hold = '1' then
      pipe_wdata := r.acc(1).hwdata;
    else
      pipe_wdata := hwdata128;
    end if;

    -- Read data mux
    if r.cur_miss_acc.active = '1' then
      hrdata := r.cur_miss_acc.data.data(conv_integer(r.acc(0).haddr(CLOFFSETRANGE))); 
    else
      hrdata := r.cur_acc.data.data(conv_integer(r.acc(0).haddr(CLOFFSETRANGE))); 
    end if;
  
    -- Drive HRDATA
    if r.acc(0).hsize = "100" then                              -- 128-bit 
      ahbso.hrdata  <= ahbdrivedata(hrdata(127 downto 0));
    elsif r.acc(0).hsize = "011" then                           -- 64-bit 
      if r.acc(0).haddr(3) = '0' then ahbso.hrdata  <= ahbdrivedata(hrdata(127 downto 64));
      else ahbso.hrdata  <= ahbdrivedata(hrdata(63 downto 0)); end if;
    else                                                        -- 32/16/8-bit
      case r.acc(0).haddr(3 downto 2) is
      when "00" => hrdata(31 downto 0) := hrdata(127 downto 96);
      when "01" => hrdata(31 downto 0) := hrdata(95 downto 64);
      when "10" => hrdata(31 downto 0) := hrdata(63 downto 32);
      when others => hrdata(31 downto 0) := hrdata(31 downto 0);
      end case;
      ahbso.hrdata <= ahbdrivedata(hrdata(31 downto 0));
    end if;

  -- Reset --------------------------------------------------------------------------
    if rst = '0' then
      v.hready := '1';
      v.hresp  := "00";
      
      v.valid := '0';
      v.accid := (others => '0');
      v.lock_pen  := '0';
      
      v.acc_active    := '0';
      v.cur_acc.valid := '0';
      v.cur_acc.active:= '0';
      v.cur_acc.nxt   := '0';
      v.acc_index    := (others => '0');
      v.acc(0).valid := '0';
      v.acc(1).valid := '0';
      v.acc(2).valid := '0';
      v.acc(3).valid := '0';
      v.acc_0_int.valid := '0';
      v.del_acc_index    := (others => '0');
      v.del_acc(0).valid := '0';
      v.del_acc(1).valid := '0';
      v.del_acc(2).valid := '0';
      v.del_acc(3).valid := '0';
      v.miss_acc_index    := (others => '0');
      v.miss_acc(0).valid := '0';
      v.miss_acc(1).valid := '0';
      v.miss_acc(2).valid := '0';
      v.miss_acc(3).valid := '0';
      v.miss_acc_done     := '0';
      v.miss_acc_wait     := '0';
      v.cur_miss_acc.valid  := '0';
      v.cur_miss_acc.active := '0';
      v.cur_miss_acc.nxt    := '0';
      v.hit_acc_index    := (others => '0');
      v.hit_acc_nxt_index    := (others => '0');
      v.hit_acc_rbuf_index    := (others => '0');
      v.hit_acc(0).valid := '0';
      v.hit_acc(1).valid := '0';
      v.hit_acc(2).valid := '0';
      v.hit_acc(3).valid := '0';

      v.usplit_valid := '0';
      for i in 0 to MAX_AHBMST-1 loop
        v.split_buf(i).valid := '0';
      end loop;
      
    end if;
 
    rin <= v;

    fetop.acc     <= pipe_acc;
    fetop.wdata   <= pipe_wdata;
    fetop.facc_clr<= not (r.miss_acc(0).valid or r.cur_miss_acc.valid); 
    
    fetop.ahbsov  <= ahbsov;
    
    fetop.flush   <= '0';
    fetop.scrub   <= '0';
    fetop.inject  <= '0';

    -- AHB slave output
    ahbso.hready <= r.hready;
    ahbso.hresp  <= r.hresp;
    ahbso.hsplit <= r.hsplit;
    ahbso.hirq   <= r.hirq;

    -- Scan test support
    --fetop.testen <= ahbsi.testen;
    --fetop.scanen <= ahbsi.scanen;
    --fetop.testin <= ahbsi.testin;

    -- AMBA config
    ahbso.hconfig  <= hconfig;
    ahbso.hindex   <= hslvidx;

  end process comb;

  regs : process(clk)
  begin
    if rising_edge(clk) then r <= rin; end if;
  end process regs;
end;

