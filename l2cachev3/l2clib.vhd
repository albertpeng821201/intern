------------------------------------------------------------------------------
--  This file is a part of the GRLIB VHDL IP LIBRARY
--  Copyright (C) 2012, Aeroflex Gaisler AB - all rights reserved.
--
-- ANY USE OR REDISTRIBUTION IN PART OR IN WHOLE MUST BE HANDLED IN 
-- ACCORDANCE WITH THE GAISLER LICENSE AGREEMENT AND MUST BE APPROVED 
-- IN ADVANCE IN WRITING. 
-----------------------------------------------------------------------------
-- Package:     l2cache
-- File:        libcache.vhd
-- Author:      Nils-Johan Wessman - Gaisler Research
-- Description:	Internal package for the L2-Cache core types and functions
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library grlib;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
library techmap;
use techmap.gencomp.all;
library gaisler;

package l2clib is
constant MAX_ADDR_BITS  : integer := 32;
constant MAX_WAYS       : integer := 4;
constant TAG_HIGH       : integer := 31;
constant MAX_TAG_CB_BITS: integer := 7;
constant MAX_VALID_BITS : integer := 4;
constant MAX_DIRTY_BITS : integer := 4;
constant MAX_DATA_BITS  : integer := 128;
constant MAX_DATA_CB_BITS : integer := (MAX_DATA_BITS/32)*7;
constant MAX_LRU_BITS     : integer := 5;
constant MAX_OFFSET_BITS  : integer := 6;

type l2c_lru_bits_type is array(1 to 4) of integer;
constant l2c_lru_table  : l2c_lru_bits_type := (1,1,3,5);

type l2ctag_type is record
  tag   : std_logic_vector(TAG_HIGH downto 0);
  tcb   : std_logic_vector(MAX_TAG_CB_BITS-1 downto 0);
  valid : std_logic_vector(MAX_VALID_BITS-1 downto 0);
  dirty : std_logic_vector(MAX_DIRTY_BITS-1 downto 0);
end record;
type l2ctag_vector_type is array (0 to MAX_WAYS-1) of l2ctag_type;
subtype l2cdata_type is std_logic_vector(MAX_DATA_BITS-1 downto 0);
type l2cdata_vector_type is array (0 to MAX_WAYS-1) of l2cdata_type;
subtype l2ccb_type is std_logic_vector((MAX_DATA_BITS/32)*7-1 downto 0);
type l2ccb_vector_type is array (0 to MAX_WAYS-1) of l2ccb_type;
subtype l2cdctrl_vector is std_logic_vector(15 downto 0);
type l2cdctrl_type is array (0 to MAX_WAYS-1) of l2cdctrl_vector;

type l2cram_in_type is record
  way     : l2ctag_vector_type;
  taddr   : std_logic_vector(MAX_ADDR_BITS-1 downto 0);
  twaddr  : std_logic_vector(MAX_ADDR_BITS-1 downto 0);
  twrite  : std_logic_vector(MAX_WAYS-1 downto 0);
  tenable : std_logic_vector(MAX_WAYS-1 downto 0);
  tdwrite : std_logic;
  data    : std_logic_vector(MAX_DATA_BITS-1 downto 0); 
  cb      : std_logic_vector((MAX_DATA_BITS/32)*7-1 downto 0); 
  daddr   : std_logic_vector(MAX_ADDR_BITS-1 downto 0);
  dindex  : std_logic_vector(MAX_ADDR_BITS-1 downto 0);
  doffset : std_logic_vector(MAX_ADDR_BITS-1 downto 0);
  dway    : integer range 0 to MAX_WAYS-1;
  dwrite  : l2cdctrl_type;
  denable : l2cdctrl_type;
  lru     : std_logic_vector(MAX_LRU_BITS-1 downto 0);
  testen  : std_logic;
  scanen  : std_logic;
  testin  : std_logic_vector(3 downto 0);
end record;

type l2cram_out_type is record
  way   : l2ctag_vector_type;
  data  : l2cdata_vector_type; 
  cb    : l2ccb_vector_type; 
  lru   : std_logic_vector(MAX_LRU_BITS-1 downto 0);
end record;

type l2c_dma_in_type is record
  address         : std_logic_vector(31 downto 0);
  wdata           : std_logic_vector(31 downto 0);
  wdata128        : std_logic_vector(127 downto 0);
  start           : std_ulogic;
  burst           : std_ulogic;
  write           : std_ulogic;
  busy            : std_ulogic;
  irq             : std_ulogic;
  size            : std_logic_vector(2 downto 0);
  hburst          : std_logic_vector(2 downto 0);
end record;

type l2c_dma_out_type is record
  start           : std_ulogic;
  active          : std_ulogic;
  ready           : std_ulogic;
  retry           : std_ulogic;
  mexc            : std_ulogic;
  haddr           : std_logic_vector(9 downto 0);
  rdata           : std_logic_vector(31 downto 0);
  rdata128        : std_logic_vector(127 downto 0);
  hirq            : std_logic_vector(NAHBIRQ-1 downto 0);
end record;

type l2c_dma_ahb_in_type is record
  req     : std_ulogic;
  write   : std_ulogic; 
  addr    : std_logic_vector(31 downto 0);
  wdata128: std_logic_vector(127 downto 0);
  size    : std_logic_vector(2 downto 0);
  noreq   : std_logic;
  noseq   : std_logic;
  hburst  : std_logic_vector(2 downto 0);
end record;
constant l2c_dma_ahb_in_none : l2c_dma_ahb_in_type := ('0', '0', (others => '0'),
  (others => '0'), (others => '0'), '0', '0', (others => '0'));

type l2c_dma_ahb_out_type is record
  grant   : std_ulogic;
  ready   : std_ulogic;
  error   : std_ulogic;
  retry   : std_ulogic;
  rdata128: std_logic_vector(127 downto 0);
  hirq    : std_logic_vector(NAHBIRQ-1 downto 0);
end record;

--component l2cctl-v1 is
--  generic (
--    hslvidx  : integer := 0;
--    haddr    : integer := 16#F00#;
--    hmask    : integer := 16#F00#;
--    ioaddr   : integer := 16#000#;
--    cached   : integer := 16#0000#;
--    cen      : integer range 0 to 1  := 0;
--    hproten  : integer range 0 to 1  := 1;
--    wp       : integer range 0 to 1  := 0;
--    repl     : integer range 0 to 1  := 0;
--    ways     : integer range 1 to 4  := 1;
--    linesize : integer range 16 to 32  := 32;
--    waysize  : integer range 1 to 512 := 1;
--    tech     : integer range 0 to NTECH := 0;
--    bbuswidth: integer := 128;
--    bioaddr  : integer := 16#000#;
--    biomask  : integer := 16#000#;
--    sbus     : integer := 0;
--    mbus     : integer := 1;
--    stat     : integer range 0 to 2 := 0;
--    mtrr     : integer range 0 to 32 := 0);
--  port (
--    rst : in  std_ulogic;
--    clk : in  std_ulogic;
--    cramo : in  l2cram_out_type;
--    crami : out l2cram_in_type;
--    ahbsi : in  ahb_slv_in_type;
--    ahbso : out ahb_slv_out_type;
--    ahbsov: in  ahb_slv_out_vector;
--    dmai  : out l2c_dma_in_type;
--    dmao  : in  l2c_dma_out_type
--);
--end component;

component l2cctl is
  generic (
    hslvidx  : integer := 0;
    haddr    : integer := 16#F00#;
    hmask    : integer := 16#F00#;
    ioaddr   : integer := 16#000#;
    cached   : integer := 16#0000#;
    hirq     : integer := 0;
    cen      : integer range 0 to 1  := 0;
    hproten  : integer range 0 to 1  := 1;
    wp       : integer range 0 to 1  := 0;
    repl     : integer range 0 to 1  := 0;
    ways     : integer range 1 to 4  := 1;
    linesize : integer range 16 to 64  := 32;
    waysize  : integer range 1 to 512 := 1;
    tech     : integer range 0 to NTECH := 0;
    bbuswidth: integer := 128;
    bioaddr  : integer := 16#000#;
    biomask  : integer := 16#000#;
    sbus     : integer := 0;
    mbus     : integer := 1;
    stat     : integer range 0 to 2 := 0;
    mtrr     : integer range 0 to 32 := 0;
    edacen   : integer range 0 to 1 := 0;
    rmw      : integer range 0 to 1 := 0;
    ft       : integer range 0 to 1 := 0;
    fttiming : integer range 0 to 1 := 0;
    scantest : integer range 0 to 1 := 0;
    wbmask   : integer range 0 to 16#FFFF# := 16#FFFF#;
    debug    : integer range 0 to 1 := 0);
  port (
    rst : in  std_ulogic;
    clk : in  std_ulogic;
    cramo : in  l2cram_out_type;
    crami : out l2cram_in_type;
    ahbsi : in  ahb_slv_in_type;
    ahbso : out ahb_slv_out_type;
    ahbsov: in  ahb_slv_out_vector;
    dmai  : out l2c_dma_ahb_in_type;
    dmao  : in  l2c_dma_ahb_out_type;
    sto   : out std_logic_vector(2 downto 0);
    debugo: out std_logic_vector(255*debug downto 0)
);
end component;

component l2cmem is
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
end component;

component l2cahb is
  generic (
    hindex    : integer range 0 to NAHBMST-1  := 0;
    venid     : integer := VENDOR_GAISLER;
    devid     : integer := 0;
    version   : integer := 0;
    scantest  : integer := 0);
  port (
    rst   : in  std_ulogic;
    clk   : in  std_ulogic;
    ahbmi  : in  ahb_mst_in_type;
    ahbmo  : out ahb_mst_out_type;
    dmai0  : in  l2c_dma_ahb_in_type;
    dmao0  : out l2c_dma_ahb_out_type;
    dmai1  : in  l2c_dma_ahb_in_type;
    dmao1  : out l2c_dma_ahb_out_type
  );
end component;


-- 3-way set permutations
-- s012 => way 0 - least recently used
--         way 2 - most recently used
constant s012 : std_logic_vector(2 downto 0) := "000";
constant s021 : std_logic_vector(2 downto 0) := "001";
constant s102 : std_logic_vector(2 downto 0) := "010";
constant s120 : std_logic_vector(2 downto 0) := "011";
constant s201 : std_logic_vector(2 downto 0) := "100";
constant s210 : std_logic_vector(2 downto 0) := "101";


-- 4-way set permutations
-- s0123 => way 0 - least recently used
--          way 3 - most recently used
constant s0123 : std_logic_vector(4 downto 0) := "00000";
constant s0132 : std_logic_vector(4 downto 0) := "00001";
constant s0213 : std_logic_vector(4 downto 0) := "00010";
constant s0231 : std_logic_vector(4 downto 0) := "00011";
constant s0312 : std_logic_vector(4 downto 0) := "00100";
constant s0321 : std_logic_vector(4 downto 0) := "00101";
constant s1023 : std_logic_vector(4 downto 0) := "00110";
constant s1032 : std_logic_vector(4 downto 0) := "00111";
constant s1203 : std_logic_vector(4 downto 0) := "01000";
constant s1230 : std_logic_vector(4 downto 0) := "01001";
constant s1302 : std_logic_vector(4 downto 0) := "01010";
constant s1320 : std_logic_vector(4 downto 0) := "01011";
constant s2013 : std_logic_vector(4 downto 0) := "01100";
constant s2031 : std_logic_vector(4 downto 0) := "01101";
constant s2103 : std_logic_vector(4 downto 0) := "01110";
constant s2130 : std_logic_vector(4 downto 0) := "01111";
constant s2301 : std_logic_vector(4 downto 0) := "10000";
constant s2310 : std_logic_vector(4 downto 0) := "10001";
constant s3012 : std_logic_vector(4 downto 0) := "10010";
constant s3021 : std_logic_vector(4 downto 0) := "10011";
constant s3102 : std_logic_vector(4 downto 0) := "10100";
constant s3120 : std_logic_vector(4 downto 0) := "10101";
constant s3201 : std_logic_vector(4 downto 0) := "10110";
constant s3210 : std_logic_vector(4 downto 0) := "10111";

type lru_3way_table_vector_type is array(0 to 2) of std_logic_vector(2 downto 0);
type lru_3way_table_type is array (0 to 7) of lru_3way_table_vector_type;

constant lru_3way_table : lru_3way_table_type :=
  ( (s120, s021, s012),                   -- s012
    (s210, s021, s012),                   -- s021
    (s120, s021, s102),                   -- s102
    (s120, s201, s102),                   -- s120
    (s210, s201, s012),                   -- s201
    (s210, s201, s102),                   -- s210
    (s210, s201, s102),                   -- dummy
    (s210, s201, s102)                    -- dummy
  );
  
type lru_4way_table_vector_type is array(0 to 3) of std_logic_vector(4 downto 0);
type lru_4way_table_type is array(0 to 31) of lru_4way_table_vector_type;

constant lru_4way_table : lru_4way_table_type :=
  ( (s1230, s0231, s0132, s0123),       -- s0123
    (s1320, s0321, s0132, s0123),       -- s0132
    (s2130, s0231, s0132, s0213),       -- s0213
    (s2310, s0231, s0312, s0213),       -- s0231
    (s3120, s0321, s0312, s0123),       -- s0312    
    (s3210, s0321, s0312, s0213),       -- s0321
    (s1230, s0231, s1032, s1023),       -- s1023
    (s1320, s0321, s1032, s1023),       -- s1032
    (s1230, s2031, s1032, s1203),       -- s1203
    (s1230, s2301, s1302, s1203),       -- s1230
    (s1320, s3021, s1302, s1023),       -- s1302
    (s1320, s3201, s1302, s1203),       -- s1320
    (s2130, s2031, s0132, s2013),       -- s2013
    (s2310, s2031, s0312, s2013),       -- s2031
    (s2130, s2031, s1032, s2103),       -- s2103
    (s2130, s2301, s1302, s2103),       -- s2130      
    (s2310, s2301, s3012, s2013),       -- s2301
    (s2310, s2301, s3102, s2103),       -- s2310
    (s3120, s3021, s3012, s0123),       -- s3012
    (s3210, s3021, s3012, s0213),       -- s3021
    (s3120, s3021, s3102, s1023),       -- s3102
    (s3120, s3201, s3102, s1203),       -- s3120
    (s3210, s3201, s3012, s2013),       -- s3201
    (s3210, s3201, s3102, s2103),       -- s3210
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103),        -- dummy
    (s3210, s3201, s3102, s2103)         -- dummy
  );

type lru3_repl_table_single_type is array(0 to 2) of integer range 0 to 2;
type lru3_repl_table_type is array(0 to 7) of lru3_repl_table_single_type;

constant lru3_repl_table : lru3_repl_table_type :=
  ( (0, 1, 2),      -- s012
    (0, 2, 2),      -- s021
    (1, 1, 2),      -- s102
    (1, 1, 2),      -- s120
    (2, 2, 2),      -- s201
    (2, 2, 2),      -- s210
    (2, 2, 2),      -- dummy
    (2, 2, 2)       -- dummy
  );

type lru4_repl_table_single_type is array(0 to 3) of integer range 0 to 3;
type lru4_repl_table_type is array(0 to 31) of lru4_repl_table_single_type;

constant lru4_repl_table : lru4_repl_table_type :=
  ( (0, 1, 2, 3), -- s0123
    (0, 1, 3, 3), -- s0132
    (0, 2, 2, 3), -- s0213
    (0, 2, 2, 3), -- s0231
    (0, 3, 3, 3), -- s0312
    (0, 3, 3, 3), -- s0321
    (1, 1, 2, 3), -- s1023
    (1, 1, 3, 3), -- s1032
    (1, 1, 2, 3), -- s1203
    (1, 1, 2, 3), -- s1230
    (1, 1, 3, 3), -- s1302
    (1, 1, 3, 3), -- s1320
    (2, 2, 2, 3), -- s2013
    (2, 2, 2, 3), -- s2031
    (2, 2, 2, 3), -- s2103
    (2, 2, 2, 3), -- s2130
    (2, 2, 2, 3), -- s2301
    (2, 2, 2, 3), -- s2310
    (3, 3, 3, 3), -- s3012
    (3, 3, 3, 3), -- s3021
    (3, 3, 3, 3), -- s3102
    (3, 3, 3, 3), -- s3120
    (3, 3, 3, 3), -- s3201
    (3, 3, 3, 3), -- s3210
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0), -- dummy
    (0, 0, 0, 0)  -- dummy
  );
  
type l2c_edacdectype is record
  data : std_logic_vector(31 downto 0);
  err  : std_ulogic;
  merr : std_ulogic;
end record;

type lru_bits_type is array(1 to 4) of integer;
constant lru_table  : lru_bits_type := (1,1,3,5);

function ben128(size   : in std_logic_vector(2 downto 0); 
                offset : in std_logic_vector(3 downto 0);
                inv    : in boolean) return std_logic_vector;

function lru_way (lru : in std_logic_vector; WAYS : in integer) return std_logic_vector;
function lru_calc (lru : in std_logic_vector; way : in integer; WAYS : in integer) return std_logic_vector;

function get_valid(wayv : in std_logic_vector; cramo_way : in l2ctag_vector_type) return std_logic_vector;
function get_dirty(wayv : in std_logic_vector; cramo_way : in l2ctag_vector_type) return std_logic_vector;
function get_lock_way(addr : in std_logic_vector; lock : in std_logic_vector(MAX_WAYS-1 downto 0); constant WAYS : in integer) return integer;
  
--calculates 7 BCH(32,7) checkbits from the 32-bit dataword d
function l2c_edacencode( d  : in std_logic_vector(31 downto 0) )
          return std_logic_vector;

--version of edacdecode used in sdram controller. The difference is
--how err is set and that syn is generated internally. 
function l2c_edacdecode2( d    : in std_logic_vector(31 downto 0);
          cbin : in std_logic_vector(6 downto 0) )
          return l2c_edacdectype;
  

-- NEW =>

  subtype  MAX_ADDRRANGE is natural range 31 downto 0;
  subtype  MAX_WAYRANGE is natural range 3 downto 0;
  subtype  MAX_TAGRANGE is natural range 31 downto 10;
  subtype  MAX_TAGCBRANGE is natural range 6 downto 0;
  subtype  MAX_VALIDRANGE is natural range 3 downto 0;
  subtype  MAX_DIRTYRANGE is natural range 3 downto 0;
  subtype  MAX_LRURANGE is natural range 4 downto 0;
  subtype  MAX_CBRANGE is natural range 6 downto 0;
  constant CRAMDW : integer := 128;
  subtype  MAX_DATARANGE is natural range CRAMDW-1 downto 0;
  subtype  MAX_DATACBRANGE is natural range (CRAMDW/(8*4)*7)-1 downto 0;
  subtype  MAX_OFFSETRANGE is natural range 6 downto 0;
  subtype  MAX_INDEXRANGE is natural range 31 downto 0;

  type dram_out_type is record
    data : std_logic_vector(MAX_DATARANGE);
    cb   : std_logic_vector(MAX_DATACBRANGE);
  end record;
  type dram_out_vector_type is array (0 to 3) of dram_out_type;
  
  type l2c_access_type is record
    valid   : std_logic;
    accid   : std_logic_vector(3 downto 0);
    reissued: std_logic;
    reissue : std_logic;
    locked  : std_logic;

    addr    : std_logic_vector(31 downto 0);
    size    : std_logic_vector(2 downto 0);
    write   : std_logic;
    master  : std_logic_vector(3 downto 0);
    mbsel   : std_logic_vector(0 to 2);
  end record;
  
  -- Frontend <=> PIPE
  type l2c_fe_to_pipe_type is record
    acc     : l2c_access_type;
    wdata   : std_logic_vector(127 downto 0);
    --cfg     : l2c_config_type;
    ahbsov  : ahb_slv_out_vector;

    -- type
    flush   : std_logic;
    scrub   : std_logic;
    inject  : std_logic;

    --
    facc_clr: std_logic;

    testen  : std_logic;
    scanen  : std_logic;
    testin  : std_logic_vector(3 downto 0);
  end record;
  
  type l2c_pipe_to_fe_type is record
    hold    : std_logic;

    -- Access hit/miss status
    p2_valid  : std_logic;                    
    p2_accid  : std_logic_vector(3 downto 0); 
    p2_slice  : std_logic_vector(3 downto 0); 
    p2_hit    : std_logic;
    p2_reissue: std_logic;
    p2_fdone  : std_logic;
    p2_fbusy  : std_logic;
    p2_locked : std_logic;
    p2_bypass : std_logic;
    -- Access read data
    accid         : std_logic_vector(3 downto 0);
    data_valid    : std_logic;
    data_done     : std_logic;
    data          : std_logic_vector(127 downto 0);
    data_offset   : std_logic_vector(MAX_OFFSETRANGE);
    data_err      : std_logic;
    data_reissued : std_logic;
    data_locked   : std_logic;
    -- Config
    cfg_cen       : std_logic;
    cfg_split     : std_logic_vector(1 downto 0);
  end record;
 
  -- PIPE <=> Backend
  type l2c_pipe_to_be_type is record
    accid       : std_logic_vector(3 downto 0);
    master      : std_logic_vector(3 downto 0);

    new_addr    : std_logic_vector(31 downto 0);  -- Fetch/bypass(bp)
    old_addr    : std_logic_vector(31 downto 0);  -- Store
    size        : std_logic_vector(2 downto 0);
    data_valid  : std_logic;
    data_done   : std_logic;
    data        : std_logic_vector(MAX_DATA_BITS-1 downto 0);
    data_offset : std_logic_vector(MAX_OFFSETRANGE);
    data_err    : std_logic;
    
    fetch_valid : std_logic_vector(1 downto 0);   -- Fetch cache line [ to FE | to cache ]
    fetch_slice : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    
    store_valid : std_logic;                      -- Store cache line
    store_slice : std_logic_vector(MAX_VALID_BITS-1 downto 0);
    
    bp_valid    : std_logic;                      -- Write-through/read bypass
    bp_write    : std_logic;
    bp_data     : std_logic_vector(MAX_DATA_BITS-1 downto 0);

    cfg_besize  : std_logic_vector(2 downto 0);

  end record;
  
  type l2c_be_to_pipe_type is record
    hold        : std_logic;
    valid       : std_logic;
    accid       : std_logic_vector(3 downto 0);
    data        : std_logic_vector(127 downto 0);
    data_valid  : std_logic;
    data_offset : std_logic_vector(MAX_OFFSETRANGE);
    data_err    : std_logic;
    done        : std_logic;
  end record;
  
  -- Frontend <=> Backend
  type l2c_fe_to_be_type is record
    valid       : std_logic;
    accid       : std_logic_vector(3 downto 0);
    
    addr        : std_logic_vector(31 downto 0);
    size        : std_logic_vector(2 downto 0);
    write       : std_logic;
    data        : std_logic_vector(127 downto 0);
    data_valid  : std_logic;
    data_offset : std_logic_vector(MAX_OFFSETRANGE);
  end record;
  
  type l2c_be_to_fe_type is record
    accid       : std_logic_vector(3 downto 0);
    data_valid  : std_logic;
    data_done   : std_logic;
    data        : std_logic_vector(127 downto 0);
    data_offset : std_logic_vector(MAX_OFFSETRANGE);
    data_err    : std_logic;
    data_locked : std_logic;
  end record;
  

  component l2cfe is
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
      debugo: out std_logic_vector(255*debug downto 0)
      );
  end component;

  component l2cpipe is
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
      crami   : out l2cram_in_type);
  end component;
  
  component l2cbe is
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
      dmao    : in  l2c_dma_ahb_out_type);
  end component;

-- NEW <=
end;

package body l2clib is
function get_valid(wayv : in std_logic_vector; cramo_way : in l2ctag_vector_type) return std_logic_vector is
  variable xwayv : std_logic_vector(wayv'length-1 downto 0);
  variable valid : std_logic_vector(cramo_way(0).valid'range); -- should use VALID_BITS
begin
  xwayv := wayv;
  valid := (others => '0');
  for i in 0 to wayv'length-1 loop
    if xwayv(i) = '1' then
       valid := cramo_way(i).valid;
    end if;
  end loop;
  return valid;
end function;

function get_dirty(wayv : in std_logic_vector; cramo_way : in l2ctag_vector_type) return std_logic_vector is
  variable xwayv : std_logic_vector(wayv'length-1 downto 0);
  variable dirty : std_logic_vector(cramo_way(0).valid'range); -- should use VALID_BITS
begin
  xwayv := wayv;
  dirty := (others => '0');
  for i in 0 to wayv'length-1 loop
    if xwayv(i) = '1' then
       dirty := cramo_way(i).dirty and cramo_way(i).valid;
    end if;
  end loop;
  return dirty;
end function;

-- 128-bit byte enable
function ben128(size   : in std_logic_vector(2 downto 0); 
             offset : in std_logic_vector(3 downto 0);
             inv    : in boolean) 
             return std_logic_vector is
  variable tmp : std_logic_vector(15 downto 0);
begin
  case size is
  when "100" => -- 256-bit
    tmp := (others => '1');
  when "011" => -- 64-bit
    case offset is
    when "0000" => tmp := "1111111100000000";
    when "1000" => tmp := "0000000011111111";
    when others  => tmp := (others => '0');
    end case;
  when "010" => --  32-bit
    case offset is
    when "0000" => tmp := "1111000000000000";
    when "0100" => tmp := "0000111100000000";
    when "1000" => tmp := "0000000011110000";
    when "1100" => tmp := "0000000000001111";
    when others  => tmp := (others => '0');
    end case;
  when "001" => --  16-bit
    case offset is
    when "0000" => tmp := "1100000000000000";
    when "0010" => tmp := "0011000000000000";
    when "0100" => tmp := "0000110000000000";
    when "0110" => tmp := "0000001100000000";
    when "1000" => tmp := "0000000011000000";
    when "1010" => tmp := "0000000000110000";
    when "1100" => tmp := "0000000000001100";
    when "1110" => tmp := "0000000000000011";
    when others  => tmp := (others => '0');
    end case;
  when "000" => --   8-bit
    case offset is
    when "0000" => tmp := "1000000000000000";
    when "0001" => tmp := "0100000000000000";
    when "0010" => tmp := "0010000000000000";
    when "0011" => tmp := "0001000000000000";
    when "0100" => tmp := "0000100000000000";
    when "0101" => tmp := "0000010000000000";
    when "0110" => tmp := "0000001000000000";
    when "0111" => tmp := "0000000100000000";
    when "1000" => tmp := "0000000010000000";
    when "1001" => tmp := "0000000001000000";
    when "1010" => tmp := "0000000000100000";
    when "1011" => tmp := "0000000000010000";
    when "1100" => tmp := "0000000000001000";
    when "1101" => tmp := "0000000000000100";
    when "1110" => tmp := "0000000000000010";
    when "1111" => tmp := "0000000000000001";
    when others  => tmp := (others => '0');
    end case;
  when others =>
    tmp := (others => '0');
  end case;
  if inv = true then tmp := not tmp; end if;
  return tmp;
end function;

function lru_way (lru : in std_logic_vector; WAYS : in integer) return std_logic_vector is
--variable xlru : std_logic_vector(lru'length-1 downto 0);
variable xlru : std_logic_vector(4 downto 0);
variable way  : std_logic_vector(1 downto 0);
begin
  xlru := (others => '0');
  xlru(lru'length-1 downto 0) := lru;
  way := (others => '0');

  case WAYS is
  when 2 =>
    way(0) := xlru(0); 
  when 3 => 
    way := conv_std_logic_vector(lru3_repl_table(conv_integer(xlru(2 downto 0))) (0), 2);
  when 4 =>
    way := conv_std_logic_vector(lru4_repl_table(conv_integer(xlru(4 downto 0))) (0), 2);
  when others => 
  end case;

  return(way);
end;

function lru_calc (lru : in std_logic_vector; way : in integer; WAYS : in integer) return std_logic_vector is
--variable xlru : std_logic_vector(lru'length-1 downto 0);
variable xlru : std_logic_vector(4 downto 0);
--variable new_lru : std_logic_vector(lru'length-1 downto 0);
variable new_lru : std_logic_vector(4 downto 0);
begin
  xlru := (others => '0');
  xlru(lru'length-1 downto 0) := lru;
  new_lru := (others => '0');

  case WAYS is
  when 2 => 
    if way = 0 then new_lru(0) := '1'; else new_lru(0) := '0'; end if;
  when 3 =>
    new_lru(2 downto 0) := lru_3way_table(conv_integer(xlru(2 downto 0)))(way); 
  when 4 => 
    new_lru(4 downto 0) := lru_4way_table(conv_integer(xlru(4 downto 0)))(way);
  when others => 
  end case;
  return(new_lru(lru'length-1 downto 0));
end;
 
function get_lock_way(addr : in std_logic_vector; lock : in std_logic_vector(MAX_WAYS-1 downto 0); constant WAYS : in integer) return integer is
  variable way : integer range 0 to WAYS+3;
  variable vaddr : std_logic_vector(1 downto 0);
begin
  vaddr := addr;
  if lock /= zero32(WAYS-1 downto 0) then
    if WAYS < 3 then way := WAYS - conv_integer(lock) + conv_integer(vaddr(0));
    else way := WAYS - conv_integer(lock) + conv_integer(vaddr(1 downto 0)); end if;
  else
    way := WAYS;
  end if;
  if way > WAYS then way := WAYS; end if;
  return way;
end function;

function l2c_edacencode( d : in std_logic_vector(31 downto 0) )
                         return std_logic_vector is
  variable cb : std_logic_vector(6 downto 0); 
begin
  cb(0) := d(0) xor d(4) xor d(6) xor d(7) xor d(8) xor d(9) xor
     d(11) xor d(14) xor d(17) xor d(18) xor d(19) xor d(21)
     xor d(26) xor d(28) xor d(29) xor d(31);
  cb(1) := d(0) xor d(1) xor d(2) xor d(4) xor d(6) xor d(8) xor
     d(10) xor d(12) xor d(16) xor d(17) xor d(18) xor d(20)
     xor d(22) xor d(24) xor d(26) xor d(28);
  cb(2) := not ( d(0) xor d(3) xor d(4) xor d(7) xor d(9) xor d(10) xor
     d(13) xor d(15) xor d(16) xor d(19) xor d(20) xor d(23)
     xor d(25) xor d(26) xor d(29) xor d(31) );
  cb(3) := not ( d(0) xor d(1) xor d(5) xor d(6) xor d(7) xor d(11) xor
     d(12) xor d(13) xor d(16) xor d(17) xor d(21) xor d(22)
     xor d(23) xor d(27) xor d(28) xor d(29) );
  cb(4) := d(2) xor d(3) xor d(4) xor d(5) xor d(6) xor d(7) xor
     d(14) xor d(15) xor d(18) xor d(19) xor d(20) xor d(21)
     xor d(22) xor d(23) xor d(30) xor d(31);
  cb(5) := d(8) xor d(9) xor d(10) xor d(11) xor d(12) xor d(13) xor
     d(14) xor d(15) xor d(24) xor d(25) xor d(26) xor d(27)
     xor d(28) xor d(29) xor d(30) xor d(31);
  cb(6) := d(0) xor d(1) xor d(2) xor d(3) xor d(4) xor d(5) xor
     d(6) xor d(7) xor d(24) xor d(25) xor d(26) xor d(27)
           xor d(28) xor d(29) xor d(30) xor d(31);
  return cb;
end function;

function l2c_edacdecode2( d    : in std_logic_vector(31 downto 0);
          cbin : in std_logic_vector(6 downto 0) )
          return l2c_edacdectype is
  variable co : l2c_edacdectype;
  variable syn : std_logic_vector(6 downto 0);
begin
  co.data := d; co.merr := '0';
  syn := cbin xor l2c_edacencode(d);
  if (syn = "0000000") then co.err := '0'; else co.err := '1'; end if;
  case syn is
  when "0000000" => null; 
  when "1001111" => co.data(0) := not co.data(0);  
  when "1001010" => co.data(1) := not co.data(1);
  when "1010010" => co.data(2) := not co.data(2);
  when "1010100" => co.data(3) := not co.data(3);
  when "1010111" => co.data(4) := not co.data(4);
  when "1011000" => co.data(5) := not co.data(5);
  when "1011011" => co.data(6) := not co.data(6);
  when "1011101" => co.data(7) := not co.data(7);
  when "0100011" => co.data(8) := not co.data(8);
  when "0100101" => co.data(9) := not co.data(9);
  when "0100110" => co.data(10) := not co.data(10);   
  when "0101001" => co.data(11) := not co.data(11);
  when "0101010" => co.data(12) := not co.data(12);
  when "0101100" => co.data(13) := not co.data(13);
  when "0110001" => co.data(14) := not co.data(14);
  when "0110100" => co.data(15) := not co.data(15);
  when "0001110" => co.data(16) := not co.data(16);
  when "0001011" => co.data(17) := not co.data(17);
  when "0010011" => co.data(18) := not co.data(18);
  when "0010101" => co.data(19) := not co.data(19);  
  when "0010110" => co.data(20) := not co.data(20);   
  when "0011001" => co.data(21) := not co.data(21);
  when "0011010" => co.data(22) := not co.data(22);
  when "0011100" => co.data(23) := not co.data(23);
  when "1100010" => co.data(24) := not co.data(24);
  when "1100100" => co.data(25) := not co.data(25);
  when "1100111" => co.data(26) := not co.data(26);
  when "1101000" => co.data(27) := not co.data(27);
  when "1101011" => co.data(28) := not co.data(28);
  when "1101101" => co.data(29) := not co.data(29);
  when "1110000" => co.data(30) := not co.data(30);   
  when "1110101" => co.data(31) := not co.data(31);
  when "0000001" => null; 
  when "0000010" => null;
  when "0000100" => null;
  when "0001000" => null; 
  when "0010000" => null;
  when "0100000" => null; 
  when "1000000" => null; 
  when others => co.merr := '1';
  end case;
  return co;
end function;

end;
