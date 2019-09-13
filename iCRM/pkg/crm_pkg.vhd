use STD.textio.all;
library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;
use ieee.std_logic_textio.all;
library grlib;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
use grlib.dma2ahb_package.all;
library techmap;
use techmap.gencomp.all;
library gaisler;
library top;
use top.config.all;
library ina;
library ina_pkg;
use ina_pkg.ina_config.all;
use ina_pkg.bus_package.all;

package crm_pkg is

-------------------- Local TLM Sharing Record ---------------------------
type coherency_block is record
        id : std_logic_vector(31 downto 0);
        tiles : std_logic_vector((dim_x*dim_y)-1 downto 0);
        start_addr:  std_logic_vector(31 downto 0);
        end_addr: std_logic_vector(31 downto 0);
end record;

-------------------- Remote TLM Sharing Record ---------------------------
type shared_block is record
        id : std_logic_vector(31 downto 0);
        start_addr:  std_logic_vector(31 downto 0);
        end_addr: std_logic_vector(31 downto 0);
end record;

constant zeros_32 : std_logic_vector(31 downto 0) := (others => '0');

constant COHERENCY_BLOCK_ARRAY_SIZE : integer := 8;
constant SHARED_BLOCK_ARRAY_SIZE : integer := 8;

type coherency_block_array is array (0 to (COHERENCY_BLOCK_ARRAY_SIZE - 1)) of coherency_block;
type shared_block_array is array (0 to (SHARED_BLOCK_ARRAY_SIZE - 1)) of shared_block;

constant share_addr_start	: std_logic_vector(31 downto 0) := x"80000000"; -- start of shareable address of TLM
constant share_addr_end		: std_logic_vector(31 downto 0) := x"807fffff"; -- end of shareable address of TLM
--constant share_addr_end		: std_logic_vector(31 downto 0) := x"801fffff"; -- end of shareable address of TLM (ila)

constant region_size : integer := 8;
constant dbit_status_bit : integer := 2;
constant dbit : integer := dbit_status_bit + (region_size-1);
constant abit : integer := clog2(to_integer(unsigned(share_addr_end(23 downto 5))));

type crmLUT_type is array (0 to (region_size-2)) of integer;

type CStoIM_type is record
    tile_src    : std_logic_vector(7 downto 0);  -- if NA then originating tileID, else myTileID
    data        : std_logic_vector((dim_x*dim_y)-1 downto 0); -- sharer tiles, one bit per tileID (tiles from coherency block)
    address     : std_logic_vector(31 downto 0); -- address of CL + MSG
end record;

type tracebuf_in_type is record
  data			: std_logic_vector(127 downto 0);
  enable		: std_logic;
  write			: std_logic;--std_logic_vector(3 downto 0);
  tile_src		: std_logic_vector(log2x(dim_x*dim_y)-1 downto 0);
end record;

type regtype is record
  thaddr        : std_logic_vector(31 downto 0);
  thwrite       : std_logic;
  thtrans		: std_logic_vector(1 downto 0);
  thsize        : std_logic_vector(2 downto 0);
  thburst       : std_logic_vector(2 downto 0);
  thmaster      : std_logic_vector(3 downto 0);
  thmastlock    : std_logic;
  ahbactive     : std_logic;

  hready        : std_logic; --
  hready2       : std_logic;
  hready3       : std_logic;
  hsel          : std_logic; --
  hwrite        : std_logic; --
  haddr         : std_logic_vector(31 downto 2);
  hrdata        : std_logic_vector(31 downto 0);
  regacc        : std_logic_vector(3 downto 0);

  enable        : std_logic;	-- trace enable
  bahb          : std_logic;	-- break on AHB watchpoint hit

  ctr_word		: std_logic_vector(31 downto 0); --
  trans_code	: std_logic_vector(3 downto 0); --
  trigger		: std_ulogic; --
  cb_write		: std_ulogic;
  cb_addr		: std_logic_vector(9 downto 0);
  cb_data		: std_logic_vector(31 downto 0);

  sb_write		: std_ulogic;
  sb_addr		: std_logic_vector(9 downto 0);
  sb_data		: std_logic_vector(31 downto 0);

  fifoDepth         : std_logic_vector(31 downto 0);
  fifoCriticalLevel : std_logic_vector(15 downto 0);
  fifoSafeLevel     : std_logic_vector(15 downto 0);

  dir_addr			: std_logic_vector((abit-1) downto 0);
  dir_rd			: std_logic;
  sharing_vector	: std_logic_vector((dbit-1) downto 0);
  evictionPolicy	: std_logic_vector(31 downto 0);
end record;

type counter_reg is record
-- counter

  master_counter : std_logic_vector(31 downto 0);
  update_counter : std_logic_vector(31 downto 0);
  writeback_counter : std_logic_vector(31 downto 0);
  invalidation_gen_counter : std_logic_vector(31 downto 0);
  invalidation_exe_counter : std_logic_vector(31 downto 0);
  full_counterIG : std_logic_vector(31 downto 0);
  full_counterIE : std_logic_vector(31 downto 0);
 -- cb  		: coherency_block_array;
end record;

type fregtype is record
  shsel					: std_logic_vector(0 to NAHBSLV-1);
  f_write				: std_ulogic;         -- Filter reads
  f_read				: std_ulogic;         -- Filter writes
  smask					: std_logic_vector(31 downto 0);
  mmask					: std_logic_vector(31 downto 0);
  f_retry				: std_ulogic;
  f_update				: std_ulogic;
  f_write_back			: std_ulogic;
  f_invalidation_exe	: std_ulogic;
  f_invalidation_gen	: std_ulogic;
  f_performance_counter : std_ulogic;
end record;

type if_debug_type is record
    d_full : std_ulogic;
    d_enable : std_ulogic;
    d_cr_filt_hit : boolean;
    d_sb_filt_hit: boolean;
    d_cr_filt_tile : std_logic;
    d_tbi_data_rd : std_logic;
    d_tbi_data_nonseq : std_logic_vector(1 downto 0);
    d_tbi_data_namst : std_logic_vector(3 downto 0);
    d_tbi_data_addr : std_logic_vector(31 downto 0);
    d_crm_state : integer;
    d_counter : integer;
    d_partial_expression : boolean;
    d_complete_expression : boolean;
end record;

type crm_cs_chipscope_type is record
    cs_full : std_ulogic;
    cs_enable : std_ulogic;
    cs_cr_filt_hit : std_ulogic;
    cs_sb_filt_hit: std_ulogic;
    cs_cr_filt_tile : std_logic;
    cs_tbi_data_rdwr : std_logic;
    cs_tbi_data_trans : std_logic_vector(1 downto 0);
    cs_tbi_data_namst : std_logic_vector(3 downto 0);
    cs_tbi_data_split : std_logic_vector(1 downto 0);
    cs_tbi_data_addr : std_logic_vector(31 downto 0);
	cs_fifoIF_stage : std_logic_vector(3 downto 0);
	cs_config_state : std_logic_vector(3 downto 0);
    cs_ahb_filt_hit : std_ulogic;
end record;

type crm_im_chipscope_type is record
	im_current_s		: std_logic_vector(3 downto 0);
	im_idleTrans		: std_logic;
	im_dmai				: DMA_In_Type;
	im_dmao				: DMA_OUt_Type;
	im_ram_addr			: std_logic_vector((abit - 1) downto 0);
	im_ram_data_rd		: std_logic_vector((dbit - 1) downto 0);
	im_ram_data_wr		: std_logic_vector((dbit - 1) downto 0);
	im_ram_enable		: std_logic;
	im_ram_write		: std_logic;
	im_busAccess_done	: std_logic;
	im_sharingVector	: std_logic_vector((region_size-2) downto 0);
end record;

type crm_top_chipscope_type is record
    top_wr_en : std_ulogic;
end record;

type ret_type is record
    tile_id : integer;
    new_inv_tiles : std_logic_vector(crmLUT_type'high downto 0);
end record;

type invMan_type is record
	dmai			: DMA_In_Type;
	busAccess		: bus_access_main_type;
	crmLUT			: crmLUT_type;
	sharingVector	: std_logic_vector((region_size-2) downto 0);
	evict_flag		: std_logic;
	evict_data		: std_logic_vector((dbit-1) downto 0);
	evict_address	: std_logic_vector((abit-1) downto 0);
	rd_data_change	: std_logic;
end record;
constant invMan_type_default : invMan_type := (DMA_In_Type_none, bus_access_main_type_none, (others => 0), (others => '0'), '0', (others => '0'), (others => '0'),'0');

type invManCnt_type is record
	invGen_cnt		: std_logic_vector(31 downto 0);
	invExe_cnt		: std_logic_vector(31 downto 0);
	WB_cnt			: std_logic_vector(31 downto 0);
	dirUpdate_cnt	: std_logic_vector(31 downto 0);
	eviction_cnt	: std_logic_vector(31 downto 0);
end record;
constant invManCnt_type_default : invManCnt_type := ((others => '0'), (others => '0'), (others => '0'), (others => '0'), (others => '0'));

-------------------------Functions-----------------------------
function ahb_filt_hit (  r  : regtype;  rf :  fregtype) return boolean;
function cr_filt_hit (  tbi : tracebuf_in_type;  cr :  coherency_block_array) return boolean;
function sb_filt_hit (  tbi : tracebuf_in_type;  sr :  shared_block_array) return boolean;
function cr_filt_tile (  tbi  : tracebuf_in_type;  cr :  coherency_block_array) return std_logic_vector;
function cr_mod(cr :  coherency_block_array) return boolean;
function cr_mod_id (cr :  coherency_block_array) return integer;
function rpt_test(vabufi : tracebuf_in_type; vabufi_hist : tracebuf_in_type) return boolean;

end;

package body crm_pkg is

function ahb_filt_hit (
  r  : regtype;
  rf :  fregtype) return boolean is
  variable hit : boolean;
begin
  -- filter hit -> inhibit
  hit := false;
  -- Filter on read/write
  if ((rf.f_write and r.thwrite) or (rf.f_read and not r.thwrite)) = '1'  then
    hit := true;
  end if;
  -- Filter on address range

 for i in rf.mmask'range loop
    if i = conv_integer(r.thmaster) and rf.mmask(i) = '1' then
      hit := true;
    end if;
  end loop;
  -- Filter on slave mask
  for i in rf.shsel'range loop
    if (rf.shsel(i) and rf.smask(i)) /= '0' then
      hit := true;
    end if;
  end loop;

  return hit;

end function ahb_filt_hit;

function cr_filt_hit (
  tbi : tracebuf_in_type;
  cr :  coherency_block_array) return boolean is
  variable hit : boolean;
begin
hit := FALSE;
 for i in 0 to (COHERENCY_BLOCK_ARRAY_SIZE - 1) loop
        if(cr(i).id(31) = '1' and cr(i).id(30) = '0') then
                if(tbi.data(31 downto 0) >= cr(i).start_addr and tbi.data(31 downto 0)  <= cr(i).end_addr ) then
                hit := true; --ret.tiles_inv := cr(i).tiles;
                return hit;
                end if;
        end if;
  end loop;
return hit;
end function cr_filt_hit;

function sb_filt_hit (
  tbi : tracebuf_in_type;
  sr :  shared_block_array) return boolean is
  variable hit : boolean;
begin
hit := FALSE;
 for i in 0 to (SHARED_BLOCK_ARRAY_SIZE - 1) loop
        if(sr(i).id(31) = '1') then
                if(tbi.data(31 downto 0) >= sr(i).start_addr and tbi.data(31 downto 0)  <= sr(i).end_addr ) then
                hit := true; --ret.tiles_inv := cr(i).tiles;
                return hit;
                end if;
        end if;
  end loop;
return hit;
end function sb_filt_hit;

function cr_filt_tile (
  tbi  : tracebuf_in_type;
  cr :  coherency_block_array) return std_logic_vector is
  variable tile : std_logic_vector((dim_x*dim_y)-1 downto 0);
begin
tile := (others => '0');
 for i in 0 to (COHERENCY_BLOCK_ARRAY_SIZE - 1) loop
        if(cr(i).id(31) = '1'and cr(i).id(30) = '0') then
                if((tbi.data(31 downto 0) >= cr(i).start_addr) and (tbi.data(31 downto 0)  <= cr(i).end_addr) ) then
                        tile := cr(i).tiles;
                        return tile;
                end if;
        end if;
  end loop;
return tile;
end function cr_filt_tile;

function cr_mod(cr :  coherency_block_array) return boolean is
variable hit : boolean;
begin
hit := FALSE;
 for i in 0 to (COHERENCY_BLOCK_ARRAY_SIZE - 1) loop
        if(cr(i).id(30) = '1') then
                hit := true; --ret.tiles_inv := cr(i).tiles;
                return hit;
        end if;
  end loop;
return hit;
end function cr_mod;

function cr_mod_id (cr :  coherency_block_array) return integer is
variable val : integer;
begin
val := 0;
 for i in 0 to (COHERENCY_BLOCK_ARRAY_SIZE - 1) loop
        if(cr(i).id(30) = '1') then
                val := i; --ret.tiles_inv := cr(i).tiles;
                return val;
        end if;
  end loop;
return val;
end function cr_mod_id;

function rpt_test(vabufi : tracebuf_in_type; vabufi_hist : tracebuf_in_type) return boolean is
variable hit : boolean;
begin
hit := TRUE;

        if(((vabufi.tile_src = vabufi_hist.tile_src) and (vabufi.data(31 downto 5) = vabufi_hist.data(31 downto 5)) and (vabufi.data(79) = vabufi_hist.data(79)) and (vabufi.data(70 downto 67) = vabufi_hist.data(70 downto 67))) or (vabufi.data(78 downto 77) = HTRANS_NONSEQ) )then
        hit := FALSE;
        end if;

return hit;
end function rpt_test;


end package body;
