library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library techmap;
use techmap.gencomp.all;

library top;
use top.config_types.all;

library tcpa_lib;
use tcpa_lib.AG_BUFFER_type_lib.all;

library gaisler;
use gaisler.misc.all;

-- Technology and synthesis options
package config is

  constant HW_REVISION        : integer := 31;

  constant CFG_HW_SYSTEM      : integer := 2; -- 1: chipit, 2: proFPGA

constant CFG_DEBUG : debug_type := (
	ina	=> (
			backtrace	=> 0,
			debug_regs	=> 1,
			measurement	=> 0
			),
	noc => (
			router_status => 0
	)
);

constant NMA_CFG : nma_config_type := (
	sharq => (
			en			=> '1',
			dummy		=> '1'
			),
	enemy => (
			cache		=> '0',
			meta_cache	=> '0',
			tlm_en		=> '0',
			ddr_en		=> '1',
			debug		=> '1',
			perf		=> '0',
			hmap_en		=> '0'
	)
);

--TODO:
--
-- calc dst_tile out of new RMAP

	constant MemMAP : t_memory_map_parameters := (
		SHM_start		=> x"00000000",
		SHM_TWIN_start	=> x"40000000",
		SHM_end			=> x"80000000",
		SHM_haddr		=> 16#000#,
		SHM_hmask		=> 16#800#,
		SHM_TWIN_haddr	=> 16#400#,
		SHM_TWIN_hmask	=> 16#C00#,
		SHM_IO			=> 16#F98#,
		TLM_start		=> x"80000000",
		TLM_end			=> x"80800000",
		TLM_haddr		=> 16#800#,
		TLM_hmask		=> 16#FF8#,
		RMAP_start		=> x"B0000000",
		RMAP_offset		=> x"01000000", -- 16MB
		RMAP_haddr		=> 16#B00#,
		RMAP_hmask		=> 16#F00#, --16*16MB=256MB

		LOCAL_start		=> x"80000000",
		LOCAL_end		=> x"90000000",

		INA_start		=> x"80E00000",
		INA_haddr		=> 16#80E#,
		INA_hmask		=> 16#FFE#, -- 2MB

		ENEMY_TLM_start	=> x"FFE00000",
		ENEMY_DDR_start	=> x"FFF00000",
		ENEMY_TLM_haddr	=> 16#FFE#,
		ENEMY_DDR_haddr	=> 16#FFF#,
		ENEMY_hmask		=> 16#FFF#,

		APB_HADDR		=> 16#80C#,
		CRM_HADDR		=> 16#80D#,

		DSU_HADDR		=> 16#F00#,
		DSU_HMASK		=> 16#F80#,

		HELPER_TLM_start	=> x"E0000000",
		HELPER_TLM_HADDR	=> 16#E00#,
		HELPER_TLM_HMASK	=> 16#FF8#,
		HELPER_TLM_ROMADDR	=> 16#E10#,
		HELPER_TLM_ROMMASK	=> 16#FFF#,
		HELPER_TLM_IOADDR	=> 16#E20#,
		HELPER_TLM_IOMASK	=> 16#FF1#,

		AHB_1_IO		=> 16#F80#,

		L2C_CTRL_0_HADDR	=> 16#810#,
		L2C_CTRL_0_HMASK	=> 16#FFC#,
		L2C_CTRL_1_HADDR	=> 16#814#,
		L2C_CTRL_1_HMASK	=> 16#FFE#,
		L2C_CTRL_2_HADDR	=> 16#F90#,
		L2C_CTRL_2_HMASK	=> 16#FFF#,
		L2C_RANGE_HADDR		=> 16#815#,
		L2C_RANGE_HMASK		=> 16#FFF#,

		L2C_CTRL_0_start	=> x"81000000",
		L2C_RANGE_start		=> x"81500000",

		ICORE_MMIO_HADDR		=> 16#D00#,
		ICORE_VLCCWSTO_HADDR	=> 16#D01#,
		ICORE_BIT_HADDR			=> 16#D02#,
		ICORE_B1DMA_HADDR		=> 16#D03#,
		ICORE_TLM_HADDR			=> 16#808#
	);

  -- needed by other config files:
  constant dw                 : integer := 32;
  constant TILE_ID_WIDTH      : integer := 8;  -- 8 bits to represent tile id
  constant burst_size         : integer := 8;
  constant CFG_DDR_ADDR       : integer := MemMAP.SHM_haddr;
  constant TILE_ID_SUBTRAHEND : std_logic_vector(TILE_ID_WIDTH-1 downto 0) := MemMAP.RMAP_start(31 downto 24);  --Subtrahend used to derive tile id from the received AHB address
  constant CFG_RSTADDR        : integer := to_integer(unsigned(MemMAP.TLM_start(31 downto 12))); --16#80000#;
  constant CFG_TLM_END_ADDR   : integer := to_integer(unsigned(MemMAP.TLM_end(31 downto 12))); --16#80800#;
  constant CFG_GLOBAL_BASE    : integer := to_integer(unsigned(MemMAP.RMAP_start(31 downto 20))); --16#400#;
  constant CFG_GLOBAL_OFFSET  : integer := to_integer(unsigned(MemMAP.RMAP_offset(31 downto 20))); --16#010#;
  constant CFG_DDR_END_ADDR   : integer := to_integer(unsigned(MemMAP.SHM_end(31 downto 20))); --16#400#; -- 256 MB => MASK = 16#F00#
  constant CFG_DDR_MASK		  : integer := MemMAP.SHM_hmask; -- = 16#1000# - 16#100#
  constant CFG_AHBRSZ         : integer := 512;
  constant CFG_AHBRADDR       : integer := to_integer(unsigned(MemMAP.SHM_start(31 downto 20))); --16#000#;
  constant MON_GLOBAL_ADDR    : std_logic_vector(TILE_ID_WIDTH-1 downto 0) := x"70";
  constant NA_SPLIT_ENABLE    : integer := 1;
  constant CFG_NCPU           : integer := 5; -- max num of cpu for cic
  constant CFG_CPU_FREQ       : integer := 50000; -- cpu freq for cic mon in kHz
  constant CFG_DDR_BRIDGE_FREQ: integer := 100000;

  constant CFG_VC_CNT             : integer := 4;

  constant B1DMA_on       : integer := 0;
  constant amba_monitor_on : integer := 0;

  --  DEFINITIONS
  constant WIDTH  : integer := 2;
  constant HEIGHT : integer := 2;

	constant TWIN_DDR			: integer := 1;
	constant DDR_x_loc			: integer := 1;
	constant DDR_y_loc			: integer := 1;
	constant TWIN_DDR_x_loc		: integer := 3;
	constant TWIN_DDR_y_loc		: integer := 3;
	constant CFG_DIM_X			: integer := 2;
	constant CFG_DIM_Y			: integer := 2;

  constant iCore_tile_cnt       : integer := 0;
  constant iCore_tile_ID_0      : integer := 2;
  constant iCore_CPU_ID_0       : integer := 0;
  constant iCore_tile_ID_1      : integer := 0;
  constant iCore_CPU_ID_1       : integer := 0;
  constant iCore_tile_ID_2      : integer := 0;
  constant iCore_CPU_ID_2       : integer := 0;
  constant iCore_tile_x_loc_0   : integer := 0;
  constant iCore_tile_y_loc_0   : integer := 1;

  constant TCPA_tile_cnt        : integer := 0;
  constant TCPA_tile_ID_0       : integer := 2;
  constant TCPA_pe_cnt_0        : integer := 16;
  constant TCPA_iCTRL_cnt_0     : integer := 1;
  constant TCPA_tile_x_loc_0    : integer := 0;
  constant TCPA_tile_y_loc_0    : integer := 1;
  constant TCPA_tile_ID_1       : integer := 0;
  constant TCPA_tile_ID_2       : integer := 0;

  constant CFG_SIM			: integer := 0;


	-- TODO: new slave map will be raised here soon
	constant CFG_NAADDR_MP	  : integer := MemMAP.INA_haddr;

	constant N_TILES : integer := CFG_DIM_X * CFG_DIM_Y;

--	type membar_array_type_1x1 is array(0 to 0, 0 to 7) of integer range 0 to 1073741823;
--	type membar_array_type_2x1 is array(0 to 1, 0 to 7) of integer range 0 to 1073741823;
	type membar_array_type_2x2 is array(0 to 3, 0 to 7) of integer range 0 to 1073741823;
	type membar_array_type_4x4 is array(0 to 15, 0 to 7) of integer range 0 to 1073741823;

	type membar_array_type is array(natural range <>, natural range <>) of integer range 0 to 1073741823;

--	constant membar_array_1x1 : membar_array_type_1x1 := (  -- Tile 0
--		(
--		  others => 0
--		)
--	);

--	constant membar_array_2x1 : membar_array_type_2x1 := (  -- Tile 0
--		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28) & x"10")), '1', '1', 16#FF0#),
--		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
--		  others => 0
--		),
--		-- Tile 1 with DDR
--		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28) & x"00")), '1', '1', 16#FF0#),
--		  others => 0
--		)
--	);

	constant membar_array_2x1 : membar_array_type(0 to 1, 0 to 7) := (  -- Tile 0
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"10"), '1', '1', 16#FF0#),
		  0,
		  0,
		  0,
		  0,
		  0,
		  0,
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask)
		),
		-- Tile 1
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		)
	);

	constant membar_array_2x2 : membar_array_type(0 to 3, 0 to 7) := (  -- Tile 0
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"10"), '1', '1', 16#FF0#), -- to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"10")
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"20"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask), --marker: single_tile_membar
		  0,
		  0,
		  0,
		  0,
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask)
		),
		-- Tile 1
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"20"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 2
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"30"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 3 with DDR
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"20"), '1', '1', 16#FF0#),
		  0,
		  0,
		  0,
		  0,
		  0,
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask)
		)
	);

	constant membar_array_4x4 : membar_array_type(0 to 15, 0 to 7) := (  -- Tile 0
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"10"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"20"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 1
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"20"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 2
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"30"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 3
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"20"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 4
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"50"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"60"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 5 with DDR
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"60"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_TWIN_haddr, '1', '1', MemMAP.SHM_TWIN_hmask), -- GByte 1..2
		  0,
		  0,
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_TWIN_hmask) -- GByte 0..1
		),
		-- Tile 6
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"70"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 7
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"40"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"60"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#F80#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 8
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"90"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"A0"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 9
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"A0"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 10
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"B0"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FC0#),
          ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
          others => 0
		),
		-- Tile 11
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"A0"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 12
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"D0"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"E0"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 13
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"E0"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 14
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"F0"), '1', '1', 16#FF0#),
		  ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_hmask),
		  others => 0
		),
		-- Tile 15 with TWIN_DDR
		( ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"00"), '1', '1', 16#F80#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"80"), '1', '1', 16#FC0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"C0"), '1', '1', 16#FE0#),
		  ahb2ahb_membar(to_integer(unsigned(MemMAP.RMAP_start(31 downto 28)) & x"E0"), '1', '1', 16#FF0#),
          ahb2ahb_membar(MemMAP.SHM_haddr, '1', '1', MemMAP.SHM_TWIN_hmask), -- GByte 0..1
		  0,
		  0,
		  ahb2ahb_membar(MemMAP.SHM_TWIN_haddr, '1', '1', MemMAP.SHM_TWIN_hmask) -- GByte 1..2
		)
	);

	constant membar_array : membar_array_type := membar_array_2x2;



  constant architecture_parameters_glbl : t_architecture_parameters := (
	maxahbm => 16,
	maxahbs => 16,
	maxapb  => 16,

	leon3               => 1,
	en_na               => 1,
	na_ls               => 1,
	na_mp               => 1,
	profpga_ssram       => 1,
	ahbramen            => 0,
	sim					=> CFG_SIM,
	mctrl_leon2         => 0,
	en_ddr              => 0,
	en_dvi2ahb          => 0,
	en_pendulum_control => 0,
	en_beeper           => 0,
	icore               => 0,
	b1dma_enable        => B1DMA_on,
	tcpa                => 0,
	ahb_uart            => 1,
	ahb_jtag            => 0,
	ahbstat             => 1,
	uart1_enable        => 1,
	irq3_enable         => 1,
	gpt_enable          => 1,
	i2c_enable          => 0,
	grgpio_enable       => 1,
	kbd_enable          => 0,
	vga_enable          => 0,
	svga_enable         => 0,
	grsysmon            => 0,
	gracectrl           => 0,
	pciexp              => 0,
	natlas              => 0,
	en_amba_monitor     => amba_monitor_on,
	en_eth              => 0,

	umr_data_bitwidth => 8,
	ahbuart_pindex    => 7,
	apbuart_pindex    => 1,
	irqmp_pindex      => 2,
	gptimer_pindex    => 3,
	ahbstat_pindex    => 8,
	mctrl_pindex      => 4,
	ddr_pindex        => -1,
	gc_pindex         => -1,
	cm_pindex         => -1,
	rr_pindex         => -1,
	fi_pindex         => -1,
	dvi_pindex        => -1,
	pwm_pindex        => -1,
	beeper_pindex     => -1,
	eth_pindex        => -1,
	unused_pindex     => 9,


	-- FSB AHBMO indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  => CFG_NCPU,
	na_mst_rx_idx         => CFG_NCPU,
	na_mst_tx_idx         => CFG_NCPU+1,
	l2c_reverse_mst_idx	  => CFG_NCPU+2,
	enemy_tlm_mst_idx     => CFG_NCPU+3,
	enemy_ddr_mst_idx	  => -1,
	mmi64_ahbm_idx        => CFG_NCPU+4,
	ahb_uart_m_idx        => CFG_NCPU+5,
	eth_mindex            => -1,
	loader_mst_idx		  => CFG_NCPU+6,
	crm_mst_idx           => -1,
	icore_bitload_hindexm => -1,
	b1dma_hindexm         => -1,
	unused_ahbmindex      => CFG_NCPU+7,


	-- FSB AHBSO indices
	apb_hindex               => 1,
	dsu_hindex               => 2,
	na_hindex                => 4,
	ssram_hindex             => 5,
	sim_hindex				 => 12,
	ddr_hindex               => 6,
	l2_hindex                => 7,
	amp_hindex               => 3,
	ram_hindex               => -1,
	cic_hindex               => 10,
	icore_tlm_hindexs        => 8,
	b1dma_hindexs            => 9,
	icore_bitload_hindexs    => -1,
	icore_mi_hindexs         => -1,
	icore_vlcwsto_hindexs    => -1,
	tcpa_buffer_north_hindex => -1,
	tcpa_buffer_west_hindex  => -1,
	tcpa_buffer_south_hindex => -1,
	tcpa_buffer_east_hindex  => -1,
	tcpa_rbuffer_hindex      => -1,
	dvi_hindex               => -1,
	crm_hindex               => 11,
	enemy_tlm_hindex		 => 13,
	unused_ahbsindex         => 14,

	-- na ahb
	l2c_hindexm => 0,

    fabtech   => virtex7,
	memtech   => virtex7,
	padtech   => virtex7,
	transtech => gtp0,
	noasync   => 0,
	scan      => 0
  );

  constant architecture_parameters_framebuffer_debug : t_architecture_parameters := (
	maxahbm     =>  8,
	maxahbs     =>  8,
	maxapb      =>  8,

	leon3               => 1,
	en_na               => 0,
	na_ls               => 0,
	na_mp               => 0,
	profpga_ssram       => 1,
	ahbramen            => 0,
	sim                 => CFG_SIM,
	mctrl_leon2         => 0,
	en_ddr              => 0,
	en_dvi2ahb          => 1,
	en_pendulum_control => 0,
	en_beeper           => 0,
	icore               => 0,
	b1dma_enable        => 1,
	tcpa                => 0,
	ahb_uart            => 0,
	ahb_jtag            => 0,
	ahbstat             => 1,
	uart1_enable        => 0,
	irq3_enable         => 1,
	gpt_enable          => 1,
	i2c_enable          => 0,
	grgpio_enable       => 0,
	kbd_enable          => 0,
	vga_enable          => 0,
	svga_enable         => 0,
	grsysmon            => 0,
	gracectrl           => 0,
	pciexp              => 0,
	natlas              => 0,
	en_amba_monitor     => amba_monitor_on,
	en_eth              => 0,

	umr_data_bitwidth =>  8,

	ahbuart_pindex    => -1,
	apbuart_pindex    => -1,
	irqmp_pindex      =>  1,
	gptimer_pindex    =>  2,
	ahbstat_pindex    =>  3,
	mctrl_pindex      =>  4,
	ddr_pindex        => -1,
	gc_pindex         => -1,
	cm_pindex         => -1,
	rr_pindex         => -1,
	fi_pindex         => -1,
	dvi_pindex        =>  5,
	pwm_pindex        => -1,
	beeper_pindex     => -1,
	eth_pindex        => -1,
	unused_pindex     =>  6,

	-- FSB AHBMO indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  =>  5,
	na_mst_rx_idx         => -1,
	na_mst_tx_idx         => -1,
	mmi64_ahbm_idx        =>  5,
	eth_mindex            => -1,
	ahb_uart_m_idx        => -1,
	icore_bitload_hindexm => -1,
	b1dma_hindexm         =>  6,
	crm_mst_idx           =>  7,
	loader_mst_idx		  => 8,
	enemy_tlm_mst_idx     => 9,
	enemy_ddr_mst_idx	  => -1,
	l2c_reverse_mst_idx	  => -1,
	unused_ahbmindex      => 10,

	-- FSB AHBSO indices
	apb_hindex               =>  0,
	dsu_hindex               =>  1,
	na_hindex                => -1,
	ssram_hindex             =>  2,
	sim_hindex				 => 12,
	ddr_hindex               => -1,
	l2_hindex                =>  3,
	amp_hindex               =>  4,
	ram_hindex               => -1,
	cic_hindex               =>  5,
	icore_tlm_hindexs        => -1,
	b1dma_hindexs            =>  6,
	icore_bitload_hindexs    => -1,
	icore_mi_hindexs         => -1,
	icore_vlcwsto_hindexs    => -1,
	tcpa_buffer_north_hindex => -1,
	tcpa_buffer_west_hindex  => -1,
	tcpa_buffer_south_hindex => -1,
	tcpa_buffer_east_hindex  => -1,
	tcpa_rbuffer_hindex      => -1,
	dvi_hindex               =>  7,
	crm_hindex               =>  8,
	enemy_tlm_hindex		 =>  13,
	unused_ahbsindex         =>  14,

	-- na ahb
	l2c_hindexm => 0,

    fabtech   => virtex7,
	memtech   => virtex7,
	padtech   => virtex7,
	transtech => gtp0,
	noasync   => 0,
	scan      => 0
  );

  constant architecture_parameters_ddr : t_architecture_parameters := (
	maxahbm     => 16,
	maxahbs     => 16,
	maxapb      => 16,

	leon3               => 1,
	en_na               => 1,
	na_ls               => 1,
	na_mp               => 1,
	profpga_ssram       => 1,
	ahbramen            => 0,
	sim                 => CFG_SIM,
	mctrl_leon2         => 0,
	en_ddr              => 1,
	en_dvi2ahb          => 0,
	en_pendulum_control => 0,
	en_beeper           => 0,
	icore               => 0,
	b1dma_enable        => B1DMA_on,
	tcpa                => 0,
	ahb_uart            => 1,
	ahb_jtag            => 0,
	ahbstat             => 1,
	uart1_enable        => 1,
	irq3_enable         => 1,
	gpt_enable          => 1,
	i2c_enable          => 0,
	grgpio_enable       => 1,
	kbd_enable          => 0,
	vga_enable          => 0,
	svga_enable         => 0,
	grsysmon            => 0,
	gracectrl           => 0,
	pciexp              => 0,
	natlas              => 0,
	en_amba_monitor     => amba_monitor_on,
	en_eth              => 0,

	umr_data_bitwidth   => 8,

	ahbuart_pindex    => 0,
	apbuart_pindex    => 1,
	irqmp_pindex      => 2,
	gptimer_pindex    => 3,
	ahbstat_pindex    => 8,
	mctrl_pindex      => 4,
	ddr_pindex        => 6,
	gc_pindex         => -1,
	cm_pindex         => -1,
	rr_pindex         => -1,
	fi_pindex         => -1,
	dvi_pindex        => -1,
	pwm_pindex        => -1,
	beeper_pindex     => -1,
	eth_pindex        => 7,
	unused_pindex     => 9,


	-- FSB AHBMO indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  => CFG_NCPU,
	na_mst_rx_idx         => CFG_NCPU,
	na_mst_tx_idx         => CFG_NCPU+1,
	l2c_reverse_mst_idx	  => CFG_NCPU+2,
	enemy_tlm_mst_idx     => CFG_NCPU+3,
	enemy_ddr_mst_idx	  => CFG_NCPU+4,
	mmi64_ahbm_idx        => CFG_NCPU+5,
	ahb_uart_m_idx        => CFG_NCPU+6,
	eth_mindex            => CFG_NCPU+7,
	loader_mst_idx		  => CFG_NCPU+7,
	crm_mst_idx           => -1,
	icore_bitload_hindexm => -1,
	b1dma_hindexm         => -1,
	unused_ahbmindex      => CFG_NCPU+9,

	-- FSB AHBSO indices
	apb_hindex               => 0,
	dsu_hindex               => 1,
	na_hindex                => 2,
	ssram_hindex             => 3,
	sim_hindex				 => 12,
	ddr_hindex               => 4,
	l2_hindex                => 5,
	amp_hindex               => -1,
	ram_hindex               => -1,
	cic_hindex               => 6,
	icore_tlm_hindexs        => -1,
	b1dma_hindexs            => 7,
	icore_bitload_hindexs    => -1,
	icore_mi_hindexs         => -1,
	icore_vlcwsto_hindexs    => -1,
	tcpa_buffer_north_hindex => -1,
	tcpa_buffer_west_hindex  => -1,
	tcpa_buffer_south_hindex => -1,
	tcpa_buffer_east_hindex  => -1,
	tcpa_rbuffer_hindex      => -1,
	dvi_hindex               => -1,
	crm_hindex               => 8,
	enemy_tlm_hindex		 => 13,
	unused_ahbsindex         => 14,

	-- na ahb
	l2c_hindexm     => 0,

    fabtech   => virtex7,
	memtech   => virtex7,
	padtech   => virtex7,
	transtech => gtp0,
	noasync   => 0,
	scan      => 0
  );


  constant architecture_parameters_pendulum : t_architecture_parameters := (
	maxahbm     => 16,
	maxahbs     => 16,
	maxapb      => 16,

	leon3               => 1,
	en_na               => 1,
	na_ls               => 1,
	na_mp               => 1,
	profpga_ssram       => 1,
	ahbramen            => 0,
	sim                 => CFG_SIM,
	mctrl_leon2         => 0,
	en_ddr              => 0,
	en_dvi2ahb          => 1,
	en_pendulum_control => 1,
	en_beeper           => 1,
	icore               => 0,
	b1dma_enable        => B1DMA_on,
	tcpa                => 0,
	ahb_uart            => 1,
	ahb_jtag            => 0,
	ahbstat             => 1,
	uart1_enable        => 1,
	irq3_enable         => 1,
	gpt_enable          => 1,
	i2c_enable          => 0,
	grgpio_enable       => 1,
	kbd_enable          => 0,
	vga_enable          => 0,
	svga_enable         => 0,
	grsysmon            => 0,
	gracectrl           => 0,
	pciexp              => 0,
	natlas              => 1,
	en_amba_monitor     => amba_monitor_on,
	en_eth              => 0,

	umr_data_bitwidth   => 8,

	ahbuart_pindex => 0,
	apbuart_pindex => 1,
	irqmp_pindex   => 2,
	gptimer_pindex => 3,
	ahbstat_pindex => 4,
	mctrl_pindex   => 5,
	ddr_pindex     => -1,
	gc_pindex      => -1,
	cm_pindex      => -1,
	rr_pindex      => -1,
	fi_pindex      => -1,
	dvi_pindex     => 6,
	pwm_pindex     => 7,
	beeper_pindex  => 8,
	eth_pindex     => -1,
	unused_pindex  => 9,


	-- FSB AHBMO indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  => 5,
	na_mst_rx_idx         => 5,
	na_mst_tx_idx         => 6,
	mmi64_ahbm_idx        => 7,
	eth_mindex            => -1,
	ahb_uart_m_idx        => 8,
	icore_bitload_hindexm => -1,
	b1dma_hindexm         => 9,
	crm_mst_idx           => 10,
	loader_mst_idx		  => 11,
	enemy_tlm_mst_idx     => 12,
	enemy_ddr_mst_idx	  => -1,
	l2c_reverse_mst_idx	  => -1,
	unused_ahbmindex      => 13,


	-- FSB AHBSO indices
	apb_hindex               => 0,
	dsu_hindex               => 1,
	na_hindex                => 2,
	ssram_hindex             => 3,
	sim_hindex				 => 12,
	ddr_hindex               => 4,
	l2_hindex                => 5,
	amp_hindex               => 6,
	ram_hindex               => -1,
	cic_hindex               => 7,
	icore_tlm_hindexs        => -1,
	b1dma_hindexs            => 8,
	icore_bitload_hindexs    => -1,
	icore_mi_hindexs         => -1,
	icore_vlcwsto_hindexs    => -1,
	tcpa_buffer_north_hindex => -1,
	tcpa_buffer_west_hindex  => -1,
	tcpa_buffer_south_hindex => -1,
	tcpa_buffer_east_hindex  => -1,
	tcpa_rbuffer_hindex      => -1,
	dvi_hindex               => 9,
	crm_hindex               => 10,
	enemy_tlm_hindex		 => 13,
	unused_ahbsindex         => 14,

	-- na ahb
	l2c_hindexm     => 0,

	fabtech     => virtex7,
	memtech     => virtex7,
	padtech     => virtex7,
	transtech     => gtp0,
	noasync     => 0,
	scan      => 0
  );

  constant architecture_parameters_icore : t_architecture_parameters := (
	maxahbm => 16,
	maxahbs => 16,
	maxapb  => 16,

	leon3               => 1,
	en_na               => 1,
	na_ls               => 1,
	na_mp               => 1,
	profpga_ssram       => 1,
	ahbramen            => 0,
	sim                 => CFG_SIM,
	mctrl_leon2         => 0,
	en_ddr              => 0,
	en_dvi2ahb          => 0,
	en_pendulum_control => 0,
	en_beeper           => 0,
	icore               => 1,
	b1dma_enable        => B1DMA_on,
	tcpa                => 0,
	ahb_uart            => 1,
	ahb_jtag            => 0,
	ahbstat             => 1,
	uart1_enable        => 1,
	irq3_enable         => 1,
	gpt_enable          => 1,
	i2c_enable          => 0,
	grgpio_enable       => 1,
	kbd_enable          => 0,
	vga_enable          => 0,
	svga_enable         => 0,
	grsysmon            => 0,
	gracectrl           => 0,
	pciexp              => 0,
	natlas              => 0,
	en_amba_monitor     => amba_monitor_on,
	en_eth              => 0,

	umr_data_bitwidth   => 8,

	ahbuart_pindex => 0,
	apbuart_pindex => 1,
	irqmp_pindex   => 2,
	gptimer_pindex => 3,
	ahbstat_pindex => 4,
	mctrl_pindex   => 5,
	ddr_pindex     => -1,
	gc_pindex      => -1,
	cm_pindex      => -1,
	rr_pindex      => -1,
	fi_pindex      => -1,
	dvi_pindex     => -1,
	pwm_pindex     => -1,
	beeper_pindex  => -1,
	eth_pindex     => -1,
	unused_pindex  => 6,


	-- FSB AHBMO indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  => CFG_NCPU,
	na_mst_rx_idx         => CFG_NCPU,
	na_mst_tx_idx         => CFG_NCPU+1,
	mmi64_ahbm_idx        => CFG_NCPU+2,
	eth_mindex            => -1,
	ahb_uart_m_idx        => CFG_NCPU+3,
	icore_bitload_hindexm => CFG_NCPU+4,
	b1dma_hindexm         => CFG_NCPU+5,
	crm_mst_idx           => CFG_NCPU+6,
	loader_mst_idx		  => CFG_NCPU+7,
	enemy_tlm_mst_idx     => CFG_NCPU+8,
	enemy_ddr_mst_idx	  => -1,
	l2c_reverse_mst_idx	  => -1,
	unused_ahbmindex      => CFG_NCPU+9,


	-- FSB AHBSO indices
	apb_hindex               => 0,
	dsu_hindex               => 1,
	na_hindex                => 2,
	ssram_hindex             => 3,
	sim_hindex				 => 13,
	ddr_hindex               => -1,
	l2_hindex                => 4,
	amp_hindex               => 5,
	ram_hindex               => -1,
	cic_hindex               => 6,
	icore_tlm_hindexs        => 7,
	b1dma_hindexs            => 8,
	icore_bitload_hindexs    => 9,
	icore_mi_hindexs         => 10,
	icore_vlcwsto_hindexs    => 11,
	tcpa_buffer_north_hindex => -1,
	tcpa_buffer_west_hindex  => -1,
	tcpa_buffer_south_hindex => -1,
	tcpa_buffer_east_hindex  => -1,
	tcpa_rbuffer_hindex      => -1,
	dvi_hindex               => -1,
	crm_hindex               => 12,
	enemy_tlm_hindex		 => 14,
	unused_ahbsindex         => 15,

	-- na ahb
	l2c_hindexm => 0,

    fabtech   => virtex7,
	memtech   => virtex7,
	padtech   => virtex7,
	transtech => gtp0,
	noasync   => 0,
	scan      => 0
  );

  constant architecture_parameters_tcpa : t_architecture_parameters := (
	maxahbm     => 16,
	maxahbs     => 16,
	maxapb      => 16,

	leon3               => 1,
	en_na               => 1,
	na_ls               => 1,
	na_mp               => 1,
	profpga_ssram       => 1,
	ahbramen            => 0,
	sim                 => CFG_SIM,
	mctrl_leon2         => 0,
	en_ddr              => 0,
	en_dvi2ahb          => 0,
	en_pendulum_control => 0,
	en_beeper           => 0,
	icore               => 0,
	b1dma_enable        => B1DMA_on,
	tcpa                => 1,
	ahb_uart            => 1,
	ahb_jtag            => 0,
	ahbstat             => 1,
	uart1_enable        => 1,
	irq3_enable         => 1,
	gpt_enable          => 1,
	i2c_enable          => 0,
	grgpio_enable       => 1,
	kbd_enable          => 0,
	vga_enable          => 0,
	svga_enable         => 0,
	grsysmon            => 0,
	gracectrl           => 0,
	pciexp              => 0,
	natlas              => 0,
	en_amba_monitor     => amba_monitor_on,
	en_eth              => 0,

	umr_data_bitwidth   => 8,

	ahbuart_pindex =>  0,
	apbuart_pindex =>  1,
	irqmp_pindex   =>  2,
	gptimer_pindex =>  3,
	ahbstat_pindex =>  4,
	mctrl_pindex   =>  5,
	ddr_pindex     => -1,
	gc_pindex      =>  6,
	cm_pindex      =>  7,
	rr_pindex      =>  8,
	fi_pindex      =>  9,
	dvi_pindex     => -1,
	pwm_pindex     => -1,
	beeper_pindex  => -1,
	eth_pindex     => -1,
	unused_pindex  => 10,


	-- FSB AHBMO indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  =>  5,
	na_mst_rx_idx         =>  5,
	na_mst_tx_idx         =>  6,
	mmi64_ahbm_idx        =>  7,
	eth_mindex            => -1,
	ahb_uart_m_idx        =>  8,
	icore_bitload_hindexm => -1,
	b1dma_hindexm         =>  9,
	crm_mst_idx           => 10,
	loader_mst_idx		  => 11,
	enemy_tlm_mst_idx     => 12,
	enemy_ddr_mst_idx	  => -1,
	l2c_reverse_mst_idx	  => -1,
	unused_ahbmindex      => 13,


	-- FSB AHBSO indices
	apb_hindex               =>  0,
	dsu_hindex               =>  1,
	na_hindex                =>  2,
	ssram_hindex             =>  3,
	sim_hindex				 => 14,
	ddr_hindex               => -1,
	l2_hindex                =>  4,
	amp_hindex               =>  5,
	ram_hindex               => -1,
	cic_hindex               =>  6,
	icore_tlm_hindexs        => -1,
	b1dma_hindexs            =>  7,
	icore_bitload_hindexs    => -1,
	icore_mi_hindexs         => -1,
	icore_vlcwsto_hindexs    => -1,
	tcpa_buffer_north_hindex =>  8,
	tcpa_buffer_west_hindex  =>  9,
	tcpa_buffer_south_hindex => 10,
	tcpa_buffer_east_hindex  => 11,
	tcpa_rbuffer_hindex      => 12,
	dvi_hindex               => -1,
	crm_hindex               => 13,
	enemy_tlm_hindex		 => 15,
	unused_ahbsindex         => -1,

	-- na ahb
	l2c_hindexm => 0,

    fabtech   => virtex7,
	memtech   => virtex7,
	padtech   => virtex7,
	transtech => gtp0,
	noasync   => 0,
	scan      => 0
  );

  constant clkgen_parameters_glbl : t_clkgen_parameters := (
	-- clock generator
	clktech   => virtex7,
	clkmul    => 10,
	clkdiv    => 10,
	oclkdiv   => 1,
	oclkbdiv  => 0,
	oclkcdiv  => 0,
	pcidll    => 0,
	pcisysclk => 0,
	clk_nofb  => 0,

	-- clock generator for noc
	clkmuln => 10,
	clkdivn => 10
  );

  constant leon_parameters_glbl : t_leon_parameters := (
	-- leon3 processor core
	nwin          => 8,
	v8            => 16#32#,
	mac           => 0,
	bp            => 1,
	svt           => 1,
	rstaddr       => CFG_RSTADDR,
	lddel         => 1,
	notag         => 0,
	nwp           => 2,
	pwd           => 1*2,
    fpu           => 8 + 16*1,
	grfpush       => 0,
	icen          => 1,
	isets         => 2,
	isetsz        => 8,
	iline         => 8,
	irepl         => 1,
	ilock         => 0,
	ilramen       => 0,
	ilramaddr     => 16#8E#,
	ilramsz       => 1,
	dcen          => 1,
	dsets         => 2,
	dsetsz        => 8,
	dline         => 4,
	drepl         => 1,
	dlock         => 0,
	dsnoop        => 6,
	dfixed        => 16#0#,
	dlramen       => 0,
	dlramaddr     => 16#8F#,
	dlramsz       => 1,
	mmuen         => 0,
	itlbnum       => 8,
	dtlbnum       => 8,
	tlb_type      => 0 + 1*2,
	tlb_rep       => 0,
	mmu_page      => 0,
	dsu           => 1,
	itbsz         => 2,
	atbsz         => 2,
	ahbpf         => 0,
	leon3ft_en    => 0,
	iuft_en       => 0,
	fpuft_en      => 0,
	rf_errinj     => 0,
	cache_ft_en   => 0,
	cache_errinj  => 0,
	leon3_netlist => 0,
	disas         => 0 + 0,
	pclow         => 2,
	np_asi        => 0,
	wrpsr         => 0,
	  en_addr_trans => 0,
	RCAS_EN		  => 0,

	-- CPU FREQ
	board_freq_200 => 200000,
	board_freq     => CFG_CPU_FREQ,
	vco_freq       => 1200000,
	cpu_freq       => CFG_CPU_FREQ
  );

  constant leon_parameters_mmu : t_leon_parameters := (
	-- leon3 processor core
	nwin          => 8,
	v8            => 16#32#,
	mac           => 0,
	bp            => 0,
	svt           => 1,
	rstaddr       => CFG_RSTADDR,
	lddel         => 1,
	notag         => 0,
	nwp           => 2,
	pwd           => 1*2,
    fpu           => 8 + 16*1,
	grfpush       => 0,
	icen          => 1,
	isets         => 2,
	isetsz        => 4,
	iline         => 4,
	irepl         => 1,
	ilock         => 0,
	ilramen       => 0,
	ilramaddr     => 16#8E#,
	ilramsz       => 1,
	dcen          => 1,
	dsets         => 2,
	dsetsz        => 4,
	dline         => 4,
	drepl         => 1,
	dlock         => 0,
	dsnoop        => 6,
	dfixed        => 16#0#,
	dlramen       => 0,
	dlramaddr     => 16#8F#,
	dlramsz       => 1,
	mmuen         => 1,
	itlbnum       => 2,
	dtlbnum       => 2,
	tlb_type      => 2,
	tlb_rep       => 0,
	mmu_page      => 0,
	dsu           => 1,
	itbsz         => 2,
	atbsz         => 2,
	ahbpf         => 0,
	leon3ft_en    => 0,
	iuft_en       => 0,
	fpuft_en      => 0,
	rf_errinj     => 0,
	cache_ft_en   => 0,
	cache_errinj  => 0,
	leon3_netlist => 0,
	disas         => 0 + 0,
	pclow         => 2,
	np_asi        => 0,
	wrpsr         => 0,
	  en_addr_trans => 0,
	  RCAS_EN		  => 0,

	-- CPU FREQ
	board_freq_200 => 200000,
	board_freq     => CFG_CPU_FREQ,
	vco_freq       => 1200000,
	cpu_freq       => CFG_CPU_FREQ
  );

  constant leon_parameters_bigInstTrace : t_leon_parameters := (
	-- leon3 processor core
	nwin          => 8,
	v8            => 16#32#,
	mac           => 0,
	bp            => 0,
	svt           => 1,
	rstaddr       => CFG_RSTADDR,
	lddel         => 1,
	notag         => 0,
	nwp           => 2,
	pwd           => 1*2,
    fpu           => 8 + 16*1,
	grfpush       => 0,
	icen          => 1,
	isets         => 2,
	isetsz        => 8,
	iline         => 8,
	irepl         => 1,
	ilock         => 0,
	ilramen       => 0,
	ilramaddr     => 16#8E#,
	ilramsz       => 1,
	dcen          => 1,
	dsets         => 2,
	dsetsz        => 4,
	dline         => 4,
	drepl         => 1,
	dlock         => 0,
	dsnoop        => 6,
	dfixed        => 16#0#,
	dlramen       => 0,
	dlramaddr     => 16#8F#,
	dlramsz       => 1,
	mmuen         => 0,
	itlbnum       => 8,
	dtlbnum       => 8,
	tlb_type      => 0 + 1*2,
	tlb_rep       => 0,
	mmu_page      => 0,
	dsu           => 1,
	itbsz         => 16,
	atbsz         => 2,
	ahbpf         => 0,
	leon3ft_en    => 0,
	iuft_en       => 0,
	fpuft_en      => 0,
	rf_errinj     => 0,
	cache_ft_en   => 0,
	cache_errinj  => 0,
	leon3_netlist => 0,
	disas         => 0 + 0,
	pclow         => 2,
	np_asi        => 0,
	wrpsr         => 0,
	  en_addr_trans => 0,
    RCAS_EN         => 0,

	-- CPU FREQ
	board_freq_200 => 200000,
	board_freq     => CFG_CPU_FREQ,
	vco_freq       => 1200000,
	cpu_freq       => CFG_CPU_FREQ
  );

  constant amba_parameters_glbl : t_amba_parameters := (
	-- amba settings
	defmst      => 15,
	rrobin      => 1,
	split       => 1,
	fpnpen      => 1,
	ahbio       => 16#FFF#,
	apbaddr     => MemMAP.APB_HADDR,
	ahb_mon     => 0,
	ahb_monerr  => 0,
	ahb_monwar  => 0,
	ahb_dtrace  => 0,
	en_ahb2ahb  => 1,
	en_async_na => 1
  );

  constant ethdsu_paramters_glbl : t_ethdsu_parameters := (
	-- ethernet dsu
	dsu_eth => 0 + 0,
	eth_buf => 1,
	eth_ipm => 16#C0A8#,
	eth_ipl => 16#0033#,
	eth_enm => 16#020000#,
	eth_enl => 16#000009#
  );

  constant mem_parameters_glbl : t_mem_parameters := (
	new_ssram => 1,
	bram_tlm => 0,
	sram_addr_w  => 22,
	sram_dq_pins => 18,
	sram_groups  => 2,
	-- tlm
	mctrl_paddr  => 0,
	tlm_start_addr => MemMAP.TLM_haddr,
	tlm_end_addr => CFG_TLM_END_ADDR,
	tlm_mask	=> MemMAP.TLM_hmask,
	-- ahb rom
	ahbropip => 0,
	ahbroddr => MemMAP.ICORE_TLM_HADDR,
	romaddr  => 16#000#,
	rommask  => 16#e00# + 16#000#,
	-- ahb ram
	ram_size  => 2048,
	ram_haddr => 16#000#,
	ahbrpipe  => 0,
	-- leon2 memory controller
	mctrl_ram8bit  => 0,
	mctrl_ram16bit => 1,
	mctrl_5cs      => 0,
	mctrl_sden     => 0,
	mctrl_sepbus   => 0,
	mctrl_invclk   => 0,
	mctrl_sd64     => 0,
	mctrl_page     => 0 + 0
  );

  constant ddr_parameters_glbl : t_ddr_parameters := (
	ddr_addr  => MemMAP.SHM_haddr,
	ddr_mask  => MemMAP.SHM_hmask,
	ddr_bus_io => MemMAP.SHM_IO,
	ddr_paddr => 2
  );

  constant ahbstat_parameters_glbl : t_ahbstat_parameters := (
	-- ahb status register
	ahbstat_paddr => 15,
	ahbstat_pirq  => 7,
	ahbstatn      => 1
  );

  constant geth_parameters_glbl : t_geth_parameters := (
	-- gaisler ethernet core
	paddr => 5,
	pmask => 16#FFF#,
	pirq  => 5,

	ifg_gap         => 24,
	attempt_limit   => 16,
	backoff_limit   => 10,
	slot_time       => 128,
	mdcscaler       => 40,
	enable_mdio     => 1,
	fifosize        => 32,
	nsync           => 1,
	edcl            => 0,
	edclbufsz       => 64,
	burstlength     => 32,
	macaddrh        => 16#00005E#,
	macaddrl        => 16#000000#,
	ipaddrh         => 16#c0a8#,
	ipaddrl         => 16#0035#,
	phyrstadr       => 1,
	rmii            => 0,
	sim             => 0,
	giga            => 1,
	oepol           => 0,
	scanen          => 0,
	ft              => 0,
	edclft          => 0,
	mdint_pol       => 0,
	enable_mdint    => 0,
	multicast       => 1,
	ramdebug        => 0,
	mdiohold        => 1,
	maxsize         => 1500,
	gmiimode        => 1,

	greth    => 0,
	greth1g  => 0,
	eth_fifo => 8
  );

  constant geth_parameters_giga : t_geth_parameters := (
	-- gaisler ethernet core
	paddr => 5,
	pmask => 16#FFF#,
	pirq  => 5,

	ifg_gap         => 24,
	attempt_limit   => 16,
	backoff_limit   => 10,
	slot_time       => 128,
	mdcscaler       => 40,
	enable_mdio     => 1,
	fifosize        => 32,
	nsync           => 1,
	edcl            => 1,
	edclbufsz       => 64,
	burstlength     => 32,
	macaddrh        => 16#00005E#,
	macaddrl        => 16#000000#,
	ipaddrh         => 16#c0a8#,
	ipaddrl         => 16#0035#,
	phyrstadr       => 1,
	rmii            => 0,
	sim             => 0,
	giga            => 1,
	oepol           => 0,
	scanen          => 0,
	ft              => 0,
	edclft          => 0,
	mdint_pol       => 0,
	enable_mdint    => 0,
	multicast       => 1,
	ramdebug        => 0,
	mdiohold        => 1,
	maxsize         => 1500,
	gmiimode        => 1,

	greth    => 0,
	greth1g  => 0,
	eth_fifo => 8
  );

  constant uart_parameters_glbl : t_uart_parameters := (
	-- uart
	ahbuart_paddr => 7,
	apbuart_paddr => 1,
	apbuart_pirq  => 2,
	uart1_fifo    => 4,
	duart         => 1
  );

  constant irq_parameters_glbl : t_irq_parameters := (
	-- leon3 interrupt controller
	irqmp_paddr => 2,
	irq3_nsec   => 0
  );

  constant timer_parameters_glbl : t_timer_parameters := (
	-- modular timer
	gptimer_paddr => 3,
	gpt_ntim      => 2,
	gpt_sw        => 8,
	gpt_tw        => 32,
	gpt_irq       => 8,
	gpt_sepirq    => 1,
	gpt_wdogen    => 0,
	gpt_wdog      => 16#0#
  );

  constant gpio_parameters_glbl : t_gpio_parameters := (
	-- gpio port
	grgpio_imask => 16#0FFFE#,
	grgpio_width => 32
  );

  constant pcie_parameters_glbl : t_pcie_parameters := (
	-- pciexp interface
	pcie_type    => 0,
	pcie_sim_mas => 0,
	pciexpvid    => 16#0#,
	pciexpdid    => 16#0#,
	no_of_lanes  => 1
  );

  constant na_parameters_glbl : t_na_parameters := (
	-- Network Adapter Load Store slave
	narsz           => 2,
	naaddr_ls       => MemMAP.SHM_haddr,
	namask_ls       => 16#800#,
	burst_size      => 8,
	na_split_enable => NA_SPLIT_ENABLE,
	ntilemasters    => 1,

	-- Network Adapter Message Passing slave
	naaddr_mp => CFG_NAADDR_MP,
	namask_mp => 16#FFE#,
	na_irq    => 11,

	-- CIC i-let enqueue addresses (currently dummy TLM locations)
	cic_control_addr   => MemMAP.TLM_start(31 downto 24) & x"010100",
	cic_enqueue_addr   => MemMAP.TLM_start(31 downto 24) & x"010200",
	sys_ilet_dst_addr  => MemMAP.TLM_start(31 downto 24) & x"010300",
	tile_id_subtrahend => TILE_ID_SUBTRAHEND,
	address_offset     => MemMAP.TLM_start(31 downto 24),
	mon_global_addr    => MON_GLOBAL_ADDR,
	mon_address_offset => x"90",

	ahb_addr_bar0   => MemMAP.SHM_haddr,
	ahb_addr_bar1   => to_integer(unsigned(MemMAP.RMAP_start(31 downto 20))),
	ahb_addr_bar2   => MemMAP.L2C_CTRL_0_HADDR,
	ahb_addr_bar3   => MemMAP.L2C_CTRL_1_HADDR,
	ahb_addr_bar4   => MemMAP.L2C_CTRL_2_HADDR,
	ahb_para0_bar0  => '1',
	ahb_para0_bar1  => '1',
	ahb_para0_bar2  => '0',
	ahb_para0_bar3  => '0',
	ahb_para0_bar4  => '0',
	ahb_para1_bar0  => '1',
	ahb_para1_bar1  => '1',
	ahb_para1_bar2  => '0',
	ahb_para1_bar3  => '0',
	ahb_para1_bar4  => '0',
	ahb_mask_bar0   => MemMAP.SHM_hmask,
	ahb_mask_bar1   => 16#FC0#,
	ahb_mask_bar2   => MemMAP.L2C_CTRL_0_HMASK,
	ahb_mask_bar3   => 16#FFF#,
	ahb_mask_bar4   => MemMAP.L2C_CTRL_2_HMASK
  );

  constant na_parameters_ddr : t_na_parameters := (
	-- Network Adapter Load Store slave
	narsz           => 2,
	naaddr_ls       => to_integer(unsigned(MemMAP.TLM_start(31 downto 20))),
	namask_ls       => MemMAP.SHM_hmask,
	burst_size      => 8,
	na_split_enable => NA_SPLIT_ENABLE,
	ntilemasters    => 1,

	-- Network Adapter Message Passing slave
	naaddr_mp => CFG_NAADDR_MP,
	namask_mp => 16#FFE#,
	na_irq    => 11,

	-- CIC i-let enqueue addresses (currently dummy TLM locations)
	cic_control_addr   => MemMAP.TLM_start(31 downto 24) & x"010100",
	cic_enqueue_addr   => MemMAP.TLM_start(31 downto 24) & x"010200",
	sys_ilet_dst_addr  => MemMAP.TLM_start(31 downto 24) & x"010300",
	tile_id_subtrahend => TILE_ID_SUBTRAHEND,
	address_offset     => MemMAP.TLM_start(31 downto 24),
	mon_global_addr    => MON_GLOBAL_ADDR,
	mon_address_offset => x"90",

	ahb_addr_bar0   => to_integer(unsigned(MemMAP.RMAP_start(31 downto 20))),
	ahb_addr_bar1   => MemMAP.L2C_CTRL_2_HADDR,
	ahb_addr_bar2   => MemMAP.L2C_CTRL_0_HADDR,
	ahb_addr_bar3   => MemMAP.L2C_CTRL_1_HADDR,
	ahb_addr_bar4   => MemMAP.L2C_CTRL_2_HADDR,
	ahb_para0_bar0  => '1',
	ahb_para0_bar1  => '0',
	ahb_para0_bar2  => '0',
	ahb_para0_bar3  => '0',
	ahb_para0_bar4  => '0',
	ahb_para1_bar0  => '1',
	ahb_para1_bar1  => '0',
	ahb_para1_bar2  => '0',
	ahb_para1_bar3  => '0',
	ahb_para1_bar4  => '0',
	ahb_mask_bar0   => 16#FC0#,
	ahb_mask_bar1   => MemMAP.L2C_CTRL_2_HMASK,
	ahb_mask_bar2   => MemMAP.L2C_CTRL_0_HMASK,
	ahb_mask_bar3   => 16#FFF#,
	ahb_mask_bar4   => MemMAP.L2C_CTRL_2_HMASK
  );

  -- CRM Parameters
  constant crm_parameters_glbl : t_crm_parameters := (
	crm_en => 0, -- set 1 to enable coherency region manager (CRM)
	crm_ahbs_ioaddr => MemMAP.CRM_HADDR, -- CRM slave address
	crm_ahbs_iomask => 16#FFF#, -- CRM slave address mask
	burst_gen_en => 0
  --traffic_addr => x"42000000",
  --traffic_mask => x"FFC00000"
  );

  constant icore_parameters_glbl : t_icore_parameters := (
	-- Permutator
	perm => 0,

	-- ICORE
	-- TLM address and size
	tlm_enable => 1,
	tlm_addr   => MemMAP.ICORE_TLM_HADDR, --16#808#,
	tlm_size   => 128,

	--mmio-ahb-interface
	mmio_addr => MemMAP.ICORE_MMIO_HADDR, --16#D00#,

	--IRQ generator
	irqgen_enable => 0,

	--Bitstream Loader
	bitloader_enable => 1,

	--B1 DMA Controller
	b1dma_addr => MemMAP.ICORE_B1DMA_HADDR, --16#D03#,
	b1dma_irq  => 15,

	-- i-Core AHB Adresses & Masks
	icore_bitload_haddr => MemMAP.ICORE_BIT_HADDR, --16#D02#,
	icore_bitload_hmask => 16#FFF#,
	icore_vlcwsto_haddr => MemMAP.ICORE_VLCCWSTO_HADDR, --16#D01#,
	icore_vlcwsto_hmask => 16#FFF#
  );

  constant tcpa_parameters_glbl : t_tcpa_parameters := (
	NUM_OF_BUFFER_STRUCTURES              => CUR_DEFAULT_NUM_OF_BUFFER_STRUCTURES,
	BUFFER_SIZE                           => CUR_DEFAULT_MAX_BUFFER_SIZE,
	BUFFER_SIZE_ADDR_WIDTH                => CUR_DEFAULT_BUFFER_ADDR_WIDTH,
	BUFFER_CHANNEL_SIZE                   => CUR_DEFAULT_BUFFER_CHANNEL_SIZE,
	BUFFER_CHANNEL_ADDR_WIDTH             => CUR_DEFAULT_BUFFER_CHANNEL_ADDR_WIDTH,
	BUFFER_CHANNEL_SIZES_ARE_POWER_OF_TWO => CUR_DEFAULT_CHANNEL_SIZES_ARE_POWER_OF_TWO,
	EN_ELASTIC_BUFFER                     => CUR_DEFAULT_EN_ELASTIC_BUFFER,
	AG_BUFFER_CONFIG_SIZE                 => CUR_DEFAULT_AG_BUFFER_CONFIG_SIZE,

	ag_buffer_north => (
	  CUR_DEFAULT_AG_BUFFER_NORTH.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_NORTH.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_NORTH.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_NORTH.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_north_hindex,
	  0,       -- hirq
	  16#850#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_NORTH

	ag_buffer_west => (
	  CUR_DEFAULT_AG_BUFFER_WEST.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_WEST.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_WEST.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_WEST.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_west_hindex,      -- hindex
	  0,       -- hirq
	  16#860#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_WEST

	ag_buffer_south => (
	  CUR_DEFAULT_AG_BUFFER_SOUTH.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_south_hindex,      -- hindex
	  0,       -- hirq
	  16#870#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_SOUTH

	ag_buffer_east => (
	  CUR_DEFAULT_AG_BUFFER_EAST.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_EAST.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_EAST.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_EAST.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_east_hindex,      -- hindex
	  0,       -- hirq
	  16#880#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_EAST



	rbuffer_hirq_ahb_addr => 16#890#,
	rbuffer_hirq_ahb_mask => 16#FFF#,
	rbuffer_hirq_ahb_irq  => 12,

	gc_paddr => MemMAP.TLM_haddr,
	gc_pmask => 16#FFC#,
	gc_pirq  => 3,
	cm_paddr => 16#820#,
	cm_pmask => 16#FF0#,
	rr_paddr => 16#810#,
	rr_pmask => 16#FF0#,
	fi_paddr => 16#8A0#,
	fi_pmask => 16#FFC#,
	fi_pirq  => 5,

	index_vector_dimension  => 3,
	index_vector_data_width => 32,
	matrix_pipeline_depth   => 2,

	iteration_variable_width => 24,
	dimension                => 3,
	select_width             => 3,
	no_reg_to_program        => 4,
	matrix_element_width     => 24,
	data_width               => 24,
	max_no_of_program_blocks => 35,
	num_of_ic_signals        => 3
  );


  constant tcpa_parameters_global_buffers : t_tcpa_parameters := (
	NUM_OF_BUFFER_STRUCTURES              => CUR_DEFAULT_NUM_OF_BUFFER_STRUCTURES,
	BUFFER_SIZE                           => CUR_DEFAULT_MAX_BUFFER_SIZE,
	BUFFER_SIZE_ADDR_WIDTH                => CUR_DEFAULT_BUFFER_ADDR_WIDTH,
	BUFFER_CHANNEL_SIZE                   => CUR_DEFAULT_BUFFER_CHANNEL_SIZE,
	BUFFER_CHANNEL_ADDR_WIDTH             => CUR_DEFAULT_BUFFER_CHANNEL_ADDR_WIDTH,
	BUFFER_CHANNEL_SIZES_ARE_POWER_OF_TWO => CUR_DEFAULT_CHANNEL_SIZES_ARE_POWER_OF_TWO,
	EN_ELASTIC_BUFFER                     => CUR_DEFAULT_EN_ELASTIC_BUFFER,
	AG_BUFFER_CONFIG_SIZE                 => CUR_DEFAULT_AG_BUFFER_CONFIG_SIZE,

	ag_buffer_north => (
	  CUR_DEFAULT_AG_BUFFER_NORTH.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_NORTH.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_NORTH.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_NORTH.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_north_hindex,
	  0,       -- hirq
	  16#850#, -- haddr
	  16#FFE#  -- hmask
	), -- AG_BUFFER_NORTH

	ag_buffer_west => (
	  CUR_DEFAULT_AG_BUFFER_WEST.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_WEST.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_WEST.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_WEST.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_west_hindex,      -- hindex
	  0,       -- hirq
	  16#808#, -- haddr
	  16#FFE#  -- hmask
	), -- AG_BUFFER_WEST

	ag_buffer_south => (
	  CUR_DEFAULT_AG_BUFFER_SOUTH.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_south_hindex,      -- hindex
	  0,       -- hirq
	  16#854#, -- haddr
	  16#FFE#  -- hmask
	), -- AG_BUFFER_SOUTH

	ag_buffer_east => (
	  CUR_DEFAULT_AG_BUFFER_EAST.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_EAST.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_EAST.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_EAST.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_east_hindex,      -- hindex
	  0,       -- hirq
	  16#80A#, -- haddr
	  16#FFE#  -- hmask
	), -- AG_BUFFER_EAST

	rbuffer_hirq_ahb_addr => 16#858#,
	rbuffer_hirq_ahb_mask => 16#FFF#,
	rbuffer_hirq_ahb_irq  => 12,

	gc_paddr => MemMAP.TLM_haddr,
	gc_pmask => 16#FFC#,
	gc_pirq  => 3,
	cm_paddr => 16#820#,
	cm_pmask => 16#FF0#,
	rr_paddr => 16#810#,
	rr_pmask => 16#FF0#,
	fi_paddr => 16#8A0#,
	fi_pmask => 16#FFC#,
	fi_pirq  => 5,

	index_vector_dimension  => 3,
	index_vector_data_width => 32,
	matrix_pipeline_depth   => 2,

	iteration_variable_width => 24,
	dimension                => 3,
	select_width             => 3,
	no_reg_to_program        => 4,
	matrix_element_width     => 24,
	data_width               => 24,
	max_no_of_program_blocks => 35,
	num_of_ic_signals        => 3
  );

  constant tcpa_parameters_west_global : t_tcpa_parameters := (
	NUM_OF_BUFFER_STRUCTURES              => CUR_DEFAULT_NUM_OF_BUFFER_STRUCTURES,
	BUFFER_SIZE                           => CUR_DEFAULT_MAX_BUFFER_SIZE,
	BUFFER_SIZE_ADDR_WIDTH                => CUR_DEFAULT_BUFFER_ADDR_WIDTH,
	BUFFER_CHANNEL_SIZE                   => CUR_DEFAULT_BUFFER_CHANNEL_SIZE,
	BUFFER_CHANNEL_ADDR_WIDTH             => CUR_DEFAULT_BUFFER_CHANNEL_ADDR_WIDTH,
	BUFFER_CHANNEL_SIZES_ARE_POWER_OF_TWO => CUR_DEFAULT_CHANNEL_SIZES_ARE_POWER_OF_TWO,
	EN_ELASTIC_BUFFER                     => CUR_DEFAULT_EN_ELASTIC_BUFFER,
	AG_BUFFER_CONFIG_SIZE                 => CUR_DEFAULT_AG_BUFFER_CONFIG_SIZE,

	ag_buffer_north => (
	  CUR_DEFAULT_AG_BUFFER_NORTH.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_NORTH.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_NORTH.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_NORTH.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_NORTH.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_north_hindex,
	  8,       -- hirq
	  16#850#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_NORTH

	ag_buffer_west => (
	  CUR_DEFAULT_AG_BUFFER_WEST.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_WEST.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_WEST.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_WEST.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_WEST.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_west_hindex,      -- hindex
	  9,       -- hirq
	  16#808#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_WEST

	ag_buffer_south => (
	  CUR_DEFAULT_AG_BUFFER_SOUTH.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_SOUTH.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_south_hindex,      -- hindex
	  11,       -- hirq
	  16#858#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_SOUTH

	ag_buffer_east => (
	  CUR_DEFAULT_AG_BUFFER_EAST.DESIGN_TYPE,
	  CUR_DEFAULT_AG_BUFFER_EAST.ENABLE_PIXEL_BUFFER_MODE,

	  CUR_DEFAULT_AG_BUFFER_EAST.CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.CHANNEL_COUNT,

	  CUR_DEFAULT_AG_BUFFER_EAST.AG_CONFIG_ADDR_WIDTH,

	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CONFIG_ADDR_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CONFIG_DATA_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_ADDR_HEADER_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_SEL_REG_WIDTH,
	  CUR_DEFAULT_AG_BUFFER_EAST.BUFFER_CSR_DELAY_SELECTOR_WIDTH,

	  architecture_parameters_tcpa.tcpa_buffer_east_hindex,      -- hindex
	  12,       -- hirq
	  16#85C#, -- haddr
	  16#FFC#  -- hmask
	), -- AG_BUFFER_EAST

	rbuffer_hirq_ahb_addr => 16#860#,
	rbuffer_hirq_ahb_mask => 16#FFF#,
	rbuffer_hirq_ahb_irq  => 13,

	gc_paddr => 16#801#,
	gc_pmask => 16#FFC#,
	gc_pirq  => 3,
	cm_paddr => 16#820#,
	cm_pmask => 16#FF0#,
	rr_paddr => 16#810#,
	rr_pmask => 16#FF0#,
	fi_paddr => 16#8A0#,
	fi_pmask => 16#FFC#,
	fi_pirq  => 14,

	index_vector_dimension  => 3,
	index_vector_data_width => 32,
	matrix_pipeline_depth   => 2,

	iteration_variable_width => 24,
	dimension                => 3,
	select_width             => 3,
	no_reg_to_program        => 4,
	matrix_element_width     => 24,
	data_width               => 24,
	max_no_of_program_blocks => 35,
	num_of_ic_signals        => 3
  );

  constant dvi_parameters_glbl : t_dvi_parameters := (
	dvi_haddr            => 16#808#,
	dvi_hirq             => 8,
	dvi_paddr            => 16#8c0#,
	dvi_pmask            => 16#fff#,
	pwm_paddr            => 16#8d0#,
	pwm_pmask            => 16#fff#,
	beeper_paddr         => 16#8e0#,
	beeper_counter_width => 25,
	beeper_divider_width => 16
  );

  constant dbg_parameters_glbl : t_dbg_parameters := (
	  en_ila			=> 0,
	  en_ila_icrm		=> 0,
	  en_ila_iNA_RX		=> 0,
	  en_ila_ahb2mig	=> 0,
	  en_eth_ila		=> 0,
	  en_ila_ENEMY_DDR	=> 0,
	  en_ila_GRAPH_WB	=> 0,
	  en_ila_fifo2bus	=> 0,
	  en_ila_bus2fifo	=> 0,
	  en_ila_hwldma		=> 0
  );

  constant dbg_parameters_ddr : t_dbg_parameters := (
	  en_ila			=> 0,
	  en_ila_icrm		=> 0,
	  en_ila_iNA_RX		=> 0,
	  en_ila_ahb2mig	=> 0,
	  en_eth_ila		=> 0,
	  en_ila_ENEMY_DDR	=> 0,
	  en_ila_GRAPH_WB	=> 0,
	  en_ila_fifo2bus	=> 0,
	  en_ila_bus2fifo	=> 0,
	  en_ila_hwldma		=> 0
  );


--	constant tile_config_1x1 : t_tile_config_array(0 to 0, 0 to 0) := (
--	(
--	  ( -- tile_config for tile (y,x) (0)(0)
--		architecture_parameters_glbl,
--		MemMAP,
--		clkgen_parameters_glbl,
--		leon_parameters_glbl,
--		amba_parameters_glbl,
--		ethdsu_paramters_glbl,
--		mem_parameters_glbl,
--		ddr_parameters_glbl,
--		ahbstat_parameters_glbl,
--		geth_parameters_glbl,
--		uart_parameters_glbl,
--		irq_parameters_glbl,
--		timer_parameters_glbl,
--		gpio_parameters_glbl,
--		pcie_parameters_glbl,
--		na_parameters_glbl,
--		crm_parameters_glbl,
--		icore_parameters_glbl,
--		tcpa_parameters_glbl,
--		dvi_parameters_glbl,
--		dbg_parameters_glbl
--	  )
--	 )
--	);

--	constant tile_config_2x1 : t_tile_config_array(0 to 1, 0 to 0) := (
--	(
--	  ( -- tile_config for tile (y,x) (0)(0)
--		architecture_parameters_glbl,
--		MemMAP,
--		clkgen_parameters_glbl,
--		leon_parameters_glbl,
--		amba_parameters_glbl,
--		ethdsu_paramters_glbl,
--		mem_parameters_glbl,
--		ddr_parameters_glbl,
--		ahbstat_parameters_glbl,
--		geth_parameters_glbl,
--		uart_parameters_glbl,
--		irq_parameters_glbl,
--		timer_parameters_glbl,
--		gpio_parameters_glbl,
--		pcie_parameters_glbl,
--		na_parameters_glbl,
--		crm_parameters_glbl,
--		icore_parameters_glbl,
--		tcpa_parameters_glbl,
--		dvi_parameters_glbl,
--		dbg_parameters_glbl
--	  ),
--	  ( -- tile_config for tile (0)(1)
--		architecture_parameters_glbl,
--		MemMAP,
--		clkgen_parameters_glbl,
--		leon_parameters_glbl,
--		amba_parameters_glbl,
--		ethdsu_paramters_glbl,
--		mem_parameters_glbl,
--		ddr_parameters_glbl,
--		ahbstat_parameters_glbl,
--		geth_parameters_glbl,
--		uart_parameters_glbl,
--		irq_parameters_glbl,
--		timer_parameters_glbl,
--		gpio_parameters_glbl,
--		pcie_parameters_glbl,
--		na_parameters_glbl,
--		crm_parameters_glbl,
--		icore_parameters_glbl,
--		tcpa_parameters_glbl,
--		dvi_parameters_glbl,
--		dbg_parameters_glbl
--	  )
--	 )
--	);

	constant tile_config_2x2 : t_tile_config_array(0 to 1, 0 to 1) := (
	(
	  ( -- tile_config for tile (y,x) (0)(0)
		architecture_parameters_glbl, -- marker: DDR_single_arch
		MemMAP,
		clkgen_parameters_glbl,
		leon_parameters_glbl,
		amba_parameters_glbl,
		ethdsu_paramters_glbl,
		mem_parameters_glbl,
		ddr_parameters_glbl,
		ahbstat_parameters_glbl,
		geth_parameters_glbl,
		uart_parameters_glbl,
		irq_parameters_glbl,
		timer_parameters_glbl,
		gpio_parameters_glbl,
		pcie_parameters_glbl,
		na_parameters_glbl, -- marker: DDR_single_na
		crm_parameters_glbl,
		icore_parameters_glbl,
		tcpa_parameters_glbl,
		dvi_parameters_glbl,
		dbg_parameters_glbl
	  ),
	  ( -- tile_config for tile (0)(1)
		architecture_parameters_glbl,
		MemMAP,
		clkgen_parameters_glbl,
		leon_parameters_glbl,
		amba_parameters_glbl,
		ethdsu_paramters_glbl,
		mem_parameters_glbl,
		ddr_parameters_glbl,
		ahbstat_parameters_glbl,
		geth_parameters_glbl,
		uart_parameters_glbl,
		irq_parameters_glbl,
		timer_parameters_glbl,
		gpio_parameters_glbl,
		pcie_parameters_glbl,
		na_parameters_glbl,
		crm_parameters_glbl,
		icore_parameters_glbl,
		tcpa_parameters_glbl,
		dvi_parameters_glbl,
		dbg_parameters_glbl
	  )
	),
	(( -- tile_config for tile (1)(0)
		architecture_parameters_glbl,
		MemMAP,
		clkgen_parameters_glbl,
		leon_parameters_glbl,
		amba_parameters_glbl,
		ethdsu_paramters_glbl,
		mem_parameters_glbl,
		ddr_parameters_glbl,
		ahbstat_parameters_glbl,
		geth_parameters_glbl,
		uart_parameters_glbl,
		irq_parameters_glbl,
		timer_parameters_glbl,
		gpio_parameters_glbl,
		pcie_parameters_glbl,
		na_parameters_glbl,
		crm_parameters_glbl,
		icore_parameters_glbl,
		tcpa_parameters_glbl,
		dvi_parameters_glbl,
		dbg_parameters_glbl
	  ),
	  ( -- tile_config for tile (1)(1)
		architecture_parameters_ddr, -- marker: DDR_arch
		MemMAP,
		clkgen_parameters_glbl,
		leon_parameters_glbl,
		amba_parameters_glbl,
		ethdsu_paramters_glbl,
		mem_parameters_glbl,
		ddr_parameters_glbl,
		ahbstat_parameters_glbl,
		geth_parameters_glbl,
		uart_parameters_glbl,
		irq_parameters_glbl,
		timer_parameters_glbl,
		gpio_parameters_glbl,
		pcie_parameters_glbl,
		na_parameters_ddr, -- marker: DDR_na
		crm_parameters_glbl,
		icore_parameters_glbl,
		tcpa_parameters_glbl,
		dvi_parameters_glbl,
		dbg_parameters_glbl
	  )
	 )
	);

	constant tile_config_4x4 : t_tile_config_array(0 to 3, 0 to 3) := (
    (
	  ( -- tile_config for tile (y,x) (0)(0)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
        crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (0)(1)
		architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
        crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
       ( -- tile_config for tile (0)(2)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (0)(3)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      )
    ),
    (( -- tile_config for tile (1)(0)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
        crm_parameters_glbl,
        icore_parameters_glbl,
		tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (1)(1)
		architecture_parameters_ddr, -- marker: DDR_arch
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
		na_parameters_ddr, -- marker: DDR_na
        crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
		dbg_parameters_ddr
      ),
       ( -- tile_config for tile (1)(2)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (1)(3)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      )
    ),
    (( -- tile_config for tile (2)(0)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (2)(1)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
       ( -- tile_config for tile (2)(2)
		architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
		na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (2)(3)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      )
    ),
    (( -- tile_config for tile (3)(0)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (3)(1)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
       ( -- tile_config for tile (3)(2)
        architecture_parameters_glbl,
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_glbl,
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
        dbg_parameters_glbl
      ),
      ( -- tile_config for tile (3)(3)
        architecture_parameters_ddr, -- marker: DDR_arch
		MemMAP,
        clkgen_parameters_glbl,
        leon_parameters_glbl,
        amba_parameters_glbl,
        ethdsu_paramters_glbl,
        mem_parameters_glbl,
        ddr_parameters_glbl,
        ahbstat_parameters_glbl,
        geth_parameters_glbl,
        uart_parameters_glbl,
        irq_parameters_glbl,
        timer_parameters_glbl,
        gpio_parameters_glbl,
        pcie_parameters_glbl,
        na_parameters_ddr, -- marker: DDR_na
		crm_parameters_glbl,
        icore_parameters_glbl,
        tcpa_parameters_glbl,
        dvi_parameters_glbl,
		dbg_parameters_ddr
      )
    )
  );

	constant tile_config : t_tile_config_array(0 to CFG_DIM_Y-1, 0 to CFG_DIM_X-1) := tile_config_2x2;

end;
