library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library top;
use top.config_types.all;
use top.config.all;

library grlib;
use grlib.devices.all;
use grlib.amba.all;
use grlib.stdlib.all;

library techmap;
use techmap.gencomp.all;

library gaisler;
use gaisler.leon3.all;
use gaisler.uart.all;
use gaisler.misc.all;
use gaisler.jtag.all;
use gaisler.l2cache.all;
use gaisler.memctrl.all;
-- pragma translate_off
use gaisler.sim.all;
-- pragma translate_on
use gaisler.net.all;

library ina;
library ina_pkg;
use ina_pkg.ina_config.all;
use ina_pkg.enemy_package.all;
use ina_pkg.ina_package.all;

library enemy;
library enemy_pkg;
--use enemy_pkg.enemy_package.all;
use enemy_pkg.crc_package.all;
use enemy_pkg.ina_enemy_package.all;

library esa;
use esa.memoryctrl.all;

library lis;
use lis.config.all;
use lis.cic.all;
use lis.mon.all;

library profpga;
use profpga.mmi64_pkg.all;

library icrm;
use icrm.crm_pkg.all;
use icrm.all;

--CES-iCore
library ces;
use ces.libicore.all;
use ces.atomfw.all;
use ces.libtlm.all;
--use ces.libcamera.all;
use ces.icore_leon3.all;
use ces.libmisc.all;

library ssram_ctrl;
	use ssram_ctrl.ssram_ctrl.all;

library tcpa_lib;
use tcpa_lib.tcpa_top_lib.all;


entity leon3mp is
  generic (
    CFG               : t_tile_config;
    X_LOC             : integer    := 0;
    Y_LOC             : integer    := 0;
    MASTER_XACTOR_ID  : integer    := 0;
    FPGA_ID           : integer    := 0;
    MMI64_ID          : bit_vector := X"00000000"
    );
  port (
	clk_sys : in std_ulogic;               -- 25 MHz main clock
	clk200  : in std_ulogic;               -- 200 MHz clock
	clk_gtx : in std_ulogic;               -- 125 MHz clock

	clk400_p : in std_logic; --400MHz clock for DDR
	clk400_n : in std_logic;

	ext_rst_sys_n : in  std_ulogic;
	rst_sys_n : in  std_ulogic;
	rst_ddr_n : in std_ulogic;

	--MMI64/proFPGA--
	mmi64_clk           : in  std_ulogic;
	mmi64_reset         : in  std_ulogic;
	mmi64_h_up_d_o      : out mmi64_data_t;
	mmi64_h_up_valid_o  : out std_ulogic;
	mmi64_h_up_accept_i : in  std_ulogic;
	mmi64_h_up_start_o  : out std_ulogic;
	mmi64_h_up_stop_o   : out std_ulogic;

	mmi64_h_dn_d_i      : in  mmi64_data_t;
	mmi64_h_dn_valid_i  : in  std_ulogic;
	mmi64_h_dn_accept_o : out std_ulogic;
	mmi64_h_dn_start_i  : in  std_ulogic;
	mmi64_h_dn_stop_i   : in  std_ulogic;

	--MMI64 END--


	ahb_na_ls_si : out ahb_slv_in_type;
	ahb_na_ls_so : in  ahb_slv_out_type;

	ahb_na_mp_si : out ahb_slv_in_type;
	ahb_na_mp_so : in  ahb_slv_out_type;

	na_mst_rx_in  : out ahb_mst_in_type;
	na_mst_rx_out : in  ahb_mst_out_type;

	na_mst_tx_in  : out ahb_mst_in_type;
	na_mst_tx_out : in  ahb_mst_out_type;

	txd1 : out std_ulogic;              -- UART1 tx data
	rxd1 : in  std_ulogic;              -- UART1 rx data

	na_irq : in na_irq_type;

	bus_error : out   std_logic_vector(16 downto 0);

	-- SSRAM interface
	sram_k_p    : out   std_logic;
	sram_k_n    : out   std_logic;
	sram_a      : out   std_logic_vector (CFG.mem.SRAM_ADDR_W-1 downto 0);
	sram_dq     : inout std_logic_vector (CFG.mem.SRAM_DQ_PINS-1 downto 0);
	sram_bws_n  : out   std_logic_vector (CFG.mem.SRAM_GROUPS-1 downto 0);
	sram_rnw    : out   std_logic;
	sram_ld_n   : out   std_logic;
	sram_doff_n : out   std_logic;
	sram_ready	: out	std_logic;

	-- DDR
	ddr3_dq         : inout std_logic_vector(63 downto 0);
	ddr3_dqs_p      : inout std_logic_vector(7 downto 0);
	ddr3_dqs_n      : inout std_logic_vector(7 downto 0);
	ddr3_addr       : out   std_logic_vector(14 downto 0);
	ddr3_ba         : out   std_logic_vector(2 downto 0);
	ddr3_ras_n      : out   std_logic;
	ddr3_cas_n      : out   std_logic;
	ddr3_we_n       : out   std_logic;
	ddr3_reset_n    : out   std_logic;
	ddr3_ck_p       : out   std_logic_vector(0 downto 0);
	ddr3_ck_n       : out   std_logic_vector(0 downto 0);
	ddr3_cke        : out   std_logic_vector(0 downto 0);
	ddr3_cs_n       : out   std_logic_vector(0 downto 0);
	ddr3_dm         : out   std_logic_vector(7 downto 0);
	ddr3_odt        : out   std_logic_vector(0 downto 0);
	ddr3_calib_done : out   std_logic;

	-- Ethernet interface
	eth_clk_to_mac        : in std_logic;
	eth_col_clk_mac_freq  : in std_logic;
	eth_crs_rgmii_sel0    : in std_logic;
	eth_gtx_clk_tck       : out std_logic;
	eth_mdc               : out std_logic;
	eth_mdio              : inout std_logic;
	eth_ninterrupt        : in std_logic;
	eth_nreset            : out std_logic;
	eth_rx_clk            : in std_logic;
	eth_rx_dv_rck         : in std_logic;
	eth_rx_er_rxdv_er     : in std_logic;
	eth_rxd               : in std_logic_vector(7 downto 0);
	eth_tx_clk_rgmii_sel1 : in std_logic;
	eth_tx_en_txen_er     : out std_logic;
	eth_tx_er             : out std_logic;
	eth_txd               : out std_logic_vector(7 downto 0);
	lock					: out std_ulogic;
	cs_trigger_in			: in std_logic_vector(199 downto 0);

	-- For icrm
	tile_src : in std_logic_vector(log2x(dim_x*dim_y)-1 downto 0);

	-- for enemy
	o_enemy : out from_all_enemy_type;
	i_enemy_ddr		: in  to_enemy_type;
	i_enemy_sram	: in  to_enemy_type

	-- router_mon_data : in router_mon_type
	);
end;

architecture rtl of leon3mp is

  component ODDR
	generic (
	  DDR_CLK_EDGE : string;
	  INIT         : bit;
	  SRTYPE       : string);
	port (
	  Q  : out std_ulogic;
	  C  : in  std_ulogic;
	  CE : in  std_ulogic;
	  D1 : in  std_ulogic;
	  D2 : in  std_ulogic;
	  R  : in  std_ulogic;
	  S  : in  std_ulogic);
  end component;

  component BUFG
	port (
	  O : out std_logic;
	  I : in  std_logic);
  end component;

  component ahb2mig_7series
	generic(
	  hindex                  : integer := 0;
	  haddr                   : integer := 0;
	  hmask                   : integer := 16#f00#;
	  pindex                  : integer := 0;
	  paddr                   : integer := 0;
	  pmask                   : integer := 16#fff#;
	  SIM_BYPASS_INIT_CAL     : string  := "OFF";
	  SIMULATION              : string  := "FALSE";
	  USE_MIG_INTERFACE_MODEL : boolean := false
	  );
	port(
	  ddr3_dq         : inout std_logic_vector(63 downto 0);
	  ddr3_dqs_p      : inout std_logic_vector(7 downto 0);
	  ddr3_dqs_n      : inout std_logic_vector(7 downto 0);
	  ddr3_addr       : out   std_logic_vector(14 downto 0);
	  ddr3_ba         : out   std_logic_vector(2 downto 0);
	  ddr3_ras_n      : out   std_logic;
	  ddr3_cas_n      : out   std_logic;
	  ddr3_we_n       : out   std_logic;
	  ddr3_reset_n    : out   std_logic;
	  ddr3_ck_p       : out   std_logic_vector(0 downto 0);
	  ddr3_ck_n       : out   std_logic_vector(0 downto 0);
	  ddr3_cke        : out   std_logic_vector(0 downto 0);
	  ddr3_cs_n       : out   std_logic_vector(0 downto 0);
	  ddr3_dm         : out   std_logic_vector(7 downto 0);
	  ddr3_odt        : out   std_logic_vector(0 downto 0);
	  ahbso           : out   ahb_slv_out_type;
	  ahbsi           : in    ahb_slv_in_type;
	  apbi            : in    apb_slv_in_type;
	  apbo            : out   apb_slv_out_type;
	  calib_done      : out   std_logic;
	  rst_n_syn       : in    std_logic;
	  rst_n_async     : in    std_logic;
	  clk_amba        : in    std_logic;
	  sys_clk_p       : in    std_logic;
	  sys_clk_n       : in    std_logic;
	  clk_ref_i       : in    std_logic;
	  ui_clk          : out   std_logic;
	  ui_clk_sync_rst : out   std_logic;
	  cs_trigger_in   : in std_logic_vector(199 downto 0)
	  );
  end component;

  component ahb2axi_mig_7series
	generic(
	  hindex                  : integer := 0;
	  haddr                   : integer := 0;
	  hmask                   : integer := 16#f00#;
	  pindex                  : integer := 0;
	  paddr                   : integer := 0;
	  pmask                   : integer := 16#fff#
	  );
	port(
	  ddr3_dq         : inout std_logic_vector(63 downto 0);
	  ddr3_dqs_p      : inout std_logic_vector(7 downto 0);
	  ddr3_dqs_n      : inout std_logic_vector(7 downto 0);
	  ddr3_addr       : out   std_logic_vector(14 downto 0);
	  ddr3_ba         : out   std_logic_vector(2 downto 0);
	  ddr3_ras_n      : out   std_logic;
	  ddr3_cas_n      : out   std_logic;
	  ddr3_we_n       : out   std_logic;
	  ddr3_reset_n    : out   std_logic;
	  ddr3_ck_p       : out   std_logic_vector(0 downto 0);
	  ddr3_ck_n       : out   std_logic_vector(0 downto 0);
	  ddr3_cke        : out   std_logic_vector(0 downto 0);
	  ddr3_cs_n       : out   std_logic_vector(0 downto 0);
	  ddr3_dm         : out   std_logic_vector(7 downto 0);
	  ddr3_odt        : out   std_logic_vector(0 downto 0);
	  ahbso           : out   ahb_slv_out_type;
	  ahbsi           : in    ahb_slv_in_type;
	  apbi            : in    apb_slv_in_type;
	  apbo            : out   apb_slv_out_type;
	  calib_done      : out   std_logic;
	  rst_n_syn       : in    std_logic;
	  rst_n_async     : in    std_logic;
	  clk_amba        : in    std_logic;
	  sys_clk_p       : in    std_logic;
	  sys_clk_n       : in    std_logic;
	  clk_ref_i       : in    std_logic;
	  ui_clk          : out   std_logic;
	  ui_clk_sync_rst : out   std_logic
	  );
  end component;


  constant IOAEN      : integer := 0; --cfg.arch.en_ddr;

  constant tile_id : integer := x_loc + (y_loc*dim_x);
  constant ramfile : string  := "./src/baremetal_test.srec";  -- ram contents

  signal apbi  : apb_slv_in_type;
  signal apbo  : apb_slv_out_vector := (others => apb_none);
  signal ahbsi : ahb_slv_in_type;
  signal ahbso : ahb_slv_out_vector := (others => ahbs_none);
  signal ahbmi : ahb_mst_in_type;
  signal ahbmo : ahb_mst_out_vector := (others => ahbm_none);

  signal ahbsi2 : ahb_slv_in_type;
  signal ahbso2 : ahb_slv_out_vector := (others => ahbs_none);
  signal ahbmi2 : ahb_mst_in_type;
  signal ahbmo2 : ahb_mst_out_vector := (others => ahbm_none);

	signal ahbsi_enemy : ahb_slv_in_type;
	signal ahbso_enemy : ahb_slv_out_vector := (others => ahbs_none);
	signal ahbmi_enemy : ahb_mst_in_type;
	signal ahbmo_enemy : ahb_mst_out_vector := (others => ahbm_none);


  -- A second AHB-Bus is used to connect L2-cache and iNA (back) & -- AHB Bride to L2 bus (front)
  signal ahbsi_l2_back, ahbsi_l2_front : ahb_slv_in_type;
  signal ahbso_l2_back, ahbso_l2_front : ahb_slv_out_vector := (others => ahbs_none);
  signal ahbmi_l2_back, ahbmi_l2_front : ahb_mst_in_type;
  signal ahbmo_l2_back, ahbmo_l2_front : ahb_mst_out_vector := (others => ahbm_none);
  signal ahbsi_l2c                     : ahb_slv_in_type;
  signal ahbso_l2c                     : ahb_slv_out_type;

	signal ahb0_err : std_ulogic;
	signal ahb1_err : std_ulogic;
	signal ahb2_err : std_ulogic;
	signal ahb_ddr_err : std_ulogic;

  signal rst_p : std_logic;

  signal rstraw, srclkl : std_ulogic;

  signal cgi           : clkgen_in_type;
  signal cgo           : clkgen_out_type;
  signal u1i, u2i, dui : uart_in_type;
  signal u1o, u2o, duo : uart_out_type;

  signal irqi : irq_in_vector(0 to CFG.arch.NCPU-1);
  signal irqo : irq_out_vector(0 to CFG.arch.NCPU-1);

  signal dbgi : l3_debug_in_vector(0 to CFG.arch.NCPU-1);
  signal dbgo : l3_debug_out_vector(0 to CFG.arch.NCPU-1);

  -- signals for cic and monitor
  signal lmi         : leon_monitor_in_vector(0 to CFG.arch.NCPU-1);
  signal lspdmem_mon : leon_scrpad_mem_monitor_in_vector(0 to CFG.arch.NCPU-1);
  signal busutil_O   : std_logic;
  signal ppbusutil_O : std_logic;
  signal bmi         : bus_monitor_in_type;
  signal cirq        : cic_irq_type;
  signal cicmon      : cic_mon_type;
  signal moncic      : mon_cic_type;
  signal hold        : std_logic_vector(0 to CFG.arch.NCPU-1);

  signal ethi : eth_in_type;
  signal etho : eth_out_type;

  signal dsui : dsu_in_type;
  signal dsuo : dsu_out_type;

  signal gpti : gptimer_in_type;

  signal gpioi : gpio_in_type;
  signal gpioo : gpio_out_type;

  signal tck, tckn, tms, tdi, tdo : std_ulogic;

  signal stati : ahbstat_in_type;

  signal dll0rst, CLKFB, clk0B, gnd, clk_i, clkint, clk_j, clk_k : std_logic;

  -- signals for DDR2 controller
  signal ddr_clk_fb : std_ulogic;
  signal clk_sysl      : std_ulogic;

  signal ddr_cke_i, ddr_csb_i, ddr_odt_i : std_logic_vector(1 downto 0);
  signal ddr_clk_i, ddr_clkb_i           : std_logic_vector(2 downto 0);

  -- SRAM + Memory Controller signals
  signal memi        : memory_in_type;
  signal memo        : memory_out_type;
  signal wpo         : wprot_out_type;
  signal address     : std_logic_vector(23 downto 0);
  signal data        : std_logic_vector(31 downto 0);
  signal romsn       : std_ulogic;
  signal oen         : std_ulogic;
  signal writen      : std_ulogic;
  signal sram_bw_sig : std_logic_vector (0 to 3);

	-- for helper TLM
	signal helper_sram_addr : std_logic_vector(22 downto 0);
	signal helper_sram_data : std_logic_vector(31 downto 0);
	signal helper_sram_cen  : std_logic;
	signal helper_sram_oen  : std_logic;
	signal helper_sram_wen  : std_ulogic;
	signal helper_sram_clk  : std_ulogic;
	signal helper_sram_bw   : std_logic_vector(0 to 3);
	signal helper_sram_data_0  : std_logic_vector(31 downto 0);

  --CES i-Core signals
  signal tlmi : tlm_in_vector;
  signal tlmo : tlm_out_vector;
  signal sig_msti : tlm_mst_in_vector;
  signal sig_msto : tlm_mst_out_vector;
  signal from_icore_top_msto : tlm_mst_out_vector_type (1 downto 0);
  signal from_icore_top_msti : tlm_mst_in_vector_type (1 downto 0);
  signal mmioi : mmio_in_type;
  signal mmioo : mmio_out_type;
  signal vlcwi : vlcw_loader_in;
  signal vlcwo : vlcw_loader_out;

  -- iCRM Signals
  signal en : std_ulogic;
  signal tbd : std_logic_vector(31 downto 0);
  signal write : std_ulogic;
  -------- TLM snoop and Invalidation ------------
  signal wr_en : std_ulogic;
  signal data_in : std_logic_vector(71 downto 0);
  signal rd_en :std_ulogic;
  signal data_out : std_logic_vector(71 downto 0);
  signal empty : std_ulogic;
  signal full : std_ulogic;

----------burst generator traffic signal -----------
  signal enable_crm : std_ulogic;

  --MMI64/proFPGA--


  signal ahb_hrequest : std_ulogic;
  signal ahb_hgrant   : std_ulogic;
  signal ahb_haddr    : std_ulogic_vector(31 downto 0);
  signal ahb_hwrite   : std_ulogic;
  signal ahb_htrans   : std_ulogic_vector(1 downto 0);
  signal ahb_hsize    : std_ulogic_vector(2 downto 0);
  signal ahb_hburst   : std_ulogic_vector(2 downto 0);
  signal ahb_hprot    : std_ulogic_vector(3 downto 0);
  signal ahb_hwdata   : std_ulogic_vector(31 downto 0);
  signal ahb_hrdata   : std_ulogic_vector(31 downto 0);
  signal ahb_hresp    : std_ulogic_vector(1 downto 0);
  signal ahb_hready   : std_ulogic;

  signal   mmi64_ahbmo    : ahb_mst_out_type := ahbm_none;  -- default assignment for unused signals

  signal mmi64_present : std_logic := '1';

  -- DDR signals --
  signal calib_done  : std_logic;
  signal clk100_ddr  : std_logic;

  -- local to global address translation: Sven Rheindt (21.07.2017)
  signal trans_addr    : std_logic_vector(31 downto 0);

	signal sim_sram_k_p    : std_logic;
	signal sim_sram_k_n    : std_logic;
	signal sim_sram_a      : std_logic_vector (TILE_CONFIG(0,0).mem.SRAM_ADDR_W-1 downto 0);
	signal sim_sram_a_tmp  : std_logic_vector (TILE_CONFIG(1,1).mem.SRAM_ADDR_W downto 0);
	signal sim_sram_dq     : std_logic_vector (TILE_CONFIG(0,0).mem.SRAM_DQ_PINS-1 downto 0);
	signal sim_sram_bws_n  : std_logic_vector (TILE_CONFIG(0,0).mem.SRAM_GROUPS-1 downto 0);
	signal sim_sram_rnw    : std_logic;
	signal sim_sram_ld_n   : std_logic;
	signal sim_sram_doff_n : std_logic;
	signal sim_zq0 : std_logic;

	signal rst_sys_p, ext_rst_sys_p, rst_ddr_p : std_ulogic;
	signal rst100_ddr_n, rst100_ddr_n_ff : std_ulogic;
	signal rst_loader_n : std_ulogic;
	signal sim_loader_done : std_ulogic;

	signal i_enemy_sig : from_all_enemy_type;
	signal i_from_iNA_sig : from_iNA_type;
	signal o_from_crc_sig : crc_to_all_enemy_type;

begin

	rst_sys_p <= not rst_sys_n;
	ext_rst_sys_p <= not ext_rst_sys_n;
	rst_ddr_p <= not rst_ddr_n;
	--rst_sys_n <= ext_rst_sys_n when CFG.arch.SIM = 0 else sim_loader_done;

	ddr_rst_proc : process(clk100_ddr)
	begin
		if rising_edge(clk100_ddr) then
			rst100_ddr_n_ff <= rst_sys_n;
			rst100_ddr_n	<= rst100_ddr_n_ff;
		end if;
	end process ddr_rst_proc;

  ddr_calib : if (CFG.arch.EN_DDR = 1) generate
   ddr3_calib_done <= calib_done;
  end generate;

  no_ddr_calib : if (CFG.arch.EN_DDR /= 1) generate
   ddr3_calib_done <= '1';
  end generate;

  -- NA assignments
  namp : if CFG.arch.NA_MP = 1 generate
	ahb_na_mp_si         <= ahbsi;
	ahbso(CFG.arch.NA_HINDEX) <= ahb_na_mp_so;
  end generate;

  nals : if CFG.arch.NA_LS = 1 generate
	ahb_na_ls_si     <= ahbsi_l2_back;
	ahbso_l2_back(CFG.arch.l2c_hindexm) <= ahb_na_ls_so;
  end generate;

  nanols : if CFG.arch.NA_LS = 0 generate
	ahbso_l2_back(CFG.arch.l2c_hindexm) <= ahbs_none;
  end generate;

  -- master index starts from 0
  namst_rx : if CFG.arch.NA_LS = 1 or CFG.arch.NA_MP = 1 generate
	na_mst_rx_in                  <= ahbmi;
	ahbmo(CFG.arch.NA_MST_RX_IDX) <= na_mst_rx_out;
  end generate;

  namst_tx : if CFG.arch.NA_MP = 1 generate
	na_mst_tx_in                  <= ahbmi;
	ahbmo(CFG.arch.NA_MST_TX_IDX) <= na_mst_tx_out;
  end generate;

-----------------------------
--MMI64
------------------------------

  U_AHBM : mmi64_m_ahbm generic map (
	MODULE_ID  => MMI64_ID,
	AHB_DATA_W => 32,
	ENDIANESS  => 1
	) port map (
	  ahb_hclk       => clk_sys,
	  ahb_hreset_n   => rst_sys_n,
	  -- AHB Master port
	  ahb_hrequest_o => ahb_hrequest,
	  ahb_hgrant_i   => ahb_hgrant,
	  ahb_haddr_o    => ahb_haddr,
	  ahb_hwrite_o   => ahb_hwrite,
	  ahb_htrans_o   => ahb_htrans,
	  ahb_hsize_o    => ahb_hsize,
	  ahb_hburst_o   => ahb_hburst,
	  ahb_hprot_o    => ahb_hprot,
	  ahb_hwdata_o   => ahb_hwdata,
	  ahb_hrdata_i   => ahb_hrdata,
	  ahb_hresp_i    => ahb_hresp,
	  ahb_hready_i   => ahb_hready,

	  ---------------------------
	  -- MMI64 interface
	  ---------------------------
	  mmi64_clk           => mmi64_clk,
	  mmi64_reset         => mmi64_reset,
	  mmi64_h_up_d_o      => mmi64_h_up_d_o,
	  mmi64_h_up_valid_o  => mmi64_h_up_valid_o,
	  mmi64_h_up_accept_i => mmi64_h_up_accept_i,
	  mmi64_h_up_start_o  => mmi64_h_up_start_o,
	  mmi64_h_up_stop_o   => mmi64_h_up_stop_o,
	  mmi64_h_dn_d_i      => mmi64_h_dn_d_i,
	  mmi64_h_dn_valid_i  => mmi64_h_dn_valid_i,
	  mmi64_h_dn_accept_o => mmi64_h_dn_accept_o,
	  mmi64_h_dn_start_i  => mmi64_h_dn_start_i,
	  mmi64_h_dn_stop_i   => mmi64_h_dn_stop_i


	  );


  ahb_hgrant <= ahbmi.hgrant(cfg.arch.MMI64_AHBM_IDX);
  ahb_hready <= ahbmi.hready;
  ahb_hresp  <= std_ulogic_vector(ahbmi.hresp);
  ahb_hrdata <= std_ulogic_vector(ahbmi.hrdata);
  -- not mapped: hirq        : std_logic_vector(NAHBIRQ-1 downto 0); -- interrupt result bus
  -- not mapped: testen      : std_ulogic;                           -- scan test enable
  -- not mapped: testrst     : std_ulogic;                           -- scan test reset
  -- not mapped: scanen      : std_ulogic;                           -- scan enable
  -- not mapped: testoen     : std_ulogic;                           -- test output enable
  -- not mapped: testin      : std_logic_vector(NTESTINBITS-1 downto 0);         -- test vector for syncrams

  mmi64_ahbmo.hbusreq   <= ahb_hrequest;
  --mmi64_ahbmo.haddr     <= std_logic_vector(ahb_haddr);
  mmi64_ahbmo.hwrite    <= ahb_hwrite;
  mmi64_ahbmo.htrans    <= std_logic_vector(ahb_htrans);
  mmi64_ahbmo.hsize     <= std_logic_vector(ahb_hsize);
  mmi64_ahbmo.hburst    <= std_logic_vector(ahb_hburst);
  mmi64_ahbmo.hprot     <= std_logic_vector(ahb_hprot);
  mmi64_ahbmo.hwdata    <= std_logic_vector(ahb_hwdata);
  mmi64_ahbmo.hlock     <= '0';
  -- not mapped: hirq        : std_logic_vector(NAHBIRQ-1 downto 0); -- interrupt bus
  -- not mapped: hconfig     : ahb_config_type;                      -- memory access reg.
  -- not mapped: hindex      : integer range 0 to NAHBMST-1;         -- diagnostic use only
  ahbmo(cfg.arch.MMI64_AHBM_IDX) <= mmi64_ahbmo;

	-- local to global address translation: Sven Rheindt (21.07.2017)
	trans_addr <= std_logic_vector(ahb_haddr);
	mmi64_ahbmo.haddr <= (MemMAP.TLM_start(31 downto 24) & trans_addr(23 downto 0)) when (trans_addr(31 downto 28) = MemMAP.RMAP_start(31 downto 28) and trans_addr(27 downto 24) = conv_std_logic_vector(MASTER_XACTOR_ID,4) and trans_addr(23) = '0')
					else trans_addr;
  -----------------------------------------------------------------------------
  -- AHB CONTROLLER
  -----------------------------------------------------------------------------

	--defmaster
	valid_defmst: if CFG.amba.DEFMST < CFG.arch.UNUSED_AHBMINDEX generate
		ahb0_defmst : ahbdefmst
		generic map (hindex => CFG.amba.DEFMST)
		port map(ahbmo => ahbmo(CFG.amba.DEFMST));
	end generate valid_defmst;

  -- main AHB arbiter/multiplexer
  ahb0 : ahbctrl
	generic map (defmast => CFG.amba.DEFMST, split => CFG.amba.SPLIT,
				 rrobin  => CFG.amba.RROBIN, ioaddr => CFG.amba.AHBIO, devid => XILINX_ML507,
				 ioen    => IOAEN, nahbm => CFG.arch.MAXAHBM, nahbs => CFG.arch.MAXAHBS, fpnpen => 1)
	port map (ext_rst_sys_n, clk_sys, ahbmi, ahbmo, ahbsi, ahbso, busutil_O);

	gen_ahb0_mon : if (CFG.arch.en_amba_monitor = 1) generate
	  ahb0_monitor : ahbmon
	  generic map(
		asserterr   => 1, --  integer range 0 to 1 := 1;
		assertwarn  => 0, --  integer range 0 to 1 := 1;
		hmstdisable => 0, --  integer := 0;
		hslvdisable => 0, --  integer := 0;
		arbdisable  => 0, --  integer := 0;
		nahbm       => CFG.arch.MAXAHBM, --  integer range 0 to NAHBMST := NAHBMST;
		nahbs       => CFG.arch.MAXAHBS, --  integer range 0 to NAHBSLV := NAHBSLV;
		ebterm      => 0  --  integer range 0 to 1 := 0
	  )
	  port map(
		rst         => ext_rst_sys_n, --  in std_ulogic;
		clk         => clk_sys, --  in std_ulogic;
		ahbmi       => ahbmi, --  in ahb_mst_in_type;
		ahbmo       => ahbmo, --  in ahb_mst_out_vector;
		ahbsi       => ahbsi, --  in ahb_slv_in_type;
		ahbso       => ahbso, --  in ahb_slv_out_vector;
		err         => ahb0_err,  --  out std_ulogic
		errno		=> bus_error(9 downto 2),
		errid		=> bus_error(16 downto 10)
	  );
	  bus_error(1) <= ahb0_err;
	end generate;

  bmi.bus_util   <= busutil_O;
  bmi.tlm_hready <= ahbso(CFG.arch.SSRAM_HINDEX).hready;
  bmi.l2c_hready <= ahbso(CFG.arch.L2_HINDEX).hready;

not_1x1 : if (CFG_DIM_X > 1 or CFG_DIM_Y > 1) generate
  -- AHB arbiter/multiplexer for l2 - NA connection
  ahb1 : ahbctrl
	generic map (defmast => 0, split => CFG.na.na_split_enable,
				 rrobin  => CFG.amba.RROBIN, ioaddr => MemMAP.AHB_1_IO,
				 ioen    => IOAEN, nahbm => 1, nahbs => 1, fpnpen => 1)
	port map (rst_sys_n, clk_sys, ahbmi_l2_back, ahbmo_l2_back, ahbsi_l2_back, ahbso_l2_back);


	gen_ahb1_mon : if (CFG.arch.en_amba_monitor = 1) generate
	  ahb1_monitor : ahbmon
	  generic map(
		asserterr   => 1, --  integer range 0 to 1 := 1;
		assertwarn  => 0, --  integer range 0 to 1 := 1;
		hmstdisable => 0, --  integer := 0;
		hslvdisable => 0, --  integer := 0;
		arbdisable  => 0, --  integer := 0;
		nahbm       => 1, --  integer range 0 to NAHBMST := NAHBMST;
		nahbs       => 1, --  integer range 0 to NAHBSLV := NAHBSLV;
		ebterm      => 0  --  integer range 0 to 1 := 0
	  )
	  port map(
		rst         => rst_sys_n, --  in std_ulogic;
		clk         => clk_sys, --  in std_ulogic;
		ahbmi       => ahbmi_l2_back, --  in ahb_mst_in_type;
		ahbmo       => ahbmo_l2_back, --  in ahb_mst_out_vector;
		ahbsi       => ahbsi_l2_back, --  in ahb_slv_in_type;
		ahbso       => ahbso_l2_back, --  in ahb_slv_out_vector;
		err         => ahb1_err  --  out std_ulogic
	  );
	end generate;
  -----------------------------------------------------------------------------
  --  AHB2AHB
  -----------------------------------------------------------------------------

  en_ahb2ahb : if (CFG.amba.EN_AHB2AHB = 1) generate

	ahb2 : ahbctrl  -- AHB arbiter/multiplexer before l2 cache
	  generic map (defmast => 0, split => CFG.amba.SPLIT,
		   rrobin  => CFG.amba.RROBIN, ioaddr => CFG.na.ahb_addr_bar4,
				   ioen    => IOAEN, nahbm => 3, nahbs => 3, fpnpen => 1)
	  port map (rst_sys_n, clk_sys, ahbmi_l2_front, ahbmo_l2_front, ahbsi_l2_front, ahbso_l2_front);


	l2c_flush_accel_0 : entity enemy.l2c_flush_accel
		generic map (
			x_loc			=> X_LOC,
			y_loc			=> Y_LOC,
			G_TILEID		=> tile_id,
			hindex_m_0		=> 1,
			hindex_m_1		=> 2,
			hindex_s		=> 1,
			haddr			=> MemMAP.L2C_RANGE_HADDR,
			hmask			=> MemMAP.L2C_RANGE_HMASK
		)
		port map (
			rstn			=> rst_sys_n,
			clk				=> clk_sys,
			ahb_slv_in		=> ahbsi_l2_front,
			ahb_slv_out		=> ahbso_l2_front(1),
			ahb_mst_in_0	=> ahbmi_l2_front,
			ahb_mst_out_0	=> ahbmo_l2_front(1),
			ahb_mst_in_1	=> ahbmi_l2_front,
			ahb_mst_out_1	=> ahbmo_l2_front(2)
		);

	gen_ahb2_mon : if (CFG.arch.en_amba_monitor = 1) generate
	  ahb2_monitor : ahbmon
	  generic map(
		asserterr   => 1, --  integer range 0 to 1 := 1;
		assertwarn  => 1, --  integer range 0 to 1 := 1;
		hmstdisable => 0, --  integer := 0;
		hslvdisable => 0, --  integer := 0;
		arbdisable  => 0, --  integer := 0;
		nahbm       => 2, --  integer range 0 to NAHBMST := NAHBMST;
		nahbs       => 2, --  integer range 0 to NAHBSLV := NAHBSLV;
		ebterm      => 0  --  integer range 0 to 1 := 0
	  )
	  port map(
		rst         => rst_sys_n, --  in std_ulogic;
		clk         => clk_sys, --  in std_ulogic;
		ahbmi       => ahbmi_l2_front, --  in ahb_mst_in_type;
		ahbmo       => ahbmo_l2_front, --  in ahb_mst_out_vector;
		ahbsi       => ahbsi_l2_front, --  in ahb_slv_in_type;
		ahbso       => ahbso_l2_front, --  in ahb_slv_out_vector;
		err         => ahb2_err  --  out std_ulogic
	  );
	end generate;

	ahb2ahb0 : ahb2ahb
	  generic map (
		hsindex   => CFG.arch.L2_HINDEX,     --Slave I/F AHB index
		hmindex   => 0,                 --Master I/F AHB index
		dir       => 0,
		slv       => 1,  --Slave bridge. Used in bi-directional bridge con?guration
		ffact     => 1,  --Frequency scaling factor between AHB clocks on master and slave buses.
		memtech   => cfg.arch.memtech,
		pfen      => 1,                 --Prefetch enable. Enables read FIFO.
		irqsync   => 1,                 --Interrupt forwarding.
		wburst    => 8,  --Length of write bursts in 32-bit words
		iburst    => 8,                 --Instruction fetch burst length.
		rburst    => 8,                --Incremental read burst length.
		bar0      => ahb2ahb_membar(MemMAP.L2C_CTRL_0_HADDR, '0', '0', MemMAP.L2C_CTRL_0_HMASK),  --Address area 0 decoded by the bridge's slave interface (address, prefetchable, cacheable, mask)
		bar1      => ahb2ahb_membar(MemMAP.L2C_CTRL_1_HADDR, '0', '0', MemMAP.L2C_CTRL_1_HMASK),
		bar2      => ahb2ahb_membar(CFG.na.ahb_addr_bar4, '0', '0', MemMAP.L2C_CTRL_2_HMASK),
		bar3      => membar_array(tile_id,0), -- TLM part 1
		bar4      => membar_array(tile_id,1), -- TLM part 2
		bar5      => membar_array(tile_id,2), -- TLM part 3
		bar6      => membar_array(tile_id,3), -- TLM part 4
		bar7      => membar_array(tile_id,4), -- DDR range 1GB
		sbus      => 0,  --The number of the AHB bus to which the slave interface is connected.
		mbus      => 1,  --The number of the AHB bus to which the master interface is connected
		ioarea    => CFG.na.ahb_addr_bar4,  --Address of the I/O area containing the con?guration area for AHB bus connected to the bridge's master interface.
		ibrsten   => 0,                 --Instruction fetch burst enable.
		lckdac    => 2,  --Locked access error detection and correction.  Locked 0-2 0 accesses may lead to deadlock if a locked access is made
						 --while an ongoing read access has received a SPLIT response. The value of lckdac determines how the core
						 --handles this scenario: 0: Core will deadlock, 1: Core will issue an AMBA ERROR response to the locked access,
						 --2: Core will allow both accesses to complete.
		slvmaccsz => 32,  --The maximum size of accesses that will be made to the bridge's slave interface.
		mstmaccsz => 32,  --The maximum size of accesses that will be performed by the bridge's master interface.
		rdcomb    => 0,                 --Read combining.
		wrcomb    => 0,                 --Write combining.
		combmask  => 16#ffff#,          --Read/write combining mask.
		allbrst   => 0,                 --Support all burst types
		ifctrlen  => 0,                 --Interface control enable.
		fcfs      => 0,                 --First-come, First-served operation.
		fcfsmtech => 0,  --Memory technology to use for FCFS buffer.
		scantest  => 0,                 --Enable scan support
		split     => 1,                 --Use AMBA SPLIT responses.
		pipe	  => 128)
	  port map (
		hclkm  => clk_sys,
		hclks  => clk_sys,
		rstn   => rst_sys_n,
		ahbsi  => ahbsi,
		ahbso  => ahbso(CFG.arch.L2_HINDEX),
		ahbmi  => ahbmi_l2_front,
		ahbmo  => ahbmo_l2_front(0),
		ahbso2 => ahbso_l2_front,
		lcki   => (others => '0'),  --Used in systems with multiple AHB/AHB
		lcko   => open,  --Indicates possible deadlock condition
		ifctrl => ahb2ahb_ifctrl_none);  --Enable master interface, Enable slave interface.

	ahb2ahb0_reverse : ahb2ahb
	  generic map (
		hsindex   => 2,     --Slave I/F AHB index
		hmindex   => CFG.arch.l2c_reverse_mst_idx,     --Master I/F AHB index
		dir       => 0,
		slv       => 0,  --Slave bridge. Used in bi-directional bridge con?guration
		ffact     => 1,  --Frequency scaling factor between AHB clocks on master and slave buses.
		memtech   => cfg.arch.memtech,
		pfen      => 1,                 --Prefetch enable. Enables read FIFO.
		irqsync   => 1,                 --Interrupt forwarding.
		wburst    => 8,  --Length of write bursts in 32-bit words
		iburst    => 8,                 --Instruction fetch burst length.
		rburst    => 8,                --Incremental read burst length.
		bar0      => ahb2ahb_membar(MemMap.TLM_haddr, '1', '1', MemMAP.TLM_hmask),
		bar1      => ahb2ahb_membar(MemMap.INA_haddr, '0', '0', MemMAP.INA_hmask),
		--bar2      => ahb2ahb_membar(CFG.amba.AHBIO, '0', '0', 16#FFF#),
		sbus      => 1,  --The number of the AHB bus to which the slave interface is connected.
		mbus      => 0,  --The number of the AHB bus to which the master interface is connected
		ioarea    => CFG.amba.AHBIO,  --Address of the I/O area containing the con?guration area for AHB bus connected to the bridge's master interface.
		ibrsten   => 0,                 --Instruction fetch burst enable.
		lckdac    => 2,  --Locked access error detection and correction.  Locked 0-2 0 accesses may lead to deadlock if a locked access is made
						 --while an ongoing read access has received a SPLIT response. The value of lckdac determines how the core
						 --handles this scenario: 0: Core will deadlock, 1: Core will issue an AMBA ERROR response to the locked access,
						 --2: Core will allow both accesses to complete.
		slvmaccsz => 32,  --The maximum size of accesses that will be made to the bridge's slave interface.
		mstmaccsz => 32,  --The maximum size of accesses that will be performed by the bridge's master interface.
		rdcomb    => 0,                 --Read combining.
		wrcomb    => 0,                 --Write combining.
		combmask  => 16#ffff#,          --Read/write combining mask.
		allbrst   => 0,                 --Support all burst types
		ifctrlen  => 0,                 --Interface control enable.
		fcfs      => 0,                 --First-come, First-served operation.
		fcfsmtech => 0,  --Memory technology to use for FCFS buffer.
		scantest  => 0,                 --Enable scan support
		split     => 1,                 --Use AMBA SPLIT responses.
		pipe      => 0					--Insert pipeline registers.
		)
	  port map (
		hclkm  => clk_sys,
		hclks  => clk_sys,
		rstn   => rst_sys_n,
		ahbsi  => ahbsi_l2_front,
		ahbso  => ahbso_l2_front(2),
		ahbmi  => ahbmi,
		ahbmo  => ahbmo(CFG.arch.l2c_reverse_mst_idx),
		ahbso2 => ahbso,
		lcki   => (others => '0'),  --Used in systems with multiple AHB/AHB
		lcko   => open,  --Indicates possible deadlock condition
		ifctrl => ahb2ahb_ifctrl_none);  --Enable master interface, Enable slave interface.


	ahbsi_l2c         <= ahbsi_l2_front;
	ahbso_l2_front(0) <= ahbso_l2c;

  end generate;

  dis_ahb2ahb : if (CFG.amba.EN_AHB2AHB = 0) generate
	ahbsi_l2c            <= ahbsi;
	ahbso(CFG.arch.L2_HINDEX) <= ahbso_l2c;
  end generate;

  -----------------------------------------------------------------------------
  -- L2 Cache
  -----------------------------------------------------------------------------

  l2c0 : l2c
  generic map(hslvidx  => (0+(1-CFG.amba.EN_AHB2AHB)*CFG.arch.L2_HINDEX), hmstidx => CFG.arch.L2C_HINDEXM, cen => 1, haddr0 => MemMAP.SHM_haddr, haddr1 => MemMAP.RMAP_haddr, hmask0 => MemMAP.SHM_hmask, hmask1 => MemMAP.RMAP_hmask, -- this mask is too big, but the 2nd ahb2ahb bridge takes care of that
		ioaddr   => CFG.na.ahb_addr_bar2, cached => 16#08FF#, hirq => 1, wp => 1, repl => 1, ways => 4,
				linesize => 32, waysize => 128, memtech => cfg.arch.memtech, bbuswidth => 32,
		bioaddr  => CFG.na.ahb_addr_bar3, biomask => CFG.na.ahb_mask_bar3, sbus => 0, mbus => 1, stat => 2,
				arch     => 0, edacen => 0, rmw => 0, ft => 0, mtrr => 16, fttiming => 0)
	port map(rst   => rst_sys_n, clk => clk_sys, ahbsi => ahbsi_l2c, ahbso => ahbso_l2c,
			 ahbmi => ahbmi_l2_back, ahbmo => ahbmo_l2_back(CFG.arch.L2C_HINDEXM), ahbsov => ahbso_l2_back, sto => open);

end generate not_1x1;
  -----------------------------------------------------------------------------
  -- LEON3 processor and DSU
  -----------------------------------------------------------------------------

  cpu : for i in 0 to CFG.arch.NCPU-1 generate
	leongen: if (cfg.arch.icore = 0) or (i /= 0) generate
		u0 : leon3s  -- LEON3 processor
		generic map (i, CFG.arch.FABTECH, CFG.arch.MEMTECH, CFG.leon.NWIN, CFG.leon.DSU, CFG.leon.FPU, CFG.leon.V8,
					0, CFG.leon.MAC, CFG.leon.PCLOW, 0, CFG.leon.NWP, CFG.leon.ICEN, CFG.leon.IREPL, CFG.leon.ISETS, CFG.leon.ILINE,
					CFG.leon.ISETSZ, CFG.leon.ILOCK, CFG.leon.DCEN, CFG.leon.DREPL, CFG.leon.DSETS, CFG.leon.DLINE, CFG.leon.DSETSZ,
					CFG.leon.DLOCK, CFG.leon.DSNOOP, CFG.leon.ILRAMEN, CFG.leon.ILRAMSZ, CFG.leon.ILRAMADDR, CFG.leon.DLRAMEN,
					CFG.leon.DLRAMSZ, CFG.leon.DLRAMADDR, CFG.leon.MMUEN, CFG.leon.ITLBNUM, CFG.leon.DTLBNUM, CFG.leon.TLB_TYPE, CFG.leon.TLB_REP,
					CFG.leon.LDDEL, CFG.leon.DISAS, CFG.leon.ITBSZ, CFG.leon.PWD, CFG.leon.SVT, CFG.leon.RSTADDR, CFG.arch.NCPU-1,
		  CFG.leon.DFIXED, CFG.arch.SCAN, CFG.leon.MMU_PAGE, CFG.leon.BP, CFG.leon.NP_ASI, CFG.leon.WRPSR, tile_id, CFG.leon.EN_ADDR_TRANS, CFG.leon.RCAS_EN)
		port map (clk_sys, rst_sys_n, ahbmi, ahbmo(i), ahbsi, ahbso,
					irqi(i), irqo(i), dbgi(i), dbgo(i), lmi(i), lspdmem_mon(i), hold(i));
	end generate;
	icoregen: if (cfg.arch.icore = 1) and (i = 0) generate
		icore0 : icore_top  -- i-Core processor
		generic map (i, CFG.arch.FABTECH, CFG.arch.MEMTECH, CFG.leon.NWIN, CFG.leon.DSU, CFG.leon.FPU, CFG.leon.V8,
					0, CFG.leon.MAC, CFG.leon.PCLOW, 0, CFG.leon.NWP, CFG.leon.ICEN, CFG.leon.IREPL, CFG.leon.ISETS, CFG.leon.ILINE,
					CFG.leon.ISETSZ, CFG.leon.ILOCK, CFG.leon.DCEN, CFG.leon.DREPL, CFG.leon.DSETS, CFG.leon.DLINE, CFG.leon.DSETSZ,
					CFG.leon.DLOCK, CFG.leon.DSNOOP, CFG.leon.ILRAMEN, CFG.leon.ILRAMSZ, CFG.leon.ILRAMADDR, CFG.leon.DLRAMEN,
					CFG.leon.DLRAMSZ, CFG.leon.DLRAMADDR, CFG.leon.MMUEN, CFG.leon.ITLBNUM, CFG.leon.DTLBNUM, CFG.leon.TLB_TYPE, CFG.leon.TLB_REP,
					CFG.leon.LDDEL, CFG.leon.DISAS, CFG.leon.ITBSZ, CFG.leon.PWD, CFG.leon.SVT, CFG.leon.RSTADDR, CFG.arch.NCPU-1,
					CFG.leon.DFIXED, CFG.arch.SCAN, CFG.leon.MMU_PAGE, CFG.leon.BP,  CFG.leon.NP_ASI, CFG.leon.WRPSR, CFG.icore.PERM, CFG.icore.TLM_SIZE, CFG.icore.TLM_ADDR)
		port map (clk_sys, rst_sys_n, ahbmi, ahbmo(i), ahbsi, ahbso,
					irqi(i), irqo(i), open, dbgi(i), dbgo(i), lmi(i), lspdmem_mon(i), hold(i)
					, mmioi, mmioo, from_icore_top_msti, from_icore_top_msto, vlcwi, vlcwo);
	end generate;
  end generate;

  bus_error(0) <= not dbgo(0).error;

  dsu0 : dsu3                           -- LEON3 Debug Support Unit
	generic map (hindex => CFG.arch.DSU_HINDEX, haddr => MemMAP.DSU_HADDR, hmask => MemMAP.DSU_HMASK,
				 ncpu   => CFG.arch.NCPU, tbits => 30, tech => CFG.arch.memtech, irq => 0, kbytes => CFG.leon.ATBSZ)
	port map (rst_sys_n, clk_sys, ahbmi, ahbsi, ahbso(CFG.arch.DSU_HINDEX), dbgo, dbgi, dsui, dsuo);
  dsui.enable <= '1';
  dsui.break  <= '0';                   --  South Button

  nodsu : if CFG.leon.DSU = 0 generate
	dsuo.tstop <= '0'; dsuo.active <= '0';
  end generate;

  -----------------------------------------------------------------------------
  -- Monitor Module
  -----------------------------------------------------------------------------

  --monitorgen : if MCFG.MONITOR_EN = 1 generate
  --  monitor0 : monitor_top
  --    generic map (
--        NLCPU             => CFG.arch.NCPU,
--        NICORE            => SCFG.NICORE,
--        TILE_INDEX        => MASTER_XACTOR_ID - 1,
--        NCICFIFO          => CFG.arch.NCPU ,
--        L2C_hindex        => CFG.arch.L2_HINDEX,
--        tlm_hindex        => CFG.arch.SSRAM_HINDEX,
--        UMR_CAPIM_ADDRESS => 64 - MASTER_XACTOR_ID,
--        UMR_CAPIM_TYPE    => 32768,
--        UMR_DATA_BITWIDTH => CFG.arch.UMR_DATA_BITWIDTH)
--      port map (
--        rst_I         => rst_sys_n,
--        clk_I         => clk_sys,
--        lm_I          => lmi,
--        bm_I          => bmi,
--        lspdmem_mon_I => lspdmem_mon,
--        ahbs_I        => ahbsi,
--        router_mon_I  => router_mon_data,
--        cm_I          => cicmon,
--        cm_O          => moncic,
--        umr_clk       => open,
--        umr_in_dat    => open,
--        umr_in_en     => open,
--        umr_in_valid  => open,
--        umr_out_dat   => open,
--        umr_out_en    => open,
---        umr_out_valid => open
--        );
--  end generate monitorgen;


  -----------------------------------------------------------------------------
  -- CiC Module
  -----------------------------------------------------------------------------

  cicgen : if CFG_CIC_EN = 1 generate
	cictopinst : cic_top
	  generic map (
		tech   => CFG.arch.FABTECH,
		HINDEX => CFG.arch.CIC_HINDEX,
		HADDR  => CFG_CIC_BASEADDR,
		NCPU   => CFG.arch.NCPU,
		NEQ    => NEQ,
		NDQ    => NDQ,
		NFIFO  => CFG_NCICFIFO,
		FIFODP => FIFODP,
		FIFODW => FIFODW,
		AW     => CICAW,
		DW     => AHBDW
		)
	  port map(
		rsti  => rst_sys_n,
		clki  => clk_sys,
		ahbsi => ahbsi,
		ahbso => ahbso(CFG.arch.CIC_HINDEX),
		cirqo => cirq,
		cmoni => moncic,
		cmono => cicmon,
		hold  => hold
		);
  end generate;

  --i-Core support hardware
 icoresupport: if (cfg.arch.icore = 1) generate
-----------------------------------------------------------------------
---  Bitstream Loader  ------------------------------------------------
-----------------------------------------------------------------------
  bitloadgen: if CFG.icore.BITLOADER_ENABLE = 1 generate
	bitloader : BitstreamLoader
	  generic map(
		hmindex => CFG.arch.ICORE_BITLOAD_HINDEXM,
		hsindex => CFG.arch.ICORE_BITLOAD_HINDEXS,
		hsaddr  => CFG.icore.ICORE_BITLOAD_HADDR,
		hsmask  => CFG.icore.ICORE_BITLOAD_HMASK,
		hirq    => 6
	  )
	  port map(
		clk     => clk_sys,
		rstn    => rst_sys_n,

		ahbmi   => ahbmi,
		ahbmo   => ahbmo(CFG.arch.ICORE_BITLOAD_HINDEXM),

		ahbsi   => ahbsi,
		ahbso   => ahbso(CFG.arch.ICORE_BITLOAD_HINDEXS) --instead of 9
	  );
	end generate;

-----------------------------------------------------------------------
---  VLCW Loader / SI Execution Control -------------------------------
-----------------------------------------------------------------------
  si_exec : SI_Execution
	generic map(
	  hindex1  => CFG.arch.ICORE_VLCWSTO_HINDEXS,
	  haddr1   => CFG.icore.ICORE_VLCWSTO_HADDR,
	  hmask1   => CFG.icore.ICORE_VLCWSTO_HMASK
	)
	port map(
	  clk                 => clk_sys,
	  rstn                => rst_sys_n,

	  phase_begin         => vlcwo.phase_begin,
	  phase_end           => vlcwo.phase_end,

	  sii                 => vlcwi.sii,
	  sio_ctrl            => vlcwo.sio_ctrl,
	  si_ctrl_start_addr  => vlcwi.si_ctrl_start_addr,
	  si_ctrl_length      => vlcwi.si_ctrl_length,

	  vlcw                => vlcwo.vlcw,

	  -- Interface to AHB Bus
	  ahbsi1              => ahbsi,
	  ahbso1              => ahbso(CFG.arch.ICORE_VLCWSTO_HINDEXS), -- 10

	  ahbsi2              => open,
	  ahbso2              => open
	);

	 tlm: if CFG.icore.TLM_ENABLE = 1 generate
-----------------------------------------------------------------------
---  Tilelocal Memory (TLM)  ------------------------------------------
-----------------------------------------------------------------------
	tilelocalmemory0 : tilelocalmemory
	  generic map(
		cfg_shift => 1,
		haddr => CFG.icore.TLM_ADDR,
		kbytes => CFG.icore.TLM_SIZE)
	  port map (clk_sys, tlmi, tlmo);

-----------------------------------------------------------------------
---  Arbiter for TLM  -------------------------------------------------
-----------------------------------------------------------------------
	tlmctrl0 : tlmctrl
	  port map (
		rstn => rst_sys_n,
		clk  => clk_sys,
		msti => sig_msti,
		msto => sig_msto,
		tlmi => tlmi,
		tlmo => tlmo);

-----------------------------------------------------------------------
---  AHB-Interface for TLM  -------------------------------------------
-----------------------------------------------------------------------
	  ahbiface0 : ahbiface
		generic map (hindex => CFG.arch.ICORE_TLM_HINDEXS, haddr => CFG.icore.TLM_ADDR, kbytes => CFG.icore.TLM_SIZE)
		port map (rst_sys_n, clk_sys, ahbsi, ahbso(CFG.arch.ICORE_TLM_HINDEXS),sig_msto(2), sig_msti(2));
  end generate tlm;

  sig_msto(1 downto 0) <= from_icore_top_msto; --top_msto with smco
  from_icore_top_msti <= sig_msti(1 downto 0);  --top_msti with smci

-----------------------------------------------------------------------
---  AHB-Interface for Memory-Mapped-IO  ------------------------------
-----------------------------------------------------------------------
  mm_io0 : mm_io
	generic map (hindex => CFG.arch.ICORE_MI_HINDEXS , haddr => CFG.icore.MMIO_ADDR)
	port map (rst_sys_n, clk_sys, ahbsi, ahbso(CFG.arch.ICORE_MI_HINDEXS), mmioi, mmioo); --mmioi with sii , mmioo with sio

end generate icoresupport;

-----------------------------------------------------------------------
---  DMA --------------------------------------------------------------
-----------------------------------------------------------------------
  dma : if CFG.arch.B1DMA_ENABLE = 1 generate
	dma0 : dma_controller
	  generic map (mst_hindex => CFG.arch.B1DMA_HINDEXM,
				   slv_hindex => CFG.arch.B1DMA_HINDEXS,
				   slv_haddr =>  CFG.icore.B1DMA_ADDR,
				   hirq => 9,
				   tech => CFG.arch.memtech)
	  port map (
		clk     => clk_sys,
		rstn    => rst_sys_n,

		ahbmi   => ahbmi,
		ahbmo   => ahbmo(CFG.arch.B1DMA_HINDEXM),

		ahbsi   => ahbsi,
		ahbso   => ahbso(CFG.arch.B1DMA_HINDEXS)
	 );
  end generate;

  -----------------------------------------------------------------------------
  -- UART Debugger
  -----------------------------------------------------------------------------

  dcomgen : if CFG.arch.AHB_UART = 1 generate
	dcom0 : ahbuart                     -- Debug UART
	  generic map (hindex => CFG.arch.AHB_UART_M_IDX, pindex => CFG.arch.AHBUART_PINDEX, paddr => CFG.uart.AHBUART_PADDR)
	  port map (rst_sys_n, clk_sys, dui, duo, apbi, apbo(CFG.arch.AHBUART_PINDEX), ahbmi, ahbmo(CFG.arch.AHB_UART_M_IDX));
	-- Comments added to debug through UART port
	dui.rxd <= rxd1;
  end generate;
  txd1 <= duo.txd;

  -----------------------------------------------------------------------------
  -- APB Bridge and various periherals
  -----------------------------------------------------------------------------

  apb0 : apbctrl                        -- AHB/APB bridge
	generic map (hindex => CFG.arch.APB_HINDEX, haddr => CFG.amba.APBADDR, nslaves => CFG.arch.MAXAPB)
	port map (rst_sys_n, clk_sys, ahbsi, ahbso(CFG.arch.APB_HINDEX), apbi, apbo);

  -----------------------------------------------------------------------------
  -- apb uart
  -----------------------------------------------------------------------------

  ua1 : if CFG.arch.UART1_ENABLE /= 0 generate
	uart1 : apbuart                     -- UART 1
	  generic map (pindex   => CFG.arch.APBUART_PINDEX, paddr => CFG.uart.APBUART_PADDR, pirq => CFG.uart.APBUART_PIRQ, console => cfg.uart.duart,
				   fifosize => CFG.uart.UART1_FIFO)
	  port map (rst_sys_n, clk_sys, apbi, apbo(CFG.arch.APBUART_PINDEX), u1i, u1o);
	u1i.extclk <= '0'; u1i.ctsn <= '0';
  end generate;

  -----------------------------------------------------------------------------
  -- irq
  -----------------------------------------------------------------------------

  irqctrl : if CFG.arch.IRQ3_ENABLE /= 0 generate
	irqctrl0 : irqmp                    -- interrupt controller
	  generic map (pindex => CFG.arch.IRQMP_PINDEX, paddr => CFG.irq.IRQMP_PADDR, ncpu => CFG.arch.NCPU, CFG_CIC_IRQ => CFG_CIC_IRQ, CFG_NA_IRQ => CFG.na.NA_IRQ)
	  port map (rst_sys_n, clk_sys, apbi, apbo(CFG.arch.IRQMP_PINDEX), irqo, irqi, cirq.irq, na_irq.irq);
  end generate;
  irq3 : if CFG.arch.IRQ3_ENABLE = 0 generate
	x : for i in 0 to CFG.arch.NCPU-1 generate
	  irqi(i).irl <= "0000";
	end generate;
  end generate;

  -----------------------------------------------------------------------------
  -- gpt
  -----------------------------------------------------------------------------

  gpt : if CFG.arch.GPT_ENABLE /= 0 generate
	timer0 : gptimer                    -- timer unit
	  generic map (pindex => CFG.arch.GPTIMER_PINDEX, paddr => CFG.timer.GPTIMER_PADDR, pirq => CFG.timer.GPT_IRQ,
				   sepirq => CFG.timer.GPT_SEPIRQ, sbits => CFG.timer.GPT_SW, ntimers => CFG.timer.GPT_NTIM,
				   nbits  => CFG.timer.GPT_TW)
	  port map (rst_sys_n, clk_sys, apbi, apbo(CFG.arch.GPTIMER_PINDEX), gpti, open);
	gpti.dhalt <= dsuo.tstop; gpti.extclk <= '0';
  end generate;

  -----------------------------------------------------------------------------
  -- ahbstat
  -----------------------------------------------------------------------------

  ahbs : if CFG.arch.AHBSTAT = 1 generate    -- AHB status register
	ahbstat0 : ahbstat generic map (pindex => CFG.arch.AHBSTAT_PINDEX, paddr => CFG.stat.AHBSTAT_PADDR, pirq => CFG.stat.AHBSTAT_PIRQ,
									nftslv => CFG.stat.AHBSTATN)
	  port map (rst_sys_n, clk_sys, ahbmi, ahbsi, stati, apbi, apbo(CFG.arch.AHBSTAT_PINDEX));
  end generate;

  -----------------------------------------------------------------------------
  -- DDR2 controller
  -----------------------------------------------------------------------------

  noddr : if (cfg.arch.en_ddr = 0) generate lock <= '1'; end generate;


-----------------------------------------------------------------------
--- Coherency Region Manager
-----------------------------------------------------------------------

 crm : if CFG.crm.CRM_EN = 1 generate
	crm_top : entity icrm.crm_top
		generic map(
			x_loc => X_LOC,
			y_loc => Y_LOC,
			hindex_im => CFG.arch.CRM_MST_IDX,
			my_tile_id => tile_id,
			hindex_snoop => CFG.arch.CRM_HINDEX,
			ioaddr_snoop => CFG.crm.CRM_AHBS_IOADDR,
			iomask_snoop => CFG.crm.CRM_AHBS_IOMASK,
			tech_snoop => CFG.arch.MEMTECH,
			na_mst_rx_idx => CFG.arch.NA_MST_RX_IDX
			)

		port map(
			rstn => rst_sys_n,
			clkm => clk_sys,
			ahbsi => ahbsi,
			ahbso => ahbso(CFG.arch.CRM_HINDEX),
			ahbmi => ahbmi,
			ahbmo => ahbmo(CFG.arch.CRM_MST_IDX),
			tile_src => tile_src,
			crm_en => enable_crm
			);
  end generate;

-----------------------------------------------------------------------
--- CRM traffic generator
-----------------------------------------------------------------------
--CRM_TEST: if CFG_BURST_GEN_EN = 1 generate

--        CRM_TEST_1 : entity icrm.burst_gen_top generic map(hindex  => CFG_BURST_GEN_IDX,  my_tile_id => tile_id, traffic_addr  => CFG_TRAFFIC_ADDR, traffic_mask => CFG_TRAFFIC_MASK )

--  port map(clkm, rstn, ahbmo(CFG_BURST_GEN_IDX), ahbmi, enable_crm);

--  end generate;

  -----------------------------------------------------------------------------
  -- ENEMY
  -----------------------------------------------------------------------------

	en_enemy_tlm : if NMA_CFG.enemy.tlm_en = '1' or NMA_CFG.enemy.debug = '1' or NMA_CFG.enemy.perf = '1' generate
	enemy_sram : entity enemy.enemy_unit
		generic map (
			G_AT_DDR	=> '0',
			G_TILEID	=> tile_id,
			hindex_m	=> CFG.arch.enemy_tlm_mst_idx,
			hindex_s	=> CFG.arch.enemy_tlm_hindex,
			haddr_s		=> MemMAP.ENEMY_TLM_haddr,
			hmask_s		=> MemMAP.ENEMY_hmask
		)
		port map (
			rstn		=> rst_sys_n,
			clk			=> clk_sys,
			o_to_iNA	=> i_enemy_sig.sram_enemy,
			i_from_iNA	=> i_enemy_sram,
			i_from_crc	=> o_from_crc_sig,
			ahb_mst_in	=> ahbmi,
			ahb_mst_out => ahbmo(CFG.arch.enemy_tlm_mst_idx),
			ahb_slv_in	=> ahbsi,
			ahb_slv_out	=> ahbso(CFG.arch.enemy_tlm_hindex)
		);
	end generate en_enemy_tlm;

	dis_enemy_tlm : if NMA_CFG.enemy.tlm_en = '0' and NMA_CFG.enemy.debug = '0' and NMA_CFG.enemy.perf = '0' generate
		i_enemy_sig.sram_enemy	<= from_enemy_type_none;
	end generate dis_enemy_tlm;

	o_enemy.sram_enemy	<= i_enemy_sig.sram_enemy;
	o_enemy.ddr_enemy	<= i_enemy_sig.ddr_enemy;
	o_enemy.from_crc	<= o_from_crc_sig;

	enemy_crc : entity enemy.completion_crc
		generic map (
			G_IS_DDR_TILE => CFG.arch.EN_DDR
		)
		port map (
			rstn		=> rst_sys_n,
			clk			=> clk_sys,
			rstn_ddr	=> rst100_ddr_n,
			clk_ddr		=> clk100_ddr,
			i_from_iNA  => i_enemy_sram,
			i_from_enemy=> i_enemy_sig,
			o_from_crc	=> o_from_crc_sig
		);

  -----------------------------------------------------------------------------
  -- SSRAM
  -----------------------------------------------------------------------------

  profpga_ssram : if CFG.arch.PROFPGA_SSRAM = 1 and CFG.mem.bram_tlm = 0 and CFG.arch.SIM = 0 generate

	new_sram : if CFG.mem.NEW_SSRAM = 1 generate
	sram_ctrl : entity ssram_ctrl.ahb_sram_controller_top
    generic map(
      HINDEX               => CFG.arch.SSRAM_HINDEX,
	  HADDR                => CFG.mem.tlm_start_addr,
	  HMASK                => CFG.mem.tlm_mask,
	  ADDR_WIDTH           => CFG.mem.SRAM_ADDR_W-1
    )
    port map(
	  rst         => rst_ddr_p,
      clk         => clk_sys,
      clk200      => clk200,
      clk200_rst  => mmi64_reset,

	  sram_ready   => sram_ready,

      -- amba interface
      ahbsi         => ahbsi,
      ahbso         => ahbso(CFG.arch.SSRAM_HINDEX),

      -- memory interface
      sram_k_p     => sram_k_p,
      sram_k_n     => sram_k_n,
      sram_a       => sram_a,
      sram_dq      => sram_dq,
      sram_bws_n   => sram_bws_n,
      sram_rnw     => sram_rnw,
      sram_ld_n    => sram_ld_n,
      sram_doff_n  => sram_doff_n
    );
	end generate new_sram;

	old_sram : if CFG.mem.NEW_SSRAM = 0 generate
	sram_ctrl : entity ssram_ctrl.single_ssram_ctrl
		generic map(
			hindex  => CFG.arch.SSRAM_HINDEX,
			haddr   => CFG.mem.tlm_start_addr,
			ADDR_W  => CFG.mem.SRAM_ADDR_W,      -- width of the address bus
			DQ_PINS => CFG.mem.SRAM_DQ_PINS,      -- number of DQ pins
			GROUPS  => CFG.mem.SRAM_GROUPS        -- number of byte write enable pins
		)
		port map(
			rstn         => rst_ddr_n,
			clk          => clk_sys,
			clk200       => clk200,
			clk200_rst   => mmi64_reset,

			ready		 => sram_ready,

			ahbsi         => ahbsi,
			ahbso         => ahbso(CFG.arch.SSRAM_HINDEX),

			sram_k_p     => sram_k_p,
			sram_k_n     => sram_k_n,
			sram_a       => sram_a,
			sram_dq      => sram_dq,
			sram_bws_n   => sram_bws_n,
			sram_rnw     => sram_rnw,
			sram_ld_n    => sram_ld_n,
			sram_doff_n  => sram_doff_n
		);
	end generate old_sram;
  end generate profpga_ssram;

	profpga_bram_tlm : if CFG.mem.bram_tlm = 1 generate
		ahbram0 : ahbram
			generic map (
				hindex	=> CFG.arch.SSRAM_HINDEX,
				haddr	=> CFG.mem.tlm_start_addr,
				tech	=> CFG.arch.MEMTECH,
				hmask	=> CFG.mem.tlm_mask,
				kbytes	=> CFG.mem.RAM_SIZE,
				pipe	=> 1
				)
			port map (
				rst		=> ext_rst_sys_n,
				clk		=> clk_sys,
				ahbsi	=> ahbsi,
				ahbso	=> ahbso(CFG.arch.SSRAM_HINDEX)
				);
	end generate profpga_bram_tlm;

  -----------------------------------------------------------------------------
  -- RAM FOR SIM
  -----------------------------------------------------------------------------

	unused_sim_ahh : if CFG.arch.SIM = 0 generate
		ahbmo(CFG.arch.loader_mst_idx) <= ahbm_none;
		ahbso(CFG.arch.SIM_HINDEX) <= ahbs_none;
	end generate;

-- pragma translate_off
	sim_sram : if CFG.arch.SIM = 1 generate

		ldma_helper_TLM : entity ina.ldma_loader
		generic map(
			hindex			=> CFG.arch.loader_mst_idx,
			TEXT_SRC_ADDR		=> unsigned(MemMAP.HELPER_TLM_start),
			TEXT_DST_ADDR		=> unsigned(MemMAP.TLM_start),
			TEXT_LENGTH			=> 2**14, -- 64kByte
			DATA_SRC_ADDR		=> unsigned(MemMAP.HELPER_TLM_start) + x"20000",
			DATA_DST_ADDR		=> unsigned(MemMAP.TLM_start) + x"20000",
			DATA_LENGTH			=> 2**8, -- 4kByte
			READ_LATENCY		=> 0
		)
		port map (
			sys_clk_sim		=> clk_sys,
			loader_rstn_sim	=> rst_loader_n,
			AHBIn			=> ahbmi,
			AHBOut			=> ahbmo(CFG.arch.loader_mst_idx),
			done			=> sim_loader_done
		);
		loader_rst : if CFG.mem.bram_tlm = 1 generate
			rst_loader_n <= ext_rst_sys_n;
		end generate loader_rst;

		new_sram : if CFG.mem.NEW_SSRAM = 1 and CFG.mem.bram_tlm = 0 generate
		sram_ctrl : entity ssram_ctrl.ahb_sram_controller_top
		generic map(
			HINDEX               => CFG.arch.SSRAM_HINDEX,
			HADDR                => CFG.mem.tlm_start_addr,
			HMASK                => CFG.mem.tlm_mask,
			ADDR_WIDTH           => CFG.mem.SRAM_ADDR_W-1
		)
		port map(
			rst         => ext_rst_sys_p,
			clk         => clk_sys,
			clk200      => clk200,
			clk200_rst  => ext_rst_sys_n,

			sram_ready   => rst_loader_n,

			-- amba interface
			ahbsi         => ahbsi,
			ahbso         => ahbso(CFG.arch.SSRAM_HINDEX),

			-- memory interface
			sram_k_p     => sim_sram_k_p,
			sram_k_n     => sim_sram_k_n,
			sram_a       => sim_sram_a,
			sram_dq      => sim_sram_dq,
			sram_bws_n   => sim_sram_bws_n,
			sram_rnw     => sim_sram_rnw,
			sram_ld_n    => sim_sram_ld_n,
			sram_doff_n  => sim_sram_doff_n
		);
		end generate new_sram;

		old_sram : if CFG.mem.NEW_SSRAM = 0 and CFG.mem.bram_tlm = 0 generate
		sram_ctrl : entity ssram_ctrl.single_ssram_ctrl
		generic map(
			hindex  => CFG.arch.SSRAM_HINDEX,
			haddr   => CFG.mem.tlm_start_addr,
			ADDR_W  => CFG.mem.SRAM_ADDR_W,      -- width of the address bus
			DQ_PINS => CFG.mem.SRAM_DQ_PINS,      -- number of DQ pins
			GROUPS  => CFG.mem.SRAM_GROUPS        -- number of byte write enable pins
		)
		port map(
			rstn         => ext_rst_sys_n,
			clk          => clk_sys,
			clk200       => clk200,
			clk200_rst   => ext_rst_sys_n,

			ahbsi         => ahbsi,
			ahbso         => ahbso(CFG.arch.SSRAM_HINDEX),

			ready		=> rst_loader_n,

			sram_k_p     => sim_sram_k_p,
			sram_k_n     => sim_sram_k_n,
			sram_a       => sim_sram_a,
			sram_dq      => sim_sram_dq,
			sram_bws_n   => sim_sram_bws_n,
			sram_rnw     => sim_sram_rnw,
			sram_ld_n    => sim_sram_ld_n,
			sram_doff_n  => sim_sram_doff_n
		);
		end generate old_sram;

		sim_sram_a_tmp <= '0' & sim_sram_a;

		sram_mem_0: ddr2_burst2
		port map(
			TCK    => '0',
			TMS    => '0',
			TDI    => '0',
			TDO    => open,
			K      => sim_sram_k_p,
			K_bar  => sim_sram_k_n,
			C      => '1',
			C_bar  => '1',
			LD_bar => sim_sram_ld_n,
			WE_bar => sim_sram_rnw,
			BW     => sim_sram_bws_n,
			A      => sim_sram_a_tmp,
			CQ     => open,
			CQ_bar => open,
			DQ     => sim_sram_dq,
			ZQ     => sim_zq0,
			DOFF   => sim_sram_doff_n
		);

		-- helper TLM connected to SREC File

		memi.writen <= '1'; memi.wrn <= "1111";
		memi.brdyn  <= '1'; memi.bexcn <= '1';
		memi.bwidth <= "10";

		mctrl0 : mctrl generic map (hindex  => CFG.arch.SIM_HINDEX, pindex => CFG.arch.mctrl_pindex,
									ramaddr => MemMAP.HELPER_TLM_HADDR, rammask => MemMAP.HELPER_TLM_HMASK,
									romaddr => MemMAP.HELPER_TLM_ROMADDR, rommask => MemMAP.HELPER_TLM_ROMMASK,
									ioaddr  => MemMAP.HELPER_TLM_IOADDR, iomask => MemMAP.HELPER_TLM_IOMASK,
									srbanks => 1)
		  port map (ext_rst_sys_n, clk_sys, memi, memo, ahbsi, ahbso(CFG.arch.SIM_HINDEX), apbi, apbo(CFG.arch.mctrl_pindex), wpo, open);

		ssram_data_pads : iopadvv generic map (tech => CFG.arch.padtech, voltage => 2, level => 1, width => 32)
		  port map (helper_sram_data(31 downto 0), memo.data(31 downto 0), memo.vbdrive(31 downto 0), memi.data(31 downto 0));

		ODDR_inst : ODDR
		  generic map(
			DDR_CLK_EDGE => "OPPOSITE_EDGE",  -- "OPPOSITE_EDGE" or "SAME_EDGE"
			INIT         => '0',  -- Initial value for Q port (?1? or ?0?)
			SRTYPE       => "SYNC")         -- Reset Type ("ASYNC" or "SYNC")
		  port map (
			Q  => open,                 -- 1-bit DDR output
			C  => clk_sys,                     -- 1-bit clock input
			CE => '1',                      -- 1-bit clock enable input
			D1 => '0',                      -- 1-bit data input (positive edge)
			D2 => '1',                      -- 1-bit data input (negative edge)
			R  => rst_p,                    -- 1-bit reset input
			S  => '0'                       -- 1-bit set input
			);

		helper_sram_addr <= "00" & memo.address(22 downto 2);
		helper_sram_cen  <= memo.ramsn(0);
		helper_sram_oen  <= memo.oen;
		helper_sram_wen  <= memo.writen;
		helper_sram_bw   <= memo.wrn(3 downto 0);

		rst_p <= not ext_rst_sys_n;


		-- SREC file switches the first and last 16 bit
		-- Tile 0
		mctrl0_data : PROCESS (helper_sram_wen, helper_sram_data, helper_sram_data_0)
		BEGIN
		if (helper_sram_wen = '0') then
		  helper_sram_data_0(31 downto 16) <= helper_sram_data(15 downto 0);
		  helper_sram_data_0(15 downto 0)  <= helper_sram_data(31 downto 16);
		  helper_sram_data <= (others => 'Z');
		elsif (helper_sram_wen = '1') then
		   helper_sram_data(31 downto 16) <= helper_sram_data_0(15 downto 0);
		   helper_sram_data(15 downto 0)  <= helper_sram_data_0(31 downto 16);
		   helper_sram_data_0  <= (others => 'Z');
		end if;
		END process;


		-- connection to SREC File
		sram_0_1 : for i in 0 to 1 generate
			sr0 : sram generic map (index => i, abits => 23, fname => ramfile)
			port map (helper_sram_addr, helper_sram_data_0(15-i*8 downto 8-i*8), helper_sram_cen,
				helper_sram_bw(i+2), helper_sram_oen);
		end generate;

		sram_2_3 : for i in 2 to 3 generate
			sr0 : sram generic map (index => i, abits => 23, fname => ramfile)
			port map (helper_sram_addr, helper_sram_data_0(47-i*8 downto 40-i*8), helper_sram_cen,
				helper_sram_bw(i-2), helper_sram_oen);
		end generate;

	end generate sim_sram;
-- pragma translate_on

  -----------------------------------------------------------------------
  ---  TCPA  ------------------------------------------------------------
  -----------------------------------------------------------------------
  tcpa_gen : if (CFG.arch.TCPA = 1) generate
	  tcpa : TCPA_TOP
		generic map(
		  NUM_OF_BUFFER_STRUCTURES              => CFG.tcpa.NUM_OF_BUFFER_STRUCTURES,
		  BUFFER_SIZE                           => CFG.tcpa.BUFFER_SIZE,
		  BUFFER_SIZE_ADDR_WIDTH                => CFG.tcpa.BUFFER_SIZE_ADDR_WIDTH,
		  BUFFER_CHANNEL_SIZE                   => CFG.tcpa.BUFFER_CHANNEL_SIZE,
		  BUFFER_CHANNEL_ADDR_WIDTH             => CFG.tcpa.BUFFER_CHANNEL_ADDR_WIDTH,
		  BUFFER_CHANNEL_SIZES_ARE_POWER_OF_TWO => CFG.tcpa.BUFFER_CHANNEL_SIZES_ARE_POWER_OF_TWO,
		  EN_ELASTIC_BUFFER                     => CFG.tcpa.EN_ELASTIC_BUFFER,
		  AG_BUFFER_CONFIG_SIZE                 => CFG.tcpa.AG_BUFFER_CONFIG_SIZE,

		  AG_BUFFER_NORTH => CFG.tcpa.AG_BUFFER_NORTH,
		  AG_BUFFER_SOUTH => CFG.tcpa.AG_BUFFER_SOUTH,
		  AG_BUFFER_EAST  => CFG.tcpa.AG_BUFFER_EAST,
		  AG_BUFFER_WEST  => CFG.tcpa.AG_BUFFER_WEST,

		  RBUFFER_HIRQ_AHB_INDEX => CFG.arch.TCPA_RBUFFER_HINDEX,
		  RBUFFER_HIRQ_AHB_ADDR  => CFG.tcpa.RBUFFER_HIRQ_AHB_ADDR,
		  RBUFFER_HIRQ_AHB_MASK  => CFG.tcpa.RBUFFER_HIRQ_AHB_MASK,
		  RBUFFER_HIRQ_AHB_IRQ   => CFG.tcpa.RBUFFER_HIRQ_AHB_IRQ,

		  INDEX_VECTOR_DIMENSION  => CFG.tcpa.INDEX_VECTOR_DIMENSION,
		  INDEX_VECTOR_DATA_WIDTH => CFG.tcpa.INDEX_VECTOR_DATA_WIDTH,
		  MATRIX_PIPELINE_DEPTH   => CFG.tcpa.MATRIX_PIPELINE_DEPTH,

		  --#######################################################################
		  GC_pindex => CFG.arch.GC_PINDEX,
		  GC_paddr  => CFG.tcpa.GC_PADDR,
		  GC_pmask  => CFG.tcpa.GC_PMASK,
		  GC_pirq   => CFG.tcpa.GC_PIRQ,

		  CM_pindex => CFG.arch.CM_PINDEX,
		  CM_paddr  => CFG.tcpa.CM_PADDR,
		  CM_pmask  => CFG.tcpa.CM_PMASK,

		  RR_pindex => CFG.arch.RR_PINDEX,
		  RR_paddr  => CFG.tcpa.RR_PADDR,
		  RR_pmask  => CFG.tcpa.RR_PMASK,

		  FI_pindex => CFG.arch.FI_pindex,
		  FI_pirq   => CFG.tcpa.FI_pirq,
		  FI_paddr  => CFG.tcpa.FI_paddr,
		  FI_pmask  => CFG.tcpa.FI_pmask,

		  --#######################################################################
		  ITERATION_VARIABLE_WIDTH        => CFG.tcpa.ITERATION_VARIABLE_WIDTH,
		  DIMENSION                       => CFG.tcpa.DIMENSION,
		  SELECT_WIDTH                    => CFG.tcpa.SELECT_WIDTH,
		  NO_REG_TO_PROGRAM               => CFG.tcpa.NO_REG_TO_PROGRAM,
		  MATRIX_ELEMENT_WIDTH            => CFG.tcpa.MATRIX_ELEMENT_WIDTH,
		  DATA_WIDTH                      => CFG.tcpa.DATA_WIDTH,
		  MAX_NO_OF_PROGRAM_BLOCKS        => CFG.tcpa.MAX_NO_OF_PROGRAM_BLOCKS,
		  NUM_OF_IC_SIGNALS               => CFG.tcpa.NUM_OF_IC_SIGNALS
		)
		port map(
		  dclk_in     => clk_sys,
		  TCPA_clk    => clk_sys,
		  ahb_clk_in  => clk_sys,
		  ahb_rstn_in => rst_sys_n,

		  ahbsi_in         => ahbsi,
		  ahbso_out_NORTH  => ahbso(CFG.arch.tcpa_buffer_north_hindex),
		  ahbso_out_SOUTH  => ahbso(CFG.arch.tcpa_buffer_south_hindex),
		  ahbso_out_EAST   => ahbso(CFG.arch.tcpa_buffer_east_hindex),
		  ahbso_out_WEST   => ahbso(CFG.arch.tcpa_buffer_west_hindex),
		  RBuffer_hirq_out => ahbso(CFG.arch.tcpa_rbuffer_hindex),

		  apbi_in            => apbi,
		  reconfig_regs_apbo => apbo(CFG.arch.RR_pindex),
		  CM_apbo            => apbo(CFG.arch.CM_pindex),
		  GC_apbo_out        => apbo(CFG.arch.GC_pindex),
		  FI_apbo            => apbo(CFG.arch.FI_pindex)
		);


  end generate;

  -----------------------------------------------------------------------------
  -- DDR
  -----------------------------------------------------------------------------

  --AHB bridge for DDR--
  en_ahb2ahb_ddr : if (CFG.arch.EN_DDR = 1) generate

  -----------------------------------------------------------------------------
  -- ENEMY at DDR
  -----------------------------------------------------------------------------
	enemy_ddr : entity enemy.enemy_unit
		generic map (
			G_AT_DDR => '1',
			G_TILEID => tile_id,
			hindex_m => 0,
			hindex_s => 1,
			haddr_s	 => MemMAP.ENEMY_DDR_haddr,
			hmask_s	 => MemMAP.ENEMY_hmask
		)
		port map (
			rstn		=> rst100_ddr_n,
			clk			=> clk100_ddr,
			o_to_iNA	=> i_enemy_sig.ddr_enemy,
			i_from_iNA	=> i_enemy_ddr,
			i_from_crc	=> o_from_crc_sig,
			ahb_mst_in	=> ahbmi_enemy,
			ahb_mst_out => ahbmo_enemy(0),
			ahb_slv_in	=> ahbsi2,
			ahb_slv_out	=> ahbso2(1)
			);

	en_enemy_meta_cache : if (NMA_CFG.enemy.meta_cache = '1') generate
		enemy_meta_cache : l2c
		generic map(hslvidx  => 1, hmstidx => 2, cen => 1, haddr0 => MemMAP.TLM_haddr, hmask0 => 16#FF0#, -- this mask is too big, but the 2nd ahb2ahb bridge takes care of that
			ioaddr   => 16#820#, cached => 16#0100#, hirq => 1, wp => 1, repl => 1, ways => 1,
					linesize => 32, waysize => 1, memtech => cfg.arch.memtech, bbuswidth => 32,
			bioaddr  => 16#824#, biomask => 16#FFF#, sbus => 0, mbus => 1, stat => 2,
					arch     => 0, edacen => 0, rmw => 0, ft => 0, mtrr => 16, fttiming => 0)
		port map(rst   => rst100_ddr_n, clk => clk100_ddr, ahbsi => ahbsi_enemy, ahbso => ahbso_enemy(1),
				 ahbmi => ahbmi2, ahbmo => ahbmo2(2), ahbsov => ahbso2, sto => open);

		ahb_ctrl_ddr : ahbctrl  -- AHB arbiter/multiplexer for DDR connection
		  generic map (defmast => 0, split => CFG.amba.SPLIT,
					   rrobin  => CFG.amba.RROBIN, ioaddr => CFG.ddr.ddr_bus_io,
					   ioen    => IOAEN, nahbm => 3, nahbs => 3, fpnpen => 1)
		  port map (rst100_ddr_n, clk100_ddr, ahbmi2, ahbmo2, ahbsi2, ahbso2);
	end generate en_enemy_meta_cache;

	dis_enemy_meta_cache : if (NMA_CFG.enemy.meta_cache = '0') generate
	ahb_ctrl_ddr : ahbctrl  -- AHB arbiter/multiplexer for DDR connection
	  generic map (defmast => 0, split => CFG.amba.SPLIT,
				   rrobin  => CFG.amba.RROBIN, ioaddr => CFG.ddr.ddr_bus_io,
				   ioen    => IOAEN, nahbm => 2, nahbs => 3, fpnpen => 1)
	  port map (rst100_ddr_n, clk100_ddr, ahbmi2, ahbmo2, ahbsi2, ahbso2);
	end generate dis_enemy_meta_cache;

	en_enemy_cache : if (NMA_CFG.enemy.cache = '1') generate
		enemy_cache : l2c
		generic map(hslvidx  => 0, hmstidx => 0, cen => 1, haddr0 => MemMAP.SHM_haddr, hmask0 => MemMAP.SHM_hmask, -- this mask is too big, but the 2nd ahb2ahb bridge takes care of that
			ioaddr   => CFG.na.ahb_addr_bar2, cached => 16#00FF#, hirq => 1, wp => 1, repl => 1, ways => 2,
					linesize => 32, waysize => 16, memtech => cfg.arch.memtech, bbuswidth => 32,
			bioaddr  => CFG.na.ahb_addr_bar3, biomask => CFG.na.ahb_mask_bar3, sbus => 0, mbus => 1, stat => 2,
					arch     => 0, edacen => 0, rmw => 0, ft => 0, mtrr => 16, fttiming => 0)
		port map(rst   => rst100_ddr_n, clk => clk100_ddr, ahbsi => ahbsi_enemy, ahbso => ahbso_enemy(0),
				 ahbmi => ahbmi2, ahbmo => ahbmo2(0), ahbsov => ahbso2, sto => open);

		ahb_enemy : ahbctrl  -- AHB arbiter/multiplexer for DDR connection
		  generic map (defmast => 0, split => CFG.amba.SPLIT,
					   rrobin  => CFG.amba.RROBIN, ioaddr => 16#FFC#,
					   ioen    => IOAEN, nahbm => 1, nahbs => 2, fpnpen => 1)
		  port map (rst100_ddr_n, clk100_ddr, ahbmi_enemy, ahbmo_enemy, ahbsi_enemy, ahbso_enemy);
	end generate en_enemy_cache;

	dis_enemy_cache : if (NMA_CFG.enemy.cache = '0') generate
		ahbmo2(0)	<= ahbmo_enemy(0);
		ahbmi_enemy <= ahbmi2;
	end generate dis_enemy_cache;


	gen_ahb_ddr_mon : if (CFG.arch.en_amba_monitor = 1) generate
	  ahb_ddr_monitor : ahbmon
	  generic map(
		asserterr   => 1, --  integer range 0 to 1 := 1;
		assertwarn  => 1, --  integer range 0 to 1 := 1;
		hmstdisable => 0, --  integer := 0;
		hslvdisable => 0, --  integer := 0;
		arbdisable  => 0, --  integer := 0;
		nahbm       => 2, --  integer range 0 to NAHBMST := NAHBMST;
		nahbs       => 4, --  integer range 0 to NAHBSLV := NAHBSLV;
		ebterm      => 0  --  integer range 0 to 1 := 0
	  )
	  port map(
		rst         => rst100_ddr_n, --  in std_ulogic;
		clk         => clk100_ddr, --  in std_ulogic;
		ahbmi       => ahbmi2, --  in ahb_mst_in_type;
		ahbmo       => ahbmo2, --  in ahb_mst_out_vector;
		ahbsi       => ahbsi2, --  in ahb_slv_in_type;
		ahbso       => ahbso2, --  in ahb_slv_out_vector;
		err         => ahb_ddr_err  --  out std_ulogic
	  );
	end generate;

	ahb2ahb0 : ahb2ahb
	  generic map (
		hsindex   => CFG.arch.DDR_HINDEX,     --Slave I/F AHB index
		hmindex   => 1,     --Master I/F AHB index
		dir       => 1,
		slv       => 1,  --Slave bridge. Used in bi-directional bridge con?guration
		ffact     => integer(ceil(real(CFG_DDR_BRIDGE_FREQ)/real(CFG_CPU_FREQ))),  --Frequency scaling factor between AHB clocks on master and slave buses.
		memtech   => cfg.arch.memtech,
		pfen      => 1,                 --Prefetch enable. Enables read FIFO.
		irqsync   => 1,                 --Interrupt forwarding.
		wburst    => 32,  --Length of write bursts in 32-bit words
		iburst    => 8,                 --Instruction fetch burst length.
		rburst    => 32,                --Incremental read burst length.
		bar0      => membar_array(tile_id, 7),
		bar1      => ahb2ahb_membar(CFG.ddr.ddr_bus_io, '0', '0', 16#FFF#),
		bar2      => ahb2ahb_membar(MemMAP.ENEMY_DDR_haddr, '0', '0', MemMAP.ENEMY_hmask),
		sbus      => 0,  --The number of the AHB bus to which the slave interface is connected.
		mbus      => 2,  --The number of the AHB bus to which the master interface is connected
		ioarea    => CFG.ddr.ddr_bus_io,  --Address of the I/O area containing the con?guration area for AHB bus connected to the bridge's master interface.
		ibrsten   => 0,                 --Instruction fetch burst enable.
		lckdac    => 2,  --Locked access error detection and correction.  Locked 0-2 0 accesses may lead to deadlock if a locked access is made
						 --while an ongoing read access has received a SPLIT response. The value of lckdac determines how the core
						 --handles this scenario: 0: Core will deadlock, 1: Core will issue an AMBA ERROR response to the locked access,
						 --2: Core will allow both accesses to complete.
		slvmaccsz => 32,  --The maximum size of accesses that will be made to the bridge's slave interface.
		mstmaccsz => 32,  --The maximum size of accesses that will be performed by the bridge's master interface.
		rdcomb    => 0,                 --Read combining.
		wrcomb    => 0,                 --Write combining.
		combmask  => 16#ffff#,          --Read/write combining mask.
		allbrst   => 2,                 --Support all burst types
		ifctrlen  => 0,                 --Interface control enable.
		fcfs      => 0,                 --First-come, First-served operation.
		fcfsmtech => 0,  --Memory technology to use for FCFS buffer.
		scantest  => 0,                 --Enable scan support
		split     => 1,                 --Use AMBA SPLIT responses.
		pipe      => 128				--Insert pipeline registers.
		)
	  port map (
		hclkm  => clk100_ddr,
		hclks  => clk_sys,
		rstn   => rst_sys_n,
		ahbsi  => ahbsi,
		ahbso  => ahbso(CFG.arch.DDR_HINDEX),
		ahbmi  => ahbmi2,
		ahbmo  => ahbmo2(1),
		ahbso2 => ahbso2,
		lcki   => (others => '0'),  --Used in systems with multiple AHB/AHB
		lcko   => open,  --Indicates possible deadlock condition
		ifctrl => ahb2ahb_ifctrl_none);  --Enable master interface, Enable slave interface.

	ahb2ahb0_reverse : ahb2ahb
	  generic map (
		hsindex   => 0,     --Slave I/F AHB index
		hmindex   => CFG.arch.enemy_ddr_mst_idx,     --Master I/F AHB index
		dir       => 0,
		slv       => 0,  --Slave bridge. Used in bi-directional bridge con?guration
		ffact     => integer(ceil(real(CFG_DDR_BRIDGE_FREQ)/real(CFG_CPU_FREQ))),  --Frequency scaling factor between AHB clocks on master and slave buses.
		memtech   => cfg.arch.memtech,
		pfen      => 1,                 --Prefetch enable. Enables read FIFO.
		irqsync   => 1,                 --Interrupt forwarding.
		wburst    => 8,  --Length of write bursts in 32-bit words
		iburst    => 8,                 --Instruction fetch burst length.
		rburst    => 8,                --Incremental read burst length.
		bar0      => ahb2ahb_membar(MemMap.TLM_haddr, '1', '1', MemMAP.TLM_hmask),
		bar1      => ahb2ahb_membar(MemMap.INA_haddr, '0', '0', MemMAP.INA_hmask),
		--bar2      => ahb2ahb_membar(CFG.amba.AHBIO, '0', '0', 16#FFF#),
		sbus      => 2,  --The number of the AHB bus to which the slave interface is connected.
		mbus      => 0,  --The number of the AHB bus to which the master interface is connected
		ioarea    => CFG.amba.AHBIO,  --Address of the I/O area containing the con?guration area for AHB bus connected to the bridge's master interface.
		ibrsten   => 0,                 --Instruction fetch burst enable.
		lckdac    => 2,  --Locked access error detection and correction.  Locked 0-2 0 accesses may lead to deadlock if a locked access is made
						 --while an ongoing read access has received a SPLIT response. The value of lckdac determines how the core
						 --handles this scenario: 0: Core will deadlock, 1: Core will issue an AMBA ERROR response to the locked access,
						 --2: Core will allow both accesses to complete.
		slvmaccsz => 32,  --The maximum size of accesses that will be made to the bridge's slave interface.
		mstmaccsz => 32,  --The maximum size of accesses that will be performed by the bridge's master interface.
		rdcomb    => 0,                 --Read combining.
		wrcomb    => 0,                 --Write combining.
		combmask  => 16#ffff#,          --Read/write combining mask.
		allbrst   => 0,                 --Support all burst types
		ifctrlen  => 0,                 --Interface control enable.
		fcfs      => 0,                 --First-come, First-served operation.
		fcfsmtech => 0,  --Memory technology to use for FCFS buffer.
		scantest  => 0,                 --Enable scan support
		split     => 1,                 --Use AMBA SPLIT responses.
		pipe      => 128				--Insert pipeline registers.
		)
	  port map (
		hclkm  => clk_sys,
		hclks  => clk100_ddr,
		rstn   => rst_sys_n,
		ahbsi  => ahbsi2,
		ahbso  => ahbso2(0),
		ahbmi  => ahbmi,
		ahbmo  => ahbmo(CFG.arch.enemy_ddr_mst_idx),
		ahbso2 => ahbso,
		lcki   => (others => '0'),  --Used in systems with multiple AHB/AHB
		lcko   => open,  --Indicates possible deadlock condition
		ifctrl => ahb2ahb_ifctrl_none);  --Enable master interface, Enable slave interface.


	ahb2mig_hw : if CFG.arch.SIM = 0 generate
--		ddrc : ahb2mig_7series
--		  generic map(
--			hindex                  => 2,
--			haddr                   => CFG.ddr.ddr_addr,
--			hmask                   => CFG.ddr.ddr_mask,
--			pindex                  => CFG.arch.DDR_PINDEX,
--			paddr                   => CFG.ddr.DDR_PADDR,
--			SIM_BYPASS_INIT_CAL     => "OFF",
--			SIMULATION              => "FALSE",
--			USE_MIG_INTERFACE_MODEL => false)
--		  port map(
--			ddr3_dq         => ddr3_dq,
--			ddr3_dqs_p      => ddr3_dqs_p,
--			ddr3_dqs_n      => ddr3_dqs_n,
--			ddr3_addr       => ddr3_addr,
--			ddr3_ba         => ddr3_ba,
--			ddr3_ras_n      => ddr3_ras_n,
--			ddr3_cas_n      => ddr3_cas_n,
--			ddr3_we_n       => ddr3_we_n,
--			ddr3_reset_n    => ddr3_reset_n,
--			ddr3_ck_p       => ddr3_ck_p,
--			ddr3_ck_n       => ddr3_ck_n,
--			ddr3_cke        => ddr3_cke,
--			ddr3_cs_n       => ddr3_cs_n,
--			ddr3_dm         => ddr3_dm,
--			ddr3_odt        => ddr3_odt,
--			ahbsi           => ahbsi2,
--			ahbso           => ahbso2(2),
--			apbi            => apbi,
--			apbo            => open, -- apbo(CFG.arch.DDR_PINDEX),
--			calib_done      => calib_done,
--			rst_n_syn       => rst_sys_n,
--			rst_n_async     => rst_ddr_n,
--			clk_amba        => clk100_ddr,
--			sys_clk_p       => clk400_p,
--			sys_clk_n       => clk400_n,
--			clk_ref_i       => clk200,
--			ui_clk          => clk100_ddr,
--			ui_clk_sync_rst => open,
--			cs_trigger_in	=> cs_trigger_in
--			);
--		end generate;
		ddrc : ahb2axi_mig_7series
		  generic map(
			hindex                  => 2,
			haddr                   => CFG.ddr.ddr_addr,
			hmask                   => CFG.ddr.ddr_mask,
			pindex                  => CFG.arch.DDR_PINDEX,
			paddr                   => CFG.ddr.DDR_PADDR)
		  port map(
			ddr3_dq         => ddr3_dq,
			ddr3_dqs_p      => ddr3_dqs_p,
			ddr3_dqs_n      => ddr3_dqs_n,
			ddr3_addr       => ddr3_addr,
			ddr3_ba         => ddr3_ba,
			ddr3_ras_n      => ddr3_ras_n,
			ddr3_cas_n      => ddr3_cas_n,
			ddr3_we_n       => ddr3_we_n,
			ddr3_reset_n    => ddr3_reset_n,
			ddr3_ck_p       => ddr3_ck_p,
			ddr3_ck_n       => ddr3_ck_n,
			ddr3_cke        => ddr3_cke,
			ddr3_cs_n       => ddr3_cs_n,
			ddr3_dm         => ddr3_dm,
			ddr3_odt        => ddr3_odt,
			ahbsi           => ahbsi2,
			ahbso           => ahbso2(2),
			apbi            => apbi,
			apbo            => open, -- apbo(CFG.arch.DDR_PINDEX),
			calib_done      => calib_done,
			rst_n_syn       => rst100_ddr_n,
			rst_n_async     => rst_ddr_n,
			clk_amba        => clk100_ddr,
			sys_clk_p       => clk400_p,
			sys_clk_n       => clk400_n,
			clk_ref_i       => clk200,
			ui_clk          => clk100_ddr,
			ui_clk_sync_rst => open
			);
		end generate;

	ahb2mig_sim: if CFG.arch.SIM = 1 generate
		ddrc : ahb2mig_7series
		  generic map(
			hindex                  => 2,
			haddr                   => CFG.ddr.ddr_addr,
			hmask                   => CFG.ddr.ddr_mask,
			pindex                  => CFG.arch.DDR_PINDEX,
			paddr                   => CFG.ddr.DDR_PADDR,
			SIM_BYPASS_INIT_CAL     => "OFF", -- not used
			SIMULATION              => "FALSE", -- not used
			USE_MIG_INTERFACE_MODEL => true)
		  port map(
			ddr3_dq         => ddr3_dq,
			ddr3_dqs_p      => ddr3_dqs_p,
			ddr3_dqs_n      => ddr3_dqs_n,
			ddr3_addr       => ddr3_addr,
			ddr3_ba         => ddr3_ba,
			ddr3_ras_n      => ddr3_ras_n,
			ddr3_cas_n      => ddr3_cas_n,
			ddr3_we_n       => ddr3_we_n,
			ddr3_reset_n    => ddr3_reset_n,
			ddr3_ck_p       => ddr3_ck_p,
			ddr3_ck_n       => ddr3_ck_n,
			ddr3_cke        => ddr3_cke,
			ddr3_cs_n       => ddr3_cs_n,
			ddr3_dm         => ddr3_dm,
			ddr3_odt        => ddr3_odt,
			ahbsi           => ahbsi2,
			ahbso           => ahbso2(2),
			apbi            => apbi,
			apbo            => open, -- apbo(CFG.arch.DDR_PINDEX),
			calib_done      => calib_done,
			rst_n_syn       => rst100_ddr_n,
			rst_n_async     => rst_ddr_n,
			clk_amba        => clk100_ddr,
			sys_clk_p       => clk400_p,
			sys_clk_n       => clk400_n,
			clk_ref_i       => clk200,
			ui_clk          => clk100_ddr,
			ui_clk_sync_rst => open,
			cs_trigger_in	=> cs_trigger_in
			);
		end generate;

  end generate;

  not_en_ahb2ahb_ddr : if (CFG.arch.EN_DDR /= 1) generate
	ddr3_dq         <= (others => 'Z');
	ddr3_dqs_p      <= (others => 'Z');
	ddr3_dqs_n      <= (others => 'Z');
	ddr3_addr       <= (others => 'Z');
	ddr3_ba         <= (others => 'Z');
	ddr3_ras_n      <= 'Z';
	ddr3_cas_n      <= 'Z';
	ddr3_we_n       <= 'Z';
	ddr3_reset_n    <= 'Z';
	ddr3_ck_p       <= (others => 'Z');
	ddr3_ck_n       <= (others => 'Z');
	ddr3_cke        <= (others => 'Z');
	ddr3_cs_n       <= (others => 'Z');
	ddr3_dm         <= (others => 'Z');
	ddr3_odt        <= (others => 'Z');

	i_enemy_sig.ddr_enemy <= from_enemy_type_none;
  end generate;

  -----------------------------------------------------------------------------
  -- Ethernet Controller
  -----------------------------------------------------------------------------
  en_ethernet : if (CFG.arch.EN_ETH = 1) generate
	grethm_i : grethm
	  generic map(
		hindex        => cfg.arch.eth_mindex,
		pindex        => cfg.arch.eth_pindex,
		paddr         => cfg.geth.paddr,
		pirq          => cfg.geth.pirq,
		memtech       => cfg.arch.memtech,
		ifg_gap       => cfg.geth.ifg_gap,
		attempt_limit => cfg.geth.attempt_limit,
		backoff_limit => cfg.geth.backoff_limit,
		slot_time     => cfg.geth.slot_time,
		mdcscaler     => cfg.geth.mdcscaler,
		enable_mdio   => cfg.geth.enable_mdio,
		fifosize      => cfg.geth.fifosize,
		nsync         => cfg.geth.nsync,
		edcl          => cfg.geth.edcl,
		edclbufsz     => cfg.geth.edclbufsz,
		burstlength   => cfg.geth.burstlength,
		macaddrh      => cfg.geth.macaddrh,
		macaddrl      => cfg.geth.macaddrl,
		ipaddrh       => cfg.geth.ipaddrh,
		ipaddrl       => cfg.geth.ipaddrl,
		phyrstadr     => cfg.geth.phyrstadr,
		rmii          => cfg.geth.rmii,
		sim           => cfg.geth.sim,
		giga          => cfg.geth.giga,
		oepol         => cfg.geth.oepol,
		scanen        => cfg.geth.scanen,
		ft            => cfg.geth.ft,
		edclft        => cfg.geth.edclft,
		mdint_pol     => cfg.geth.mdint_pol,
		enable_mdint  => cfg.geth.enable_mdint,
		multicast     => cfg.geth.multicast,
		ramdebug      => cfg.geth.ramdebug,
		mdiohold      => cfg.geth.mdiohold,
		maxsize       => cfg.geth.maxsize,
		gmiimode      => cfg.geth.gmiimode)
	  port map(
		rst   => rst_sys_n,
		clk   => clk_sys,
		ahbmi => ahbmi,
		ahbmo => ahbmo(cfg.arch.eth_mindex),
		apbi  => apbi,
		apbo  => apbo(cfg.arch.eth_pindex),
		ethi  => ethi,
		etho  => etho);

	ethi.gtx_clk     <= clk_gtx; -- 125 MHz
	ethi.rmii_clk    <= eth_clk_to_mac;
	ethi.tx_clk      <= eth_tx_clk_rgmii_sel1;
	ethi.tx_dv       <= '1';
	ethi.rx_clk      <= eth_rx_clk;
	ethi.rxd         <= eth_rxd;
	ethi.rx_dv       <= eth_rx_dv_rck;
	ethi.rx_er       <= eth_rx_er_rxdv_er;
	ethi.rx_col      <= eth_col_clk_mac_freq;
	ethi.rx_crs      <= eth_crs_rgmii_sel0;
	ethi.rx_en       <= '1';
	ethi.mdint       <= eth_ninterrupt;
	ethi.phyrstaddr  <= (others => '0');
	ethi.edcladdr    <= (others => '0');
	ethi.edclsepahb  <= '0';
	ethi.edcldisable <= '1';

	eth_gtx_clk_tck        <= clk_gtx; -- 125 MHz
	eth_mdc                <= etho.mdc;
	eth_nreset             <= etho.reset;
	eth_tx_en_txen_er      <= etho.tx_en;
	eth_tx_er              <= etho.tx_er;
	eth_txd                <= etho.txd;

  emdio_pad : iopad
	generic map (
	  level => cmos,
	  voltage => x18v,
	  tech => cfg.arch.padtech)
	port map (
	  eth_mdio,
	  etho.mdio_o,
	  etho.mdio_oe,
	  ethi.mdio_i);

  end generate;

-- unused amba modules
   unsused_ahbm : for i in CFG.arch.UNUSED_AHBMINDEX to CFG.arch.MAXAHBM-2 generate
	 ahbmo(i) <= ahbm_none;
   end generate;

   unsused_ahbs : for i in CFG.arch.UNUSED_AHBSINDEX to CFG.arch.MAXAHBS-1 generate
	 ahbso(i) <= ahbs_none;
   end generate;

   unsused_apb : for i in CFG.arch.UNUSED_PINDEX to CFG.arch.MAXAPB-1 generate
	 apbo(i) <= apb_none;
   end generate;


end;
