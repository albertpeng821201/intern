-----------------------------------------------------------------------------
--  Leon Tile including iNA
--  Copyright LIS
------------------------------------------------------------------------------
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library grlib;
use grlib.amba.all;
use grlib.stdlib.all;

library top;
use top.config_types.all;
use top.config.all;

library ina;
library ina_pkg;
use ina_pkg.ina_config.all;
use ina_pkg.ina_typedefs.all;
use ina_pkg.ina_functions.all;
use ina_pkg.enemy_package.all;
use ina_pkg.ina_package.all;

library enemy;
library enemy_pkg;
use enemy_pkg.ina_enemy_package.all;

library lis;
use lis.mon.all;

library profpga;
use profpga.mmi64_pkg.all;

entity tile is
  generic (
    iNOC_VC_CNT         : integer    := 4;
    iNOC_CLD_VC_CNT     : integer    := 2;
    iNOC_BUFFER_DEPTH   : integer    := 4;
    iNOC_FLIT_SIZE      : integer    := 32;
	iNOC_CTRL_EN		: integer	 := 0;
    iNOC_CTRL_FLIT_SIZE : integer    := 16;
    iNoC_TS_CNT         : integer    := 8;
    iNoC_MAX_TS         : integer    := 3;
    iNOC_DIM_X          : integer    := 2;
    iNOC_DIM_Y          : integer    := 2;
    THIS_X              : integer    := 0;
    THIS_Y              : integer    := 0;
    MASTER_XACTOR_ID    : integer    := 0;
    FPGA_ID             : integer    := 0;
    MMI64_ID            : bit_vector := X"00000000"
  );

  port (
    clk_sys  : in std_ulogic;
    clk_noc  : in std_ulogic;
    clk200   : in std_ulogic;
    clk400_p : in std_logic; --400MHz clock for DDR
    clk400_n : in std_logic;
    clk_gtx  : in std_logic;

    rst_sys_n : in std_ulogic;
    rst_loader_n : in std_ulogic;
    rst_ddr_n : in std_ulogic;
    rst_noc_n : in std_ulogic;

    --MMI64--
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

    --NOC--
    inoc_ctrl_data_out : in std_logic_vector(iNOC_CTRL_FLIT_SIZE downto 0);
    inoc_ctrl_req_out  : in std_logic;
    inoc_ctrl_ack_out  : in std_logic;

    inoc_data_out : in std_logic_vector(iNOC_FLIT_SIZE downto 0);
    inoc_vc_out   : in std_logic_vector(iNOC_CLD_VC_CNT-1 downto 0);
    inoc_req_out  : in std_logic;
    inoc_ack_out  : in std_logic_vector(iNOC_VC_CNT-1 downto 0);

    inoc_ctrl_data_in : out std_logic_vector(iNOC_CTRL_FLIT_SIZE downto 0);
    inoc_ctrl_req_in  : out std_logic;
    inoc_ctrl_ack_in  : out std_logic;

    inoc_data_in : out std_logic_vector(iNOC_FLIT_SIZE downto 0);
    inoc_vc_in   : out std_logic_vector(iNOC_CLD_VC_CNT-1 downto 0);
    inoc_req_in  : out std_logic;
    inoc_ack_in  : out std_logic_vector(iNOC_VC_CNT-1 downto 0);

    txd : out std_logic;
    rxd : in  std_logic;

    -- SSRAM interface
    sram_k_p    : out   std_logic;
    sram_k_n    : out   std_logic;
    sram_a      : out   std_logic_vector (TILE_CONFIG(THIS_Y, THIS_X).mem.SRAM_ADDR_W-1 downto 0);
    sram_dq     : inout std_logic_vector (TILE_CONFIG(THIS_Y, THIS_X).mem.SRAM_DQ_PINS-1 downto 0);
    sram_bws_n  : out   std_logic_vector (TILE_CONFIG(THIS_Y, THIS_X).mem.SRAM_GROUPS-1 downto 0);
    sram_rnw    : out   std_logic;
    sram_ld_n   : out   std_logic;
    sram_doff_n : out   std_logic;
    sram_ready	: out	  std_logic;

    -- Router Status Signals
    router_status_enable      : out std_logic;
    router_status_wr_en       : out std_logic;
    router_status_addr_out    : out std_logic_vector(15 downto 0);
    router_status_data_wr_out : out std_logic_vector(31 downto 0);
    router_status_data_rd_in  : in  std_logic_vector(31 downto 0);

    router_mon_link   : in    std_logic_vector(port_cnt-1 downto 0);
    router_mon_vc     : in    std_logic_vector(port_cnt*log2x(vc_cnt+1)-1 downto 0);
    router_mon_buffer : in    std_logic_vector(port_cnt*vc_cnt*log2x(buff_size+1)-1 downto 0);

    -- DDR Signals
    ddr3_dq          : inout std_logic_vector(63 downto 0);
    ddr3_dqs_p       : inout std_logic_vector(7 downto 0);
    ddr3_dqs_n       : inout std_logic_vector(7 downto 0);
    ddr3_addr        : out   std_logic_vector(14 downto 0);
    ddr3_ba          : out   std_logic_vector(2 downto 0);
    ddr3_ras_n       : out   std_logic;
    ddr3_cas_n       : out   std_logic;
    ddr3_we_n        : out   std_logic;
    ddr3_reset_n     : out   std_logic;
    ddr3_ck_p        : out   std_logic_vector(0 downto 0);
    ddr3_ck_n        : out   std_logic_vector(0 downto 0);
    ddr3_cke         : out   std_logic_vector(0 downto 0);
    ddr3_cs_n        : out   std_logic_vector(0 downto 0);
    ddr3_dm          : out   std_logic_vector(7 downto 0);
    ddr3_odt         : out   std_logic_vector(0 downto 0);
    ddr3_calib_done  : out   std_logic;

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

    lock : out std_ulogic
  );
end;

architecture rtl of tile is

  signal inoc_data_in_tmp : std_logic_vector(iNOC_FLIT_SIZE downto 0);
  signal inoc_vc_in_tmp   : std_logic_vector(iNOC_CLD_VC_CNT-1 downto 0);
  signal inoc_req_in_tmp  : std_logic;
  signal inoc_ack_in_tmp  : std_logic_vector(iNOC_VC_CNT-1 downto 0);

  signal ahbsi_ls : ahb_slv_in_type;
  signal ahbso_ls : ahb_slv_out_type;

  signal ahbsi_mp : ahb_slv_in_type;
  signal ahbso_mp : ahb_slv_out_type;

  signal na_mst_rx_in  : ahb_mst_in_type;
  signal na_mst_rx_out : ahb_mst_out_type;

  signal na_mst_tx_in  : ahb_mst_in_type;
  signal na_mst_tx_out : ahb_mst_out_type;

  signal router_status_data : router_status_data_type;
  signal router_status_req  : router_status_req_type;
  signal router_mon_data    : router_mon_type;

  signal na_irq : na_irq_type;

  signal tile_src : std_logic_vector(log2x(dim_x*dim_y)-1 downto 0);

  signal cs_dbg        : std_logic_vector(199 downto 0);
  signal bus_error     : std_logic_vector(16 downto 0);

  signal from_enemy : from_all_enemy_type;
  signal to_enemy_ddr		: to_enemy_type;
  signal to_enemy_sram	: to_enemy_type;

begin

  inoc_data_in <= inoc_data_in_tmp;
  inoc_vc_in   <= inoc_vc_in_tmp;
  inoc_req_in  <= inoc_req_in_tmp;
  inoc_ack_in  <= inoc_ack_in_tmp;

  router_status_enable      <= router_status_req.enable;
  router_status_wr_en       <= router_status_req.wr_en;
  router_status_addr_out    <= router_status_req.addr_out;
  router_status_data_wr_out <= router_status_req.data_wr_out;
  router_status_data        <= router_status_data_rd_in;

  leon_tile : entity work.leon3mp
    generic map(
      CFG               => TILE_CONFIG(THIS_Y, THIS_X),
      X_LOC             => THIS_X,
      Y_LOC             => THIS_Y,
      MASTER_XACTOR_ID  => MASTER_XACTOR_ID,
      FPGA_ID           => FPGA_ID,
      MMI64_ID          => MMI64_ID
    ) port map (
      clk_sys  => clk_sys,
      clk200   => clk200,
      clk400_p => clk400_p,
      clk400_n => clk400_n,
      clk_gtx  => clk_gtx,

      ext_rst_sys_n => rst_sys_n,
      rst_sys_n     => rst_loader_n,
      rst_ddr_n     => rst_ddr_n,

      mmi64_clk           => mmi64_clk,
      mmi64_reset         => mmi64_reset,
      mmi64_h_up_d_o      => mmi64_h_up_d_o ,
      mmi64_h_up_valid_o  => mmi64_h_up_valid_o,
      mmi64_h_up_accept_i => mmi64_h_up_accept_i,
      mmi64_h_up_start_o  => mmi64_h_up_start_o,
      mmi64_h_up_stop_o   => mmi64_h_up_stop_o ,
      mmi64_h_dn_d_i      => mmi64_h_dn_d_i,
      mmi64_h_dn_valid_i  => mmi64_h_dn_valid_i,
      mmi64_h_dn_accept_o => mmi64_h_dn_accept_o,
      mmi64_h_dn_start_i  => mmi64_h_dn_start_i,
      mmi64_h_dn_stop_i   => mmi64_h_dn_stop_i ,

      ahb_na_mp_si => ahbsi_mp,
      ahb_na_mp_so => ahbso_mp,

      ahb_na_ls_si => ahbsi_ls,
      ahb_na_ls_so => ahbso_ls,

      na_mst_rx_in  => na_mst_rx_in,
      na_mst_rx_out => na_mst_rx_out,

      na_mst_tx_in  => na_mst_tx_in,
      na_mst_tx_out => na_mst_tx_out,

      txd1 => txd, -- UART1 tx data
      rxd1 => rxd, -- UART1 rx data

      bus_error => bus_error,

      -- SSRAM
      sram_k_p     => sram_k_p,
      sram_k_n     => sram_k_n,
      sram_a       => sram_a,
      sram_dq      => sram_dq,
      sram_bws_n   => sram_bws_n,
      sram_rnw     => sram_rnw,
      sram_ld_n    => sram_ld_n,
      sram_doff_n  => sram_doff_n,
      sram_ready   => sram_ready,

      -- DDR3
      ddr3_dq          => ddr3_dq,
      ddr3_dqs_p       => ddr3_dqs_p,
      ddr3_dqs_n       => ddr3_dqs_n,
      ddr3_addr        => ddr3_addr,
      ddr3_ba          => ddr3_ba,
      ddr3_ras_n       => ddr3_ras_n,
      ddr3_cas_n       => ddr3_cas_n,
      ddr3_we_n        => ddr3_we_n,
      ddr3_reset_n     => ddr3_reset_n,
      ddr3_ck_p        => ddr3_ck_p,
      ddr3_ck_n        => ddr3_ck_n,
      ddr3_cke         => ddr3_cke,
      ddr3_cs_n        => ddr3_cs_n,
      ddr3_dm          => ddr3_dm,
      ddr3_odt         => ddr3_odt,
      ddr3_calib_done  => ddr3_calib_done,

      -- Ethernet interface
      eth_clk_to_mac        => eth_clk_to_mac,
      eth_col_clk_mac_freq  => eth_col_clk_mac_freq,
      eth_crs_rgmii_sel0    => eth_crs_rgmii_sel0,
      eth_gtx_clk_tck       => eth_gtx_clk_tck,
      eth_mdc               => eth_mdc,
      eth_mdio              => eth_mdio,
      eth_ninterrupt        => eth_ninterrupt,
      eth_nreset            => eth_nreset,
      eth_rx_clk            => eth_rx_clk,
      eth_rx_dv_rck         => eth_rx_dv_rck,
      eth_rx_er_rxdv_er     => eth_rx_er_rxdv_er,
      eth_rxd               => eth_rxd,
      eth_tx_clk_rgmii_sel1 => eth_tx_clk_rgmii_sel1,
      eth_tx_en_txen_er     => eth_tx_en_txen_er,
      eth_tx_er             => eth_tx_er,
      eth_txd               => eth_txd,

      lock      => lock,
      cs_trigger_in => cs_dbg,

      na_irq => na_irq,

	  tile_src => tile_src,
	  o_enemy => from_enemy,
	  i_enemy_ddr	=> to_enemy_ddr,
	  i_enemy_sram	=> to_enemy_sram
      );

  --gen_port0 : for p in 0 to port_cnt-1 generate
  -- router_mon_data(p).link <= router_mon_link(p);
  -- router_mon_data(p).vc   <= router_mon_vc((p+1)*log2x(vc_cnt+1)-1 downto p*log2x(vc_cnt+1));
  --gen_vc0 : for v in 0 to vc_cnt-1 generate
  --  router_mon_data(p).buff(v) <= router_mon_buffer(p*vc_cnt*log2x(buff_size+1)+(v+1)*log2x(buff_size+1)-1 downto p*vc_cnt*log2x(buff_size+1)+v*log2x(buff_size+1));
  --end generate;
  --end generate;

  -----------------------------------------------------------------------
  ---  NA SLAVE instance ------------------------------------------------
  -----------------------------------------------------------------------

  na0 : entity ina.networkadapter
    generic map (
      hindex_mp => TILE_CONFIG(THIS_Y, THIS_X).arch.NA_HINDEX,       --- Slave # 04, 07 is AHB RAM
      haddr_mp  => TILE_CONFIG(THIS_Y, THIS_X).na.NAADDR_MP,       -- defined in config.vhd
      hmask_mp  => TILE_CONFIG(THIS_Y, THIS_X).na.NAMASK_MP,

      hindex_ls => TILE_CONFIG(THIS_Y, THIS_X).arch.l2c_hindexm,              --- Slave Number changed for L2 controller
    --haddr_ls  => TILE_CONFIG(THIS_Y, THIS_X).na.NAADDR_LS,       -- defined in config.vhd
    --hmask_ls  => TILE_CONFIG(THIS_Y, THIS_X).na.NAMASk_LS,

      hindex_na_tx_mst => TILE_CONFIG(THIS_Y, THIS_X).arch.NA_MST_TX_IDX,
      hindex_na_rx_mst => TILE_CONFIG(THIS_Y, THIS_X).arch.NA_MST_RX_IDX,
      vc_cnt           => iNOC_VC_CNT,
      buffer_depth     => iNOC_BUFFER_DEPTH,
      flit_size        => iNOC_FLIT_SIZE,
	  ctrl_en		   => iNOC_CTRL_EN,
      ctrl_flit_size   => iNOC_CTRL_FLIT_SIZE,
      ts_cnt           => iNoC_TS_CNT,
      ts_max           => iNoC_MAX_TS,
      x_loc            => THIS_X,
      y_loc            => THIS_Y,
      ddr_used         => TILE_CONFIG(DDR_y_loc,DDR_x_loc).arch.en_ddr
    ) port map (
      rst_ahb => rst_loader_n,
      clk_ahb => clk_sys,

      na_slv_ls_in  => ahbsi_ls,
      na_slv_ls_out => ahbso_ls,
      na_slv_mp_in  => ahbsi_mp,
      na_slv_mp_out => ahbso_mp,

      na_mst_rx_in  => na_mst_rx_in,
      na_mst_rx_out => na_mst_rx_out,

      na_mst_tx_in  => na_mst_tx_in,
      na_mst_tx_out => na_mst_tx_out,

      rst_noc => rst_noc_n,
      clk_noc => clk_noc,

      data_in => inoc_data_out,
      vc_in   => inoc_vc_out,
      req_in  => inoc_req_out,
      ack_out => inoc_ack_in_tmp,

      data_out => inoc_data_in_tmp,
      vc_out   => inoc_vc_in_tmp,
      req_out  => inoc_req_in_tmp,
      ack_in   => inoc_ack_out,

      --added TD
      ctrl_data_in => inoc_ctrl_data_out,
      ctrl_req_in  => inoc_ctrl_req_out,
      ctrl_ack_out => inoc_ctrl_ack_in,

      ctrl_data_out => inoc_ctrl_data_in,
      ctrl_req_out  => inoc_ctrl_req_in,
      ctrl_ack_in   => inoc_ctrl_ack_out,

      cs_dbg => cs_dbg,
      bus_error => bus_error,

      router_status_data => router_status_data,
      router_status_req  => router_status_req,

      na_irq => na_irq,

	  tile_src => tile_src,
	  i_enemy => from_enemy,
	  o_enemy_ddr	=> to_enemy_ddr,
	  o_enemy_sram	=> to_enemy_sram
      );
end;
