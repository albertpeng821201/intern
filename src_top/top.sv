//`define SIMUL_WORKAROUND

/*
SIMUL_WORKAROUND does 4 things:

- connect the "chipit" sram for the simulations
- uses the reset and clock directly from the tb instead of the derived version via the "sync" signals
- use the generics for the NoC routers (black box for synthesis)
- connect the mmi64 transactor to the iotile for synthesis

*/

`ifndef SIMUL_WORKAROUND
`include "pd_muxdemux_param.vh"
`endif

import functions::*;
import types::*;
import parameters::*;

module top (
`ifdef SIMUL_WORKAROUND
    sys_rstn_sim,
    loader_rstn_sim,
    sys_clk_sim,
    clk200_sim,
    clk2_p_sim,
    clk2_n_sim
`else
    clk0_p,
    clk0_n,

    sync0_p,
    sync0_n,

    dmbi_h2f,
    dmbi_f2h,

    tx_pin,
    rx_pin,

    tx2_pin,
    rx2_pin,

    clk1_p,
    clk1_n,

    sync1_p,
    sync1_n,

    led_red,
    led_green,
    led_blue,
    led_yellow,

    `ifdef EN_DDR
        clk2_p,
        clk2_n,
    `endif

    // SSRAM interface
    sram0_k_p,
    sram0_k_n,
    sram0_a,
    sram0_dq,
    sram0_bws_n,
    sram0_rnw,
    sram0_ld_n,
    sram0_doff_n,

    sram1_k_p,
    sram1_k_n,
    sram1_a,
    sram1_dq,
    sram1_bws_n,
    sram1_rnw,
    sram1_ld_n,
    sram1_doff_n,

    sram2_k_p,
    sram2_k_n,
    sram2_a,
    sram2_dq,
    sram2_bws_n,
    sram2_rnw,
    sram2_ld_n,
    sram2_doff_n,

    sram3_k_p,
    sram3_k_n,
    sram3_a,
    sram3_dq,
    sram3_bws_n,
    sram3_rnw,
    sram3_ld_n,
    sram3_doff_n

    `ifdef EN_DDR
        ,
        ddr3_dq,
        ddr3_dqs_p,
        ddr3_dqs_n,
        ddr3_addr,
        ddr3_ba ,
        ddr3_ras_n,
        ddr3_cas_n ,
        ddr3_we_n   ,
        ddr3_reset_n,
        ddr3_ck_p,
        ddr3_ck_n,
        ddr3_cke ,
        ddr3_cs_n ,
        ddr3_dm ,
        ddr3_odt
    `endif

    `ifdef EN_ETH
        ,
        eth_clk_to_mac,
        eth_col_clk_mac_freq,
        eth_crs_rgmii_sel0,
        eth_gtx_clk_tck,
        eth_mdc,
        eth_mdio,
        eth_ninterrupt,
        eth_nreset,
        eth_rx_clk,
        eth_rx_dv_rck,
        eth_rx_er_rxdv_er,
        eth_rxd,
        eth_tx_clk_rgmii_sel1,
        eth_tx_en_txen_er,
        eth_tx_er,
        eth_txd
    `endif
`endif
    );

    parameter ADDR_W  = 22;
    parameter DQ_PINS = 18;
    parameter GROUPS  = 2;

    localparam TX_PINS = 59;
    localparam RX_PINS = 59;

    parameter int unsigned FPGA = TA3;
    parameter int fpga_x = 0;
    parameter int fpga_y = 0;

    `ifdef SIMUL_WORKAROUND
        input  logic       sys_rstn_sim;
        input  logic       sys_clk_sim;
        input  logic       loader_rstn_sim;
        input  logic       clk200_sim;
        input  logic       clk2_p_sim;
        input  logic       clk2_n_sim;
    `else
        output logic               sram0_k_p;
        output logic               sram0_k_n;
        output logic [ADDR_W-1:0]  sram0_a;
        inout  tri   [DQ_PINS-1:0] sram0_dq;
        output logic [GROUPS-1:0]  sram0_bws_n;
        output logic               sram0_rnw;
        output logic               sram0_ld_n;
        output logic               sram0_doff_n;

        output logic               sram1_k_p;
        output logic               sram1_k_n;
        output logic [ADDR_W-1:0]  sram1_a;
        inout  tri   [DQ_PINS-1:0] sram1_dq;
        output logic [GROUPS-1:0]  sram1_bws_n;
        output logic               sram1_rnw;
        output logic               sram1_ld_n;
        output logic               sram1_doff_n;

        output logic               sram2_k_p;
        output logic               sram2_k_n;
        output logic [ADDR_W-1:0]  sram2_a;
        inout  tri   [DQ_PINS-1:0] sram2_dq;
        output logic [GROUPS-1:0]  sram2_bws_n;
        output logic               sram2_rnw;
        output logic               sram2_ld_n;
        output logic               sram2_doff_n;

        output logic               sram3_k_p;
        output logic               sram3_k_n;
        output logic [ADDR_W-1:0]  sram3_a;
        inout  tri   [DQ_PINS-1:0] sram3_dq;
        output logic [GROUPS-1:0]  sram3_bws_n;
        output logic               sram3_rnw;
        output logic               sram3_ld_n;
        output logic               sram3_doff_n;

        `ifdef EN_DDR
            inout wire  [63:0] ddr3_dq;
            inout wire  [7:0]  ddr3_dqs_n;
            inout wire  [7:0]  ddr3_dqs_p;
            output wire [14:0] ddr3_addr;
            output wire [2:0]  ddr3_ba;
            output wire        ddr3_ras_n;
            output wire        ddr3_cas_n;
            output wire        ddr3_we_n;
            output wire        ddr3_reset_n;
            output wire        ddr3_ck_p;
            output wire        ddr3_ck_n;
            output wire        ddr3_cke;
            output wire        ddr3_cs_n;
            output wire [7:0]  ddr3_dm;
            output wire        ddr3_odt;
        `endif

        `ifdef EN_ETH
            input  logic       eth_clk_to_mac;
            input  logic       eth_col_clk_mac_freq;
            input  logic       eth_crs_rgmii_sel0;
            output logic       eth_gtx_clk_tck;
            output logic       eth_mdc;
            inout  tri         eth_mdio;
            input  logic       eth_ninterrupt;
            output logic       eth_nreset;
            input  logic       eth_rx_clk;
            input  logic       eth_rx_dv_rck;
            input  logic       eth_rx_er_rxdv_er;
            input  logic [7:0] eth_rxd;
            input  logic       eth_tx_clk_rgmii_sel1;
            output logic       eth_tx_en_txen_er;
            output logic       eth_tx_er;
            output logic [7:0] eth_txd;
        `endif

        /*  proFPGA */
        input logic clk0_p;
        input logic clk0_n;
        input logic sync0_p;
        input logic sync0_n;

        input logic clk1_p;
        input logic clk1_n;
        input logic sync1_p;
        input logic sync1_n;

        output logic [19 : 0] dmbi_f2h;
        input logic [19 : 0]  dmbi_h2f;

        // TX and RX pins
        output wire [TX_PINS-1:0] tx_pin;   // input  data pins
        input  wire [RX_PINS-1:0] rx_pin;   // output data pins

        output wire [TX_PINS-1:0] tx2_pin;   // input  data pins
        input  wire [RX_PINS-1:0] rx2_pin;   // output data pins

        output logic led_red;
        output logic led_green;
        output logic led_blue;
        output logic led_yellow;

        `ifdef EN_DDR
            input logic     clk2_n;
            input logic     clk2_p;
        `endif

    `endif

    // INTERNAL SIGNALS
    // Clock and Reset
    logic ahb_clk, noc_clk;  // internal clock signals
    logic ahb_rst, noc_rst;  // internal reset signals

    logic clk_cs;
    assign clk_cs = ahb_clk;

    // iNoC internal Signals used to connect the routers as a meshed NoC
    conf_port_type [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0] conf_port;
    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [CLD(G_DIM_X)-1:0] coord_x;
    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [CLD(G_DIM_Y)-1:0] coord_y;

    ch_t [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0] ch_in;
    ch_t [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0] ch_out;
    ch_t [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0] ch_in_dbg;

    router_status_data_t [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] router_status_data;
    router_status_req_t  [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] router_status_req;

    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0]                                router_mon_link;
    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS*CLD(G_VCS+1)-1:0]                   router_mon_vc;
    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS*G_VCS*CLD(G_VC_BUFFER_DEPTH+1)-1:0] router_mon_buffer;
    mon_data_t [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0]                           router_mon_data;

    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] uart_txd, uart_rxd;

    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] lock_o;
    logic lock;

    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [115:0] cs_dbg;
    logic [G_DIM_X_LOCAL-1:0] [G_DIM_Y_LOCAL-1:0] [G_PORTS-1:0] port_enable;

    logic [7:0] enable;
    ch_t  [7:0] ch_conn;
    logic [7:0] ack;

    logic ddr_rstn;

    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]               sram_k_p;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]               sram_k_n;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [ADDR_W-1:0]  sram_a;
    tri   [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [DQ_PINS-1:0] sram_dq;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [GROUPS-1:0]  sram_bws_n;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]               sram_rnw;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]               sram_ld_n;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]               sram_doff_n;
    wire  [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]               sram_ready;
    wire  [G_DIM_Y_LOCAL*G_DIM_X_LOCAL-1:0]                     calib_done;

    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [63:0] w_ddr3_dq;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [7:0]  w_ddr3_dqs_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [7:0]  w_ddr3_dqs_p;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [14:0] w_ddr3_addr;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [2:0]  w_ddr3_ba;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_ras_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_cas_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_we_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_reset_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_ck_p;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_ck_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_cke;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_cs_n;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [7:0]  w_ddr3_dm;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_odt;
    wire [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]        w_ddr3_calib_done;
    wire [G_DIM_Y_LOCAL*G_DIM_X_LOCAL-1:0]              ddr3_calib_done, sync_ddr3_calib_done;

    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_mii_data;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_tx_clk;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_rx_clk;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [3:0] w_phy_rx_data;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_dv;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_rx_er;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_col;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_crs;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0] [3:0] w_phy_tx_data;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_tx_en;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_tx_er;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_n_int;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_mii_clk;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_rst_n;
    logic [G_DIM_Y_LOCAL-1:0] [G_DIM_X_LOCAL-1:0]       w_phy_npwrdwn;

    /*proFPGA */
    logic       sys_clk;
    logic       sys_rstn;
    logic       sys_clk_pad;
    logic       sys_rst_in;

    wire        mmi64_clk;
    wire        mmi64_reset;
    wire        clk200;

    wire [63:0] mmi64_router_to_router_up_d;
    wire        mmi64_router_to_router_up_valid;
    wire        mmi64_router_to_router_up_accept;
    wire        mmi64_router_to_router_up_start;
    wire        mmi64_router_to_router_up_stop;

    wire [63:0] mmi64_router_to_router_dn_d;
    wire        mmi64_router_to_router_dn_valid;
    wire        mmi64_router_to_router_dn_accept;
    wire        mmi64_router_to_router_dn_start;
    wire        mmi64_router_to_router_dn_stop;

    wire [G_DIM_X_LOCAL*G_DIM_Y_LOCAL*64-1:0] mmi64_router_to_module_up_d;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_up_valid;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_up_accept;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_up_start;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_up_stop;

    wire [G_DIM_X_LOCAL*G_DIM_Y_LOCAL*64-1:0] mmi64_router_to_module_dn_d;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_dn_valid;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_dn_accept;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_dn_start;
    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  mmi64_router_to_module_dn_stop;

    wire [0:(G_DIM_X_LOCAL*G_DIM_Y_LOCAL-1)]  module_presence_detection;

    wire [19:0] clk_cfg_dn[1:1];
    wire [19:0] clk_cfg_up[1:1];
    wire [19:0] cfg_up;
    wire [19:0] cfg_dn;

    genvar x, y, z;
    genvar i, j;

`ifndef SIMUL_WORKAROUND
    /* proFPGA added */
    /* ODDR->led to show us that clock is working */
    ODDR clk_out_buf(
        .D1(1'b1),
        .D2(1'b0),
        .CE(1'b1),
        .C(sys_clk),
        .S(1'b0),
        .R(1'b0),
        .Q(led_red)
    );

    /* rst->led */;
    assign led_green = delay_rstn;
    assign led_yellow = ddr_rstn;
    assign led_blue  = w_ddr3_calib_done[DDR_Y_LOC][DDR_X_LOC];

    assign sram0_k_p    = sram_k_p[0][0];
    assign sram0_k_n    = sram_k_n[0][0];
    assign sram0_a      = sram_a[0][0];
    assign sram0_dq     = sram_dq[0][0];
    assign sram0_bws_n  = sram_bws_n[0][0];
    assign sram0_rnw    = sram_rnw[0][0];
    assign sram0_ld_n   = sram_ld_n[0][0];
    assign sram0_doff_n = sram_doff_n[0][0];

    assign sram1_k_p    = sram_k_p[0][1];
    assign sram1_k_n    = sram_k_n[0][1];
    assign sram1_a      = sram_a[0][1];
    assign sram1_dq     = sram_dq[0][1];
    assign sram1_bws_n  = sram_bws_n[0][1];
    assign sram1_rnw    = sram_rnw[0][1];
    assign sram1_ld_n   = sram_ld_n[0][1];
    assign sram1_doff_n = sram_doff_n[0][1];

    assign sram2_k_p    = sram_k_p[1][0];
    assign sram2_k_n    = sram_k_n[1][0];
    assign sram2_a      = sram_a[1][0];
    assign sram2_dq     = sram_dq[1][0];
    assign sram2_bws_n  = sram_bws_n[1][0];
    assign sram2_rnw    = sram_rnw[1][0];
    assign sram2_ld_n   = sram_ld_n[1][0];
    assign sram2_doff_n = sram_doff_n[1][0];

    assign sram3_k_p    = sram_k_p[1][1];
    assign sram3_k_n    = sram_k_n[1][1];
    assign sram3_a      = sram_a[1][1];
    assign sram3_dq     = sram_dq[1][1];
    assign sram3_bws_n  = sram_bws_n[1][1];
    assign sram3_rnw    = sram_rnw[1][1];
    assign sram3_ld_n   = sram_ld_n[1][1];
    assign sram3_doff_n = sram_doff_n[1][1];

    profpga_ctrl U_PROFPGA_CTRL (
        // access to FPGA pins
        .clk0_p          (clk0_p),
        .clk0_n          (clk0_n),
        .sync0_p         (sync0_p),
        .sync0_n         (sync0_n),
        .srcclk_p        (),
        .srcclk_n        (),
        .srcsync_p       (),
        .srcsync_n       (),
        .dmbi_h2f        (dmbi_h2f),
        .dmbi_f2h        (dmbi_f2h),

        // 200 MHz clock (useful for delay calibration)
        .clk_200mhz_o    (clk200),

        // clk0 sync events (synchronized with mmi64_clk)
        .clk0_event_id_o      (),
        .clk0_event_en_o      (),
        .clk0_event_strobe1_o (),
        .clk0_event_strobe2_o (),

        // MMI-64 access (synchronous to mmi64_clk)
        .mmi64_present_i      (1'b1),
        .mmi64_clk_o          (mmi64_clk),
        .mmi64_reset_o        (mmi64_reset),
        .mmi64_m_dn_d_o       (mmi64_router_to_router_dn_d),
        .mmi64_m_dn_valid_o   (mmi64_router_to_router_dn_valid),
        .mmi64_m_dn_accept_i  (mmi64_router_to_router_dn_accept),
        .mmi64_m_dn_start_o   (mmi64_router_to_router_dn_start),
        .mmi64_m_dn_stop_o    (mmi64_router_to_router_dn_stop),
        .mmi64_m_up_d_i       (mmi64_router_to_router_up_d),
        .mmi64_m_up_valid_i   (mmi64_router_to_router_up_valid),
        .mmi64_m_up_accept_o  (mmi64_router_to_router_up_accept),
        .mmi64_m_up_start_i   (mmi64_router_to_router_up_start),
        .mmi64_m_up_stop_i    (mmi64_router_to_router_up_stop),


        // source clock/sync input
        .src_clk_i            (),
        .src_clk_locked_i     (4'b1111),
        .src_event_id_i       (),
        .src_event_en_i       (),
        .src_event_busy_o     (),
        .src_event_reset_i    (4'b1111),
        .src_event_strobe1_i  (),
        .src_event_strobe2_i  (),

        // clock configuration ports (synchronous to mmi64_clk)
        .clk1_cfg_dn_o        (cfg_dn),
        .clk1_cfg_up_i        (cfg_up),
        .clk2_cfg_dn_o        (),
        .clk2_cfg_up_i        (),
        .clk3_cfg_dn_o        (),
        .clk3_cfg_up_i        (),
        .clk4_cfg_dn_o        (),
        .clk4_cfg_up_i        (),
        .clk5_cfg_dn_o        (),
        .clk5_cfg_up_i        (),
        .clk6_cfg_dn_o        (),
        .clk6_cfg_up_i        (),
        .clk7_cfg_dn_o        (),
        .clk7_cfg_up_i        ()
    );

    wire [`PD_MUXDEMUX_CLK_WIDTH-1:0] clk_mux;
    wire [`PD_MUXDEMUX_MODE_WIDTH-1:0] mux_mode;
    reg rst_clk200;
    always @(posedge mmi64_clk) rst_clk200 <= mmi64_reset;

    pd_mux_ctrl_core #(
        .PD_MUXDEMUX_MUX_TYPE                  (`PD_MUXDEMUX_MUX_TYPE),
        .PD_MUXDEMUX_SIM_MODE                  (`PD_MUXDEMUX_SIM_MODE),
        .PD_MUXDEMUX_MUX_FACTOR                (`PD_MUXDEMUX_MUX_FACTOR),
        .PD_MUXDEMUX_WORD_COUNT                (`PD_MUXDEMUX_WORD_COUNT),
        .PD_MUXDEMUX_MIN_DATARATE_MBIT         (`PD_MUXDEMUX_MIN_DATARATE_MBIT),
        .PD_MUXDEMUX_MAX_DATARATE_MBIT         (`PD_MUXDEMUX_MAX_DATARATE_MBIT),
        .PD_MUXDEMUX_MIN_CLK_BASE_FREQUENCY_HZ (`PD_MUXDEMUX_MIN_CLK_BASE_FREQUENCY_HZ),
        .PD_MUXDEMUX_MAX_CLK_BASE_FREQUENCY_HZ (`PD_MUXDEMUX_MAX_CLK_BASE_FREQUENCY_HZ),
        .PD_MUXDEMUX_MIN_CLK_BASE_PERIOD_NS    (`PD_MUXDEMUX_MIN_CLK_BASE_PERIOD_NS),
        .PD_MUXDEMUX_MAX_CLK_BASE_PERIOD_NS    (`PD_MUXDEMUX_MAX_CLK_BASE_PERIOD_NS),
        .PD_MUXDEMUX_FPGA_TECHNOLOGY           (`PD_MUXDEMUX_FPGA_TECHNOLOGY),
        .PD_MUXDEMUX_PLL_M                     (`PD_MUXDEMUX_PLL_M),
        .PD_MUXDEMUX_PLL_D0                    (`PD_MUXDEMUX_PLL_D0),
        .PD_MUXDEMUX_PLL_D1                    (`PD_MUXDEMUX_PLL_D1),
        .PD_MUXDEMUX_PLL_D2                    (`PD_MUXDEMUX_PLL_D2),
        .PD_MUXDEMUX_PLL_D3                    (`PD_MUXDEMUX_PLL_D3),
        .PD_MUXDEMUX_ENABLE_DCIRESET           (`PD_MUXDEMUX_ENABLE_DCIRESET))
    U_PD_MUX_CTRL_CORE (
        .clk_p               (clk1_p),
        .clk_n               (clk1_n),
        .sync_p              (sync1_p),
        .sync_n              (sync1_n),
        .clk200              (clk200),
        .rst_clk200          (rst_clk200),
        .clk                 (clk_mux),
        .clk_dut             (sys_clk),
        .rst_dut             (sys_rst_in),
        .mux_mode            (mux_mode),
        .mmi64_clk           (mmi64_clk),
        .mmi64_reset         (mmi64_reset),
        .cfg_dn_i            (cfg_dn),
        .cfg_up_o            (cfg_up),
        .dbg_mmcm_locked     (),
        .dbg_idelay_ctrl_rdy (),
        .dbg_dcireset_locked (),
        .dbg_training_done   (),
        .dbg_training_error  (),
        .dbg_clk_dv_period   (),
        .dbg_half_bit_period (),
        .dbg_rst_dut_base    ()
    );

    mmi64_router #(
        .PORT_COUNT(G_DIM_X_LOCAL*G_DIM_Y_LOCAL))
    mmi64_router (
        .mmi64_clk           (mmi64_clk),
        .mmi64_reset         (mmi64_reset),

        .mmi64_h_up_d_o      (mmi64_router_to_router_up_d),
        .mmi64_h_up_valid_o  (mmi64_router_to_router_up_valid),
        .mmi64_h_up_accept_i (mmi64_router_to_router_up_accept),
        .mmi64_h_up_start_o  (mmi64_router_to_router_up_start),
        .mmi64_h_up_stop_o   (mmi64_router_to_router_up_stop),

        .mmi64_h_dn_d_i      (mmi64_router_to_router_dn_d),
        .mmi64_h_dn_valid_i  (mmi64_router_to_router_dn_valid),
        .mmi64_h_dn_accept_o (mmi64_router_to_router_dn_accept),
        .mmi64_h_dn_start_i  (mmi64_router_to_router_dn_start),
        .mmi64_h_dn_stop_i   (mmi64_router_to_router_dn_stop),

        .mmi64_m_up_d_i      (mmi64_router_to_module_up_d),
        .mmi64_m_up_valid_i  (mmi64_router_to_module_up_valid),
        .mmi64_m_up_accept_o (mmi64_router_to_module_up_accept),
        .mmi64_m_up_start_i  (mmi64_router_to_module_up_start),
        .mmi64_m_up_stop_i   (mmi64_router_to_module_up_stop),

        .mmi64_m_dn_d_o      (mmi64_router_to_module_dn_d),
        .mmi64_m_dn_valid_o  (mmi64_router_to_module_dn_valid),
        .mmi64_m_dn_accept_i (mmi64_router_to_module_dn_accept),
        .mmi64_m_dn_start_o  (mmi64_router_to_module_dn_start),
        .mmi64_m_dn_stop_o   (mmi64_router_to_module_dn_stop),

        .module_presence_detection_i (module_presence_detection),
        .mmi64_initialize_o ()
    );

    /* proFPGA added END */
`endif

    /*
     * DUT_reset only after DDR3 and SRAM did their calibration
     */
    wire delay_rstn;
    assign ddr_rstn = ~sys_rst_in;
    assign sys_rstn = ~sys_rst_in;

    dff_sync_reset delay_calib (
        .data(&calib_done),
        .clk(sys_clk),
        .reset(sys_rstn),
        .q(delay_rstn)
    );

    synchronizer #(
        .DATA_WIDTH(G_DIM_X_LOCAL*G_DIM_Y_LOCAL))
    ddr2dut (
        .clk     (sys_clk),
        .reset_n (sys_rstn),
        .data_i  (ddr3_calib_done),
        .data_o  (sync_ddr3_calib_done )
    );

    generate
    for (y=0; y<G_DIM_Y_LOCAL; y++) begin : calib_y
        for (x=0; x<G_DIM_X_LOCAL; x++) begin : calib_x
            assign ddr3_calib_done[y*G_DIM_X_LOCAL + x] = w_ddr3_calib_done[y][x];
            assign calib_done[y*G_DIM_X_LOCAL + x] = sram_ready[y][x] & sync_ddr3_calib_done[y*G_DIM_X_LOCAL + x];
            assign module_presence_detection[y*G_DIM_X_LOCAL + x] = 1'b1;
        end
    end
    endgenerate

    generate
    for (y=0; y<G_DIM_Y_LOCAL; y++) begin : tile_y
        for (x=0; x<G_DIM_X_LOCAL; x++) begin : tile_x
            tile #(
                .iNOC_VC_CNT         (G_VCS),
                .iNOC_CLD_VC_CNT     (CLD(G_VCS)),
                .iNOC_BUFFER_DEPTH   (G_VC_BUFFER_DEPTH),
                .iNOC_FLIT_SIZE      (G_FLIT_SIZE),
                .iNOC_CTRL_FLIT_SIZE (G_CTRL_FLIT_SIZE),
                .iNoC_TS_CNT         (G_TS),
                .iNoC_MAX_TS         (G_MAX_TS),
                .iNOC_DIM_X          (G_DIM_X),
                .iNOC_DIM_Y          (G_DIM_Y),
                .THIS_X              (fpga_x*2+x),
                .THIS_Y              (fpga_y*2+y),
                .MASTER_XACTOR_ID    (fpga_x*2+x+(fpga_y*2+y)*G_DIM_X),
                .FPGA_ID             (FPGA),
                .MMI64_ID            (32'h00000000 + x*32'h00000001 + y*32'h00000010))
            tile_inst (
            `ifdef SIMUL_WORKAROUND
                .clk_sys (sys_clk_sim),
                .clk_noc (sys_clk_sim),

                .rst_sys_n (sys_rstn_sim),
                .rst_loader_n ( loader_rstn_sim),
                .rst_noc_n (sys_rstn_sim),

                .clk200 (clk200_sim),

                .clk400_p (clk2_p_sim), //: in std_logic; --400MHz clock for DDR
                .clk400_n (clk2_n_sim), //: in std_logic;
            `else
                .clk_sys (sys_clk),
                .clk_noc (sys_clk),

                .rst_sys_n (delay_rstn),
                .rst_loader_n ( delay_rstn),
                .rst_noc_n (delay_rstn),

                .clk200 (clk200),

                `ifdef EN_DDR
                    .clk400_p (clk2_p), //: in std_logic; --400MHz clock for DDR
                    .clk400_n (clk2_n), //: in std_logic;
                `else
                    .clk400_p (1'b0), //: in std_logic; --400MHz clock for DDR
                    .clk400_n (1'b1), //: in std_logic;
                `endif
            `endif

                .clk_gtx (mmi64_clk),
                .rst_ddr_n (ddr_rstn),

                // MMI64
                .mmi64_clk            (mmi64_clk),
                .mmi64_reset          (mmi64_reset),
                .mmi64_h_dn_d_i       (mmi64_router_to_module_dn_d[(1+x+y*G_DIM_X_LOCAL)*64-1:(x+y*G_DIM_X_LOCAL)*64]),
                .mmi64_h_dn_valid_i   (mmi64_router_to_module_dn_valid[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_dn_accept_o  (mmi64_router_to_module_dn_accept[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_dn_start_i   (mmi64_router_to_module_dn_start[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_dn_stop_i    (mmi64_router_to_module_dn_stop[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_up_d_o       (mmi64_router_to_module_up_d[(1+x+y*G_DIM_X_LOCAL)*64-1:(x+y*G_DIM_X_LOCAL)*64]),
                .mmi64_h_up_valid_o   (mmi64_router_to_module_up_valid[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_up_accept_i  (mmi64_router_to_module_up_accept[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_up_start_o   (mmi64_router_to_module_up_start[x+y*G_DIM_X_LOCAL]),
                .mmi64_h_up_stop_o    (mmi64_router_to_module_up_stop[x+y*G_DIM_X_LOCAL]),

                .inoc_data_in (ch_in[x][y][DIR_LOCAL].data_ch.data),
                .inoc_vc_in   (ch_in[x][y][DIR_LOCAL].data_ch.vc),
                .inoc_req_in  (ch_in[x][y][DIR_LOCAL].data_ch.req),
                .inoc_ack_in  (ch_in[x][y][DIR_LOCAL].data_ch.ack),

                .inoc_ctrl_data_in (ch_in[x][y][DIR_LOCAL].ctrl_ch.data),
                .inoc_ctrl_req_in  (ch_in[x][y][DIR_LOCAL].ctrl_ch.req),
                .inoc_ctrl_ack_in  (ch_in[x][y][DIR_LOCAL].ctrl_ch.ack),

                .inoc_data_out (ch_out[x][y][DIR_LOCAL].data_ch.data),
                .inoc_vc_out   (ch_out[x][y][DIR_LOCAL].data_ch.vc),
                .inoc_req_out  (ch_out[x][y][DIR_LOCAL].data_ch.req),
                .inoc_ack_out  (ch_out[x][y][DIR_LOCAL].data_ch.ack),

                .inoc_ctrl_data_out (ch_out[x][y][DIR_LOCAL].ctrl_ch.data),
                .inoc_ctrl_req_out  (ch_out[x][y][DIR_LOCAL].ctrl_ch.req),
                .inoc_ctrl_ack_out  (ch_out[x][y][DIR_LOCAL].ctrl_ch.ack),

                .txd (uart_txd[y][x]),
                .rxd (uart_rxd[y][x]),

                .sram_k_p    (sram_k_p[y][x]),
                .sram_k_n    (sram_k_n[y][x]),
                .sram_a      (sram_a[y][x]),
                .sram_dq     (sram_dq[y][x]),
                .sram_bws_n  (sram_bws_n[y][x]),
                .sram_rnw    (sram_rnw[y][x]),
                .sram_ld_n   (sram_ld_n[y][x]),
                .sram_doff_n (sram_doff_n[y][x]),
                .sram_ready  (sram_ready[y][x]),

                // Router Status Signals
                .router_status_enable      (router_status_req[x][y].enable),
                .router_status_wr_en       (router_status_req[x][y].wr_en),
                .router_status_addr_out    (router_status_req[x][y].addr_out),
                .router_status_data_wr_out (router_status_req[x][y].data_wr_out),
                .router_status_data_rd_in  (router_status_data[x][y]),

                .router_mon_link   (router_mon_link[x][y]),
                .router_mon_vc     (router_mon_vc[x][y]),
                .router_mon_buffer (router_mon_buffer[x][y]),

            `ifdef EN_DDR
                .ddr3_dq      (ddr3_dq),
                .ddr3_dqs_p   (ddr3_dqs_p),
                .ddr3_dqs_n   (ddr3_dqs_n),
                .ddr3_addr    (ddr3_addr),
                .ddr3_ba      (ddr3_ba),
                .ddr3_ras_n   (ddr3_ras_n),
                .ddr3_cas_n   (ddr3_cas_n),
                .ddr3_we_n    (ddr3_we_n),
                .ddr3_reset_n (ddr3_reset_n),
                .ddr3_ck_p    (ddr3_ck_p),
                .ddr3_ck_n    (ddr3_ck_n),
                .ddr3_cke     (ddr3_cke),
                .ddr3_cs_n    (ddr3_cs_n),
                .ddr3_dm      (ddr3_dm),
                .ddr3_odt     (ddr3_odt),
            `else
                .ddr3_dq      (),
                .ddr3_dqs_p   (),
                .ddr3_dqs_n   (),
                .ddr3_addr    (),
                .ddr3_ba      (),
                .ddr3_ras_n   (),
                .ddr3_cas_n   (),
                .ddr3_we_n    (),
                .ddr3_reset_n (),
                .ddr3_ck_p    (),
                .ddr3_ck_n    (),
                .ddr3_cke     (),
                .ddr3_cs_n    (),
                .ddr3_dm      (),
                .ddr3_odt     (),
            `endif
                .ddr3_calib_done (w_ddr3_calib_done[y][x]),

            `ifdef EN_ETH
                .eth_clk_to_mac        (eth_clk_to_mac),
                .eth_col_clk_mac_freq  (eth_col_clk_mac_freq),
                .eth_crs_rgmii_sel0    (eth_crs_rgmii_sel0),
                .eth_gtx_clk_tck       (eth_gtx_clk_tck),
                .eth_mdc               (eth_mdc),
                .eth_mdio              (eth_mdio),
                .eth_ninterrupt        (eth_ninterrupt),
                .eth_nreset            (eth_nreset),
                .eth_rx_clk            (eth_rx_clk),
                .eth_rx_dv_rck         (eth_rx_dv_rck),
                .eth_rx_er_rxdv_er     (eth_rx_er_rxdv_er),
                .eth_rxd               (eth_rxd),
                .eth_tx_clk_rgmii_sel1 (eth_tx_clk_rgmii_sel1),
                .eth_tx_en_txen_er     (eth_tx_en_txen_er),
                .eth_tx_er             (eth_tx_er),
                .eth_txd               (eth_txd),
            `else
                .eth_clk_to_mac        (),
                .eth_col_clk_mac_freq  (),
                .eth_crs_rgmii_sel0    (),
                .eth_gtx_clk_tck       (),
                .eth_mdc               (),
                .eth_mdio              (),
                .eth_ninterrupt        (),
                .eth_nreset            (),
                .eth_rx_clk            (),
                .eth_rx_dv_rck         (),
                .eth_rx_er_rxdv_er     (),
                .eth_rxd               (),
                .eth_tx_clk_rgmii_sel1 (),
                .eth_tx_en_txen_er     (),
                .eth_tx_er             (),
                .eth_txd               (),
            `endif

                .lock (lock_o[x][y])
            );

            assign coord_x[x][y] = fpga_x*2 + x;
            assign coord_y[x][y] = fpga_y*2 + y;

            for (z=0; z<G_PORTS; z++) begin
                assign conf_port[x][y][z] = 0;  // the configuration port is currently not used
            end

            // router only if bigger than 1x1
            if ((G_DIM_X > 1) || (G_DIM_Y > 1)) begin
                router #(
                    .port_cnt  (G_PORTS),
                    .vc_cnt    (G_VCS),
                    .ts_cnt    (G_TS),
                    .max_ts    (G_MAX_TS),
                    .dim_x     (G_DIM_X),
                    .dim_y     (G_DIM_Y),
                    .buf_depth (G_VC_BUFFER_DEPTH))
                router (
                `ifdef SIMUL_WORKAROUND
                    .clk   (sys_clk_sim),
                    .rst_n (loader_rstn_sim),
                `else
                    .clk   (sys_clk),
                    .rst_n (delay_rstn),
                `endif
                    .ch_in              (ch_in[x][y]),
                    .ch_out             (ch_out[x][y]),
                    .router_status_data (router_status_data[x][y]),
                    .router_status_req  (router_status_req[x][y]),
                    .this_coord_x       (coord_x[x][y]),
                    .this_coord_y       (coord_y[x][y]),
                    .conf_port_in       (conf_port[x][y]),
                    .port_enable        (port_enable[x][y]),
                    .mon_data_out       (router_mon_data[y][x])
                );

                for (i=0; i<G_PORTS; i++) begin
                    assign router_mon_link[x][y][i] = router_mon_data[x][y][i].link;
                    assign router_mon_vc[x][y][(i+1)*CLD(G_VCS+1)-1 : i*CLD(G_VCS+1)] = router_mon_data[x][y][i].vc;
                    for (j=0; j<G_VCS; j++) begin
                        assign router_mon_buffer[x][y][i*G_VCS*CLD(G_VC_BUFFER_DEPTH+1)+(j+1)*CLD(G_VC_BUFFER_DEPTH+1)-1 : i*G_VCS*CLD(G_VC_BUFFER_DEPTH+1)+j*CLD(G_VC_BUFFER_DEPTH+1)] = router_mon_data[x][y][i].buffer[j];
                    end
                end
            end
        end
    end
    endgenerate

    generate
    for (x=0; x<G_DIM_X_LOCAL; x++) begin : router_port_x
        for (y=0; y<G_DIM_Y_LOCAL; y++) begin : router_port_y
            assign port_enable[x][y][DIR_LOCAL] = 1;
            if (x > 0) begin
                assign ch_in[x][y][DIR_WEST] = ch_out[x-1][y][DIR_EAST];
                assign port_enable[x][y][DIR_WEST] = 1;
            end
            if (x < G_DIM_X_LOCAL-1) begin
                assign ch_in[x][y][DIR_EAST] = ch_out[x+1][y][DIR_WEST];
                assign port_enable[x][y][DIR_EAST] = 1;
            end
            if (y > 0) begin
                assign ch_in[x][y][DIR_NORTH] = ch_out[x][y-1][DIR_SOUTH];
                assign port_enable[x][y][DIR_NORTH] = 1;
            end
            if (y < G_DIM_Y_LOCAL-1) begin
                assign ch_in[x][y][DIR_SOUTH] = ch_out[x][y+1][DIR_NORTH];
                assign port_enable[x][y][DIR_SOUTH] = 1;
            end
        end
    end
    endgenerate

`ifndef SIMUL_WORKAROUND
    ch_t [1:0] inoc_out_muxa;
    ch_t [1:0] inoc_in_muxa;
    ch_t [1:0] inoc_out_muxb;
    ch_t [1:0] inoc_in_muxb;

    muxComm #(
        .NOC_CHANNELS (2),
        .TX_PINS (TX_PINS),
        .RX_PINS (RX_PINS))
    muxCommA (
        .clk      (clk_mux),
        .mux_mode (mux_mode),
        .inoc_out (inoc_out_muxa),
        .inoc_in  (inoc_in_muxa),
        .tx_pin   (tx_pin),
        .rx_pin   (rx_pin)
    );

    muxComm #(
        .NOC_CHANNELS (2),
        .TX_PINS (TX_PINS),
        .RX_PINS (RX_PINS))
    muxCommB (
        .clk      (clk_mux),
        .mux_mode (mux_mode),
        .inoc_out (inoc_out_muxb),
        .inoc_in  (inoc_in_muxb),
        .tx_pin   (tx2_pin),
        .rx_pin   (rx2_pin)
    );

    if (FPGA == TA1) begin
        assign inoc_in_muxa = {ch_out[0][1][DIR_SOUTH], ch_out[1][1][DIR_SOUTH]};
        assign inoc_in_muxb = {ch_out[0][1][DIR_WEST],  ch_out[0][0][DIR_WEST]};
        assign {ch_in[0][1][DIR_SOUTH], ch_in[1][1][DIR_SOUTH]} = inoc_out_muxa;
        assign {ch_in[0][1][DIR_WEST],  ch_in[0][0][DIR_WEST]}  = inoc_out_muxb;
    end else if (FPGA == TA3) begin
        assign inoc_in_muxa = {ch_out[1][1][DIR_SOUTH], ch_out[0][1][DIR_SOUTH]};
        assign inoc_in_muxb = {ch_out[1][1][DIR_EAST],  ch_out[1][0][DIR_EAST]};
        assign {ch_in[1][1][DIR_SOUTH], ch_in[0][1][DIR_SOUTH]} = inoc_out_muxa;
        assign {ch_in[1][1][DIR_EAST],  ch_in[1][0][DIR_EAST]}  = inoc_out_muxb;
    end else if (FPGA == TC1) begin
        assign inoc_in_muxa = {ch_out[0][0][DIR_NORTH], ch_out[1][0][DIR_NORTH]};
        assign inoc_in_muxb = {ch_out[0][0][DIR_WEST],  ch_out[0][1][DIR_WEST]};
        assign {ch_in[0][0][DIR_NORTH], ch_in[1][0][DIR_NORTH]} = inoc_out_muxa;
        assign {ch_in[0][0][DIR_WEST],  ch_in[0][1][DIR_WEST]}  = inoc_out_muxb;
    end else if (FPGA == TC3) begin
        assign inoc_in_muxa = {ch_out[1][0][DIR_NORTH], ch_out[0][0][DIR_NORTH]};
        assign inoc_in_muxb = {ch_out[1][0][DIR_EAST],  ch_out[1][1][DIR_EAST]};
        assign {ch_in[1][0][DIR_NORTH], ch_in[0][0][DIR_NORTH]} = inoc_out_muxa;
        assign {ch_in[1][0][DIR_EAST],  ch_in[1][1][DIR_EAST]}  = inoc_out_muxb;
    end
`endif

    for (x=0; x<G_DIM_X_LOCAL; x++) begin : router_unconnected_x
        for (y=0; y<G_DIM_Y_LOCAL; y++) begin : router_unconnected_y
            //assign signals of unconnected ports to 0
            if (fpga_x == 0 && x == 0)
                assign ch_in[x][y][DIR_WEST] = 0;
            if (fpga_x == 1 && x == G_DIM_X_LOCAL-1)
                assign ch_in[x][y][DIR_EAST] = 0;
            if (fpga_y == 0 && y == 0)
                assign ch_in[x][y][DIR_NORTH] = 0;
            if (fpga_y == 1 && y == G_DIM_Y_LOCAL-1)
                assign ch_in[x][y][DIR_SOUTH] = 0;

            //set port_enable for ports connected over muxComm
            if (fpga_x == 0 && x == G_DIM_X_LOCAL-1)
                assign port_enable[x][y][DIR_EAST] = 1;
            if (fpga_x == 1 && x == 0)
                assign port_enable[x][y][DIR_WEST] = 1;
            if (fpga_y == 0 && y == G_DIM_Y_LOCAL-1)
                assign port_enable[x][y][DIR_SOUTH] = 1;
            if (fpga_y == 1 && y == 0)
                assign port_enable[x][y][DIR_NORTH] = 1;

            // for single-fpga designs, disconnect ports at all outer borders
            if (G_DIM_X == G_DIM_X_LOCAL && x == G_DIM_X_LOCAL-1)
                assign ch_in[x][y][DIR_EAST] = 0;
            if (G_DIM_Y == G_DIM_Y_LOCAL && y == G_DIM_Y_LOCAL-1)
                assign ch_in[x][y][DIR_SOUTH] = 0;
        end
    end

    assign lock = lock_o[1][1];
endmodule

module dff_sync_reset (
    input  wire data,
    input  wire clk,
    input  wire reset,
    output reg  q
);
    always_ff @(posedge clk)
    if (~reset) begin
        q <= 1'b0;
    end else begin
        q <= data;
    end
endmodule
