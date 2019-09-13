/*-----------------------------------------------------------------
--
-- Design:     Global Paramters
--
-- File:       global_par.sv
--
-- Abstract:   Global Paramters for all System Verilog Modules
--
-- Dependency: -
--
-------------------------------------------------------------------*/
//GLOBALS

`define EN_DDR
`define EN_ETH

package parameters;

parameter logic					G_USE_DW = 0;

// Dimensions
parameter int unsigned			G_DIM_X = 2;
parameter int unsigned			G_DIM_Y = 2;

//On-tile dimensions
parameter int unsigned			G_DIM_X_LOCAL = 2;
parameter int unsigned			G_DIM_Y_LOCAL = 2;

//Flits
parameter int unsigned			G_FLIT_SIZE = 32; //in Bits (corresponds to the datawidth of the physical connection)

//Ports
parameter int unsigned			G_PORTS = 5;

//Virtual Channels
parameter int unsigned			G_VCS = 4;

parameter logic					G_VC_POL_ENABLE = 0;  //controls the vc resource management policy (rerouting?)

//Time Slots
parameter int unsigned			G_TS = 8;
parameter int unsigned			G_MAX_TS = 5; //Max number of reservable TS per connection (ATTENTION: G_MAX_TS=4 would allow to reserve 3 TS per connection)

//Buffer
parameter int unsigned			G_VC_BUFFER_DEPTH = 4; //in Flits

//Internal Parameters
parameter logic					G_FAST_RESERVATION = 0; //New Header Flits are forwarded from the input to the reservation unit (might reduce the clock frequency)
parameter logic					G_MULTI_PKG_PER_BUFFER = 1;  //Allow to store parts of different pkg in one buffer at the same time (set to 1 not tested and supported)

//Pipelining Parameters
parameter logic					G_OUTPUT_REG = 1;     //Fifth stage of the pipeline. Use Registers at the outputs. Can be disabled to have shorter pipeline. needed for stability in 4x4
parameter logic					G_PIPELINE_RES = 0;   //Second stage of the pipeline. Pipelining the routing process. Can be disabled to have shorter pipeline.
parameter logic					G_PIPELINE_SEL = 0;   //Fourth Stage of the pipeline. The flit selection process. Can be disabled to have shorter pipeline.
parameter int					G_OPS_COUNTERS = 0;   //Use fill level counters inside OPS unit to improve timing (0: disabled, 1: version with less stall cycles, 2: version with higher clock frequency, but higher delay(?))

//Parameters for the Control Channel NoC
parameter logic					G_CTRL_ENABLE = 0;
parameter int unsigned			G_CTRL_FLIT_SIZE = 16; //in Bits (corresponds to the datawidth of the pysical connection) => should not be lower than 16 bit, because of ctrl_fsm implementation
parameter int unsigned			G_CTRL_BUFFER_DEPTH = 2; //in Flits

parameter logic					G_SIMPLE_CTRL_ENABLE = 0;
parameter int unsigned			G_CTRL_ARB_SIZE = 4; // number of entities which use the ctrl channel; used for arbitration size

//Parameters for the Rerouting
parameter logic					G_REROUTING_ENABLE = 0;

//Monitoring Parameters
parameter logic					G_ENABLE_MON = 0;  //Enable monitoring access via control channel

parameter int					G_REGISTER_SIZE = 32;
parameter int					G_MON_MSG_ENABLE = 1; //enables the monitoring-messages coming from the testbench
parameter int					G_NO_OF_HISTORIES = 2;

parameter logic					G_ENABLE_RVC_DET = 1; //to enable link monitoring
parameter logic					G_ENABLE_BFL_DET = 1;
parameter logic					G_ENABLE_OPR_DET = 1;

parameter logic					G_ENABLE_RVC_TX = 1; // -"- from res_vc_cnt
parameter logic					G_ENABLE_BFL_TX = 1; // -"- from buf_fill_level
parameter logic					G_ENABLE_OPR_TX = 1; //enable the transfer of history-values from out_port_req using the control channel


parameter int					G_TRIGGER_PERIOD = 100000/*ns*/;
parameter int					G_CLK_PERIOD = 1/*ns*/;
parameter int					G_TX_DATA_PRECISION = 32;   //if G_TX_DATA_PRECISION<G_HISTORY_PRECISION the lower bits of the mon_data will be cut
								//if G_TX_DATA_PRECISION >= G_HISTORY_PRECISION then G_TX_DATA_PRECISION will be set to the value of G_HISTORY_PRECISION to avoid sending redundant data over the control channel

//Monitoring out_port_req
parameter int unsigned			G_NO_OF_VALUES_OPR = 1<<26;     // number of values added to cache
parameter int unsigned			G_HISTORY_PRECISION_OPR = 32;  //bit-width of stored cache values
parameter int unsigned			G_HISTORY_LENGTH_OPR = 2;

//Monitoring buf_fill_level
parameter int unsigned			G_NO_OF_VALUES_BFL = 1<<26;     // number of values added to cache
parameter int unsigned			G_HISTORY_PRECISION_BFL = 32;  //bit-width of stored cache values
parameter int unsigned			G_HISTORY_LENGTH_BFL = 2;

//Monitoring res_vc_cnt  (res_vc_cnt = gs_vc_cnt + be_vc_cnt)
parameter int unsigned			G_NO_OF_VALUES_RVC = 1<<26;     // number of values added to cache
parameter int unsigned			G_HISTORY_PRECISION_RVC = 32;  //bit-width of stored cache values
parameter int unsigned			G_HISTORY_LENGTH_RVC = 2;

//TX Cyclic Monitoring Data - Router Destination
parameter logic					G_TX_TO_LOCAL_ROUTER = 1;   //if set the cyclic monitoring data are sent to the local router, if reset they are sent to G_TX_TO_DST_X, G_TX_TO_DST_Y
parameter int unsigned			G_TX_TO_DST_X = 0;
parameter int unsigned			G_TX_TO_DST_Y = 0;

//Memory Map
parameter int unsigned			G_EN_MEM_MAP_ASGN_MSG = 0;
parameter int unsigned			G_BFL_MAP_PREC = 8;     //precision of the bfl-history-values in the memory-map
parameter int unsigned			G_BFL_PEAK_MAP_PREC = 8;        //precision of bfl_peak_values in the memory-map
parameter int unsigned			G_RVC_MAP_PREC = 8;     //precision of the rvc-history-values in the memory-map
parameter int unsigned			G_RVC_PEAK_MAP_PREC = 8;        //precision of rvc_peak_values in the memory-map
parameter int unsigned			G_OPR_MAP_PREC = 8;     //precision of the opr-history-values in the memory-map

//Dedicated virtual channel for response message to avoid message dependent deadlock
//parameter logic					G_VC_IN_LINE = 0;     // ensure packet order in iNoC. Not tested for 4x4. Disabled because ordering is taken care of in NA. 
//parameter logic					G_PKT_ORDER_TABLE = 0;     // ensure packet order in iNoC using table based. Note that default method (0) does not scale

parameter logic					G_SPLITVC = 1;        // Split virtual channel feature. dedicated VCO for response. Packets injected in VC0, remains in VC0 in the iNoC
parameter logic					G_FIXEDTS = 0;        // to enable fixed timeslot based round robin arbitration

parameter logic					G_IN_ORDER = 0;
parameter logic					G_CTRL_SERIAL = 0;
parameter int					G_CTRL_SER_SIZE = 5;

parameter logic					G_ENABLE_DEBUGGING = 1;  // enables access to router debug registers

// parameter for ddr
parameter int					DDR_X_LOC = 1;
parameter int					DDR_Y_LOC = 1;
parameter int					nCS_PER_RANK = 1;
parameter int					BANK_WIDTH = 3;
parameter int					CK_WIDTH = 1;
parameter int					CKE_WIDTH = 1;
parameter int					COL_WIDTH = 10;
parameter int					CS_WIDTH  = 1;
parameter int					DM_WIDTH  = 8;
parameter int					DQ_WIDTH  = 64;
parameter int					DQS_WIDTH = 8;
parameter int					ROW_WIDTH = 14;
parameter int					PAYLOAD_WIDTH = 64;
parameter int					ADDR_WIDTH = 28;

endpackage
