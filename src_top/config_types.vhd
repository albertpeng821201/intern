library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library techmap;
use techmap.gencomp.all;

library tcpa_lib;
use tcpa_lib.AG_BUFFER_type_lib.all;

-- Technology and synthesis options
package config_types is

  constant dw                 : integer := 32;
  constant TILE_ID_WIDTH      : integer := 8;
  -- DECLARATIONS

  type debug_ina_type is record
	  backtrace 	: integer;
	  debug_regs	: integer;
	  measurement	: integer;
  end record;

  type debug_noc_type is record
	router_status : integer;
  end record;

  type debug_type is record
	  ina	: debug_ina_type;
	  noc 	: debug_noc_type;
  end record;

	type config_sharq_type is record
		en			: std_ulogic;
		dummy		: std_ulogic;
	end record;

	type config_enemy_type is record
		cache		: std_ulogic;
		meta_cache	: std_ulogic;
		tlm_en		: std_ulogic;
		ddr_en		: std_ulogic;
		debug		: std_ulogic;
		perf		: std_ulogic;
		hmap_en		: std_ulogic;
	end record;

	type nma_config_type is record
		sharq	: config_sharq_type;
		enemy	: config_enemy_type;
	end record;

	type t_memory_map_parameters is record
		SHM_start		: std_logic_vector(dw-1 downto 0);
		SHM_TWIN_start	: std_logic_vector(dw-1 downto 0);
		SHM_end			: std_logic_vector(dw-1 downto 0);
		SHM_haddr		: integer;
		SHM_hmask		: integer;
		SHM_TWIN_haddr	: integer;
		SHM_TWIN_hmask	: integer;
		SHM_IO			: integer;
		TLM_start		: std_logic_vector(dw-1 downto 0);
		TLM_end			: std_logic_vector(dw-1 downto 0);
		TLM_haddr		: integer;
		TLM_hmask		: integer;
		RMAP_start		: std_logic_vector(dw-1 downto 0);
		RMAP_offset		: std_logic_vector(dw-1 downto 0);
		RMAP_haddr		: integer;
		RMAP_hmask		: integer;

		LOCAL_start		: std_logic_vector(dw-1 downto 0);
		LOCAL_end		: std_logic_vector(dw-1 downto 0);

		INA_start		: std_logic_vector(dw-1 downto 0);
		INA_haddr		: integer;
		INA_hmask		: integer;

		ENEMY_TLM_start	: std_logic_vector(dw-1 downto 0);
		ENEMY_DDR_start	: std_logic_vector(dw-1 downto 0);
		ENEMY_TLM_haddr	: integer;
		ENEMY_DDR_haddr	: integer;
		ENEMY_hmask		: integer;

		APB_HADDR		: integer;
		CRM_HADDR		: integer;

		DSU_HADDR		: integer;
		DSU_HMASK		: integer;

		HELPER_TLM_start: std_logic_vector(dw-1 downto 0);
		HELPER_TLM_HADDR: integer;
		HELPER_TLM_HMASK: integer;
		HELPER_TLM_ROMADDR: integer;
		HELPER_TLM_ROMMASK: integer;
		HELPER_TLM_IOADDR: integer;
		HELPER_TLM_IOMASK: integer;

		AHB_1_IO		: integer;

		L2C_CTRL_0_HADDR: integer;
		L2C_CTRL_0_HMASK: integer;
		L2C_CTRL_1_HADDR: integer;
		L2C_CTRL_1_HMASK: integer;
		L2C_CTRL_2_HADDR: integer;
		L2C_CTRL_2_HMASK: integer;
		L2C_RANGE_HADDR	: integer;
		L2C_RANGE_HMASK	: integer;

		L2C_CTRL_0_start: std_logic_vector(dw-1 downto 0);
		L2C_RANGE_start	: std_logic_vector(dw-1 downto 0);

		ICORE_MMIO_HADDR		: integer;
		ICORE_VLCCWSTO_HADDR	: integer;
		ICORE_BIT_HADDR			: integer;
		ICORE_B1DMA_HADDR		: integer;
		ICORE_TLM_HADDR			: integer;
	end record;

  type t_architecture_parameters is record
	-----------------------------------------------------------------------------
	-- architecture
	-----------------------------------------------------------------------------

	maxahbm : integer;
	maxahbs : integer;
	maxapb  : integer;

	leon3               : integer;
	en_na               : integer;
	na_ls               : integer;
	na_mp               : integer;
	profpga_ssram       : integer;
	ahbramen            : integer;
	sim                 : integer;
	mctrl_leon2         : integer;
	en_ddr              : integer;
	en_dvi2ahb          : integer;
	en_pendulum_control : integer;
	en_beeper           : integer;
	icore               : integer;
	b1dma_enable        : integer;
	tcpa                : integer;
	ahb_uart            : integer;
	ahb_jtag            : integer;
	ahbstat             : integer;
	uart1_enable        : integer;
	irq3_enable         : integer;
	gpt_enable          : integer;
	i2c_enable          : integer;
	grgpio_enable       : integer;
	kbd_enable          : integer;
	vga_enable          : integer;
	svga_enable         : integer;
	grsysmon            : integer;
	gracectrl           : integer;
	pciexp              : integer;
	natlas              : integer;
	en_amba_monitor     : integer;
	en_eth              : integer;

	umr_data_bitwidth : integer;

	ahbuart_pindex : integer;
	apbuart_pindex : integer;
	irqmp_pindex   : integer;
	gptimer_pindex : integer;
	ahbstat_pindex : integer;
	mctrl_pindex   : integer;
	ddr_pindex     : integer;
	gc_pindex      : integer;
	cm_pindex      : integer;
	rr_pindex      : integer;
	fi_pindex      : integer;
	dvi_pindex     : integer;
	pwm_pindex     : integer;
	beeper_pindex  : integer;
	eth_pindex     : integer;
	unused_pindex  : integer;

	-- fsb ahbmo indices
	-- 0-(ncpu-1) reserved for cpus!
	ncpu                  : integer;
	na_mst_rx_idx         : integer;
	na_mst_tx_idx         : integer;
	mmi64_ahbm_idx        : integer;
	eth_mindex            : integer;
	ahb_uart_m_idx        : integer;
	icore_bitload_hindexm : integer;
	b1dma_hindexm         : integer;
	crm_mst_idx           : integer;
	loader_mst_idx        : integer;
	enemy_tlm_mst_idx	  : integer;
	enemy_ddr_mst_idx	  : integer;
	l2c_reverse_mst_idx	  : integer;
	unused_ahbmindex      : integer;

	-- fsb ahbso indices
	--
	apb_hindex    : integer;
	dsu_hindex    : integer;
	na_hindex     : integer;
	ssram_hindex  : integer;
	sim_hindex	  : integer;
	ddr_hindex    : integer;
	l2_hindex     : integer;
	amp_hindex    : integer;
	ram_hindex    : integer;
	cic_hindex    : integer;
	icore_tlm_hindexs        : integer;
	b1dma_hindexs            : integer;
	icore_bitload_hindexs    : integer;
	icore_mi_hindexs         : integer;
	icore_vlcwsto_hindexs    : integer;
	tcpa_buffer_north_hindex : integer;
	tcpa_buffer_west_hindex  : integer;
	tcpa_buffer_south_hindex : integer;
	tcpa_buffer_east_hindex  : integer;
	tcpa_rbuffer_hindex      : integer;
	dvi_hindex               : integer;
	crm_hindex               : integer;
	enemy_tlm_hindex		 : integer;
	unused_ahbsindex : integer;

	-- na ahb
	l2c_hindexm   : integer;

	fabtech   : integer;
	memtech   : integer;
	padtech   : integer;
	transtech : integer;
	noasync   : integer;
	scan      : integer;

  end record t_architecture_parameters;

  type t_clkgen_parameters is record
	-- clock generator
	clktech   : integer;
	clkmul    : integer;
	clkdiv    : integer;
	oclkdiv   : integer;
	oclkbdiv  : integer;
	oclkcdiv  : integer;
	pcidll    : integer;
	pcisysclk : integer;
	clk_nofb  : integer;

	-- clock generator for noc
	clkmuln   : integer;
	clkdivn   : integer;
  end record t_clkgen_parameters;

  type t_leon_parameters is record
	-- leon3 processor core
	nwin             : integer;
	v8               : integer;
	mac              : integer;
	bp               : integer;
	svt              : integer;
	rstaddr          : integer;
	lddel            : integer;
	notag            : integer;
	nwp              : integer;
	pwd              : integer;
	fpu              : integer;
	grfpush          : integer;
	icen             : integer;
	isets            : integer;
	isetsz           : integer;
	iline            : integer;
	irepl            : integer;
	ilock            : integer;
	ilramen          : integer;
	ilramaddr        : integer;
	ilramsz          : integer;
	dcen             : integer;
	dsets            : integer;
	dsetsz           : integer;
	dline            : integer;
	drepl            : integer;
	dlock            : integer;
	dsnoop           : integer;
	dfixed           : integer;
	dlramen          : integer;
	dlramaddr        : integer;
	dlramsz          : integer;
	mmuen            : integer;
	itlbnum          : integer;
	dtlbnum          : integer;
	tlb_type         : integer;
	tlb_rep          : integer;
	mmu_page         : integer;
	dsu              : integer;
	itbsz            : integer;
	atbsz            : integer;
	ahbpf            : integer;
	leon3ft_en       : integer;
	iuft_en          : integer;
	fpuft_en         : integer;
	rf_errinj        : integer;
	cache_ft_en      : integer;
	cache_errinj     : integer;
	leon3_netlist    : integer;
	disas            : integer;
	pclow            : integer;
	np_asi           : integer;
	wrpsr            : integer;
	en_addr_trans    : integer;
	RCAS_EN			 : integer;
	-- cpu freq
	board_freq_200 : integer;
	board_freq     : integer;
	vco_freq       : integer;
	cpu_freq       : integer;

  end record t_leon_parameters;

  type t_amba_parameters is record
	-- amba settings
	defmst           : integer;
	rrobin           : integer;
	split            : integer;
	fpnpen           : integer;
	ahbio            : integer;
	apbaddr          : integer;
	ahb_mon          : integer;
	ahb_monerr       : integer;
	ahb_monwar       : integer;
	ahb_dtrace       : integer;
	en_ahb2ahb       : integer;
	en_async_na      : integer;
  end record t_amba_parameters;

  type t_ethdsu_parameters is record
	-- ethernet dsu
	dsu_eth          : integer;
	eth_buf          : integer;
	eth_ipm          : integer;
	eth_ipl          : integer;
	eth_enm          : integer;
	eth_enl          : integer;
  end record t_ethdsu_parameters;

  type t_mem_parameters is record
	new_ssram	     : integer;
	bram_tlm		 : integer;
	sram_addr_w      : integer;
	sram_dq_pins     : integer;
	sram_groups      : integer;
	-- tlm
	mctrl_paddr      : integer;
	tlm_start_addr   : integer;
	tlm_end_addr     : integer;
	tlm_mask		 : integer;
	-- ahb rom
	ahbropip         : integer;
	ahbroddr         : integer;
	romaddr          : integer;
	rommask          : integer;
	-- ahb ram
	ram_size         : integer;
	ram_haddr        : integer;
	ahbrpipe         : integer;
	-- leon2 memory controller
	mctrl_ram8bit    : integer;
	mctrl_ram16bit   : integer;
	mctrl_5cs        : integer;
	mctrl_sden       : integer;
	mctrl_sepbus     : integer;
	mctrl_invclk     : integer;
	mctrl_sd64       : integer;
	mctrl_page       : integer;
  end record t_mem_parameters;

  type t_ddr_parameters is record
	-- ddr controller
	ddr_addr   : integer;
	ddr_mask   : integer;
	ddr_bus_io : integer;
	ddr_paddr  : integer;
  end record t_ddr_parameters;

  type t_ahbstat_parameters is record
	-- ahb status register
	ahbstat_paddr  : integer;
	ahbstat_pirq   : integer;
	ahbstatn       : integer;
  end record t_ahbstat_parameters;

  type t_geth_parameters is record
	-- gaisler ethernet core
	paddr          : integer;
	pmask          : integer;
	pirq           : integer;

	ifg_gap        : integer;
	attempt_limit  : integer;
	backoff_limit  : integer;
	slot_time      : integer;
	mdcscaler      : integer;
	enable_mdio    : integer;
	fifosize       : integer;
	nsync          : integer;
	edcl           : integer;
	edclbufsz      : integer;
	burstlength    : integer;
	macaddrh       : integer;
	macaddrl       : integer;
	ipaddrh        : integer;
	ipaddrl        : integer;
	phyrstadr      : integer;
	rmii           : integer;
	sim            : integer;
	giga           : integer;
	oepol          : integer;
	scanen         : integer;
	ft             : integer;
	edclft         : integer;
	mdint_pol      : integer;
	enable_mdint   : integer;
	multicast      : integer;
	ramdebug       : integer;
	mdiohold       : integer;
	maxsize        : integer;
	gmiimode       : integer;

	greth          : integer;
	greth1g        : integer;
	eth_fifo       : integer;
  end record t_geth_parameters;

  type t_uart_parameters is record
	-- uart
	ahbuart_paddr  : integer;
	apbuart_paddr  : integer;
	apbuart_pirq   : integer;
	uart1_fifo     : integer;
	duart          : integer;
  end record t_uart_parameters;

  type t_irq_parameters is record
	-- leon3 interrupt controller
	irqmp_paddr : integer;
	irq3_nsec   : integer;
  end record t_irq_parameters;

  type t_timer_parameters is record
	-- modular timer
	gptimer_paddr  : integer;
	gpt_ntim   : integer;
	gpt_sw     : integer;
	gpt_tw     : integer;
	gpt_irq    : integer;
	gpt_sepirq : integer;
	gpt_wdogen : integer;
	gpt_wdog   : integer;
  end record t_timer_parameters;

  type t_gpio_parameters is record
	-- gpio port
	grgpio_imask  : integer;
	grgpio_width  : integer;
  end record t_gpio_parameters;

  type t_pcie_parameters is record
	-- pciexp interface
	pcie_type    : integer;
	pcie_sim_mas : integer;
	pciexpvid    : integer;
	pciexpdid    : integer;
	no_of_lanes  : integer;
  end record t_pcie_parameters;

  type t_na_parameters is record
	-- network adapter load store slave
	narsz       : integer;
	naaddr_ls   : integer;
	namask_ls   : integer;
	-- burst size from na perspective, should be same as l2 cache controller
	burst_size      : integer;
	-- this enables split support. if its 0, then bus will be blocked until response is received from the destination
	na_split_enable : integer;
	--number of tile masters supported (parameterisable) for load/store in the current implementation
	ntilemasters    : integer;

	-- network adapter message passing slave
	naaddr_mp : integer;
	namask_mp : integer;
	-- network adapter irq
	na_irq    : integer;-- 11;
	-- cic i-let enqueue addresses (currently dummy tlm locations)
	cic_control_addr   : std_logic_vector(dw-1 downto 0);
	cic_enqueue_addr   : std_logic_vector(dw-1 downto 0);
	-- reset address to write system i-let
	sys_ilet_dst_addr  : std_logic_vector(dw-1 downto 0);
	-- parameters for address lookup
	tile_id_subtrahend : std_logic_vector(tile_id_width-1 downto 0);
	address_offset     : std_logic_vector(tile_id_width-1 downto 0);
	mon_global_addr    : std_logic_vector(tile_id_width-1 downto 0);
	mon_address_offset : std_logic_vector(tile_id_width-1 downto 0);

  ahb_addr_bar0  : integer;
  ahb_addr_bar1  : integer;
  ahb_addr_bar2  : integer;
  ahb_addr_bar3  : integer;
  ahb_addr_bar4  : integer;
  ahb_para0_bar0 : std_logic;
  ahb_para0_bar1 : std_logic;
  ahb_para0_bar2 : std_logic;
  ahb_para0_bar3 : std_logic;
  ahb_para0_bar4 : std_logic;
  ahb_para1_bar0 : std_logic;
  ahb_para1_bar1 : std_logic;
  ahb_para1_bar2 : std_logic;
  ahb_para1_bar3 : std_logic;
  ahb_para1_bar4 : std_logic;
  ahb_mask_bar0  : integer;
  ahb_mask_bar1  : integer;
  ahb_mask_bar2  : integer;
  ahb_mask_bar3  : integer;
  ahb_mask_bar4  : integer;
  end record t_na_parameters;

-- CRM Parameters
  type t_crm_parameters is record
	crm_en			: integer;
	crm_ahbs_ioaddr : integer;
	crm_ahbs_iomask : integer;
	burst_gen_en	: integer;
  end record t_crm_parameters;

  type t_icore_parameters is record
	-- permutator
	perm : integer;

	-- tlm address and size
	tlm_enable : integer;
	tlm_addr   : integer;
	tlm_size   : integer;

	--mmio-ahb-interface
	mmio_addr : integer;

	--irq generator
	irqgen_enable : integer;

	--bitstream loader
	bitloader_enable : integer;

	--b1 dma controller
	b1dma_addr   : integer;
	b1dma_irq    : integer;

	-- i-core ahb adresses & masks
	icore_bitload_haddr : integer;
	icore_bitload_hmask : integer;
	icore_vlcwsto_haddr : integer;
	icore_vlcwsto_hmask : integer;
  end record t_icore_parameters;


  type t_tcpa_parameters is record
	NUM_OF_BUFFER_STRUCTURES              : positive range 1 to 4;
	BUFFER_SIZE                           : integer;
	BUFFER_SIZE_ADDR_WIDTH                : integer;
	BUFFER_CHANNEL_SIZE                   : integer;
	BUFFER_CHANNEL_ADDR_WIDTH             : integer;
	BUFFER_CHANNEL_SIZES_ARE_POWER_OF_TWO : boolean;
	EN_ELASTIC_BUFFER                     : boolean;
	AG_BUFFER_CONFIG_SIZE                 : integer;

	ag_buffer_north : t_ag_buffer_generics;
	ag_buffer_west  : t_ag_buffer_generics;
	ag_buffer_south : t_ag_buffer_generics;
	ag_buffer_east  : t_ag_buffer_generics;

	rbuffer_hirq_ahb_addr : integer;
	rbuffer_hirq_ahb_mask : integer;
	rbuffer_hirq_ahb_irq  : integer;

	gc_paddr  : integer;
	gc_pmask  : integer;
	gc_pirq   : integer;

	cm_paddr  : integer;
	cm_pmask  : integer;

	rr_paddr  : integer;
	rr_pmask  : integer;

	fi_paddr  : integer;
	fi_pmask  : integer;
	fi_pirq   : integer;

	index_vector_dimension   : integer range 0 to 32;
	index_vector_data_width  : integer range 0 to 32;
	matrix_pipeline_depth    : integer range 0 to 32;

	iteration_variable_width : integer;
	dimension                : integer;
	select_width             : integer;
	no_reg_to_program        : integer;
	matrix_element_width     : integer;
	data_width               : integer;
	max_no_of_program_blocks : integer;
	num_of_ic_signals        : integer;
  end record t_tcpa_parameters;

  type t_dvi_parameters is record
	dvi_haddr : integer;
	dvi_hirq  : integer;
	dvi_paddr : integer;
	dvi_pmask : integer;
	pwm_paddr : integer;
	pwm_pmask : integer;

	beeper_paddr         : integer;
	beeper_counter_width : integer;
	beeper_divider_width : integer;
  end record t_dvi_parameters;

  type t_dbg_parameters is record
	en_ila			: integer;
	en_ila_icrm		: integer;
	en_ila_iNA_RX	: integer;
	en_ila_ahb2mig	: integer;
	en_eth_ila		: integer;
	en_ila_ENEMY_DDR: integer;
	en_ila_GRAPH_WB	: integer;
	en_ila_fifo2bus	: integer;
	en_ila_bus2fifo	: integer;
	en_ila_hwldma	: integer;
  end record t_dbg_parameters;

  type t_tile_config is record
	arch   : t_architecture_parameters;
	mmap   : t_memory_map_parameters;
	clkgen : t_clkgen_parameters;
	leon   : t_leon_parameters;
	amba   : t_amba_parameters;
	ethdsu : t_ethdsu_parameters;
	mem    : t_mem_parameters;
	ddr    : t_ddr_parameters;
	stat   : t_ahbstat_parameters;
	geth   : t_geth_parameters;
	uart   : t_uart_parameters;
	irq    : t_irq_parameters;
	timer  : t_timer_parameters;
	gpio   : t_gpio_parameters;
	pcie   : t_pcie_parameters;
	na     : t_na_parameters;
	crm    : t_crm_parameters;
	icore  : t_icore_parameters;
	tcpa   : t_tcpa_parameters;
	dvi    : t_dvi_parameters;
	dbg    : t_dbg_parameters;
  end record;

  type t_tile_config_array is array (natural range <>, natural range <>) of t_tile_config;

end;
