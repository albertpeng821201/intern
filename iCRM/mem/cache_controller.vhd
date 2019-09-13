library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.Numeric_Std.all;
library icrm;
use icrm.all;
use icrm.crm_pkg.all;
library ina;
library ina_pkg;
use ina_pkg.ina_config.all;

entity cache_controller is
	generic (
		tech		: integer;
		abits		: positive;
		dbits		: positive;
		my_tile_id	: integer;
		x_loc		: integer;
		y_loc		: integer;
		ways		: positive;
		depth		: positive; --lines per way
		lru_depth	: positive := 30
);

	port (
		clk					: in std_logic;
		rst					: in std_logic;
		address				: in std_logic_vector((abits-1) downto 0);
		datain				: in std_logic_vector((dbits-1) downto 0);
		dataout				: out std_logic_vector((dbits-1) downto 0);
		write				: in std_logic;
		enable				: in std_logic;
		crmLUT				: in crmLUT_type;
		evict_flag			: out std_logic;
		evict_data			: out std_logic_vector((dbits-1) downto 0);
		evict_address		: out std_logic_vector((abits-1) downto 0);
		busy				: out std_logic;
		eviction_policy_IM	: in integer;
		protect_set			: in std_logic -- indicates a request coming from a state that should not evict any entries
  );
end entity cache_controller;

architecture RTL of cache_controller is

--===============================================================================================
-- log2ceil function taken (taken from ONLINE REFERENCE)
--===============================================================================================

function log2ceil(arg : positive) return natural is
	variable tmp : positive := 1;
	variable log : natural  := 0;
	begin
	if arg = 1 then return 0; end if;
	while arg > tmp loop
		tmp := tmp * 2;
		log := log + 1;
	end loop;
	return log;
end function;

--===============================================================================================
-- Function to search for either a 1 or a 0 bit in a vector
--===============================================================================================

function search_for_bit (in_vector : std_logic_vector; bit_to_search : std_logic) return integer is
	begin
	for i in 0 to (in_vector'length-1) loop
		if (in_vector(i) = bit_to_search) then
			return i;
		end if;
	end loop;
	return in_vector'length;
end function search_for_bit;

--===============================================================================================
-- Function to search for entry with lowest number of sharers
--===============================================================================================

type bv_count_array_type is array (0 to ways-1) of integer;
type set_data_type is array (0 to ways-1) of std_logic_vector(datain'range);

impure function lowest_number_sharers (in_set : set_data_type) return integer is
	variable temp_bv		: std_logic_vector(datain'range) := (others => '0');
	variable tempcnt		: integer := 0;
	variable mincnt			: integer := 0;
	variable minsharers		: integer := 0;
	variable bv_count_array	: bv_count_array_type;
	begin
	for i in 0 to (ways-1) loop
		temp_bv := in_set(i);
		for j in 0 to (dbits-3) loop -- data pattern <enable bit><exceed max tiles in cr><tiles logical> so we ignore enable and exceed bits
			if (temp_bv(j) = '1') then
				tempcnt := tempcnt+1;
			end if;
		end loop;
		bv_count_array(i) := tempcnt;
		tempcnt := 0;
	end loop;

	mincnt := bv_count_array(0);

	for i in 0 to (ways-1) loop
		if (mincnt >= bv_count_array(i)) then
			mincnt := bv_count_array(i);
			minsharers := i;
		end if;
	end loop;
	return minsharers;
end function lowest_number_sharers;

--===============================================================================================
-- Function to search LRU memory for all tags from set
--===============================================================================================

type lru_mem_type is array (0 to lru_depth) of std_logic_vector(abits-1 downto 0); -- memory type to save the last lru_depth addresses
type lru_address_out_type is array (0 to ways-1) of std_logic_vector(abits-1 downto 0);
type distance_head_type is array (0 to ways-1) of integer;

impure function search_lru_mem (in_addresses : lru_address_out_type; in_lru_mem : lru_mem_type; in_head : integer) return integer is
	variable d_from_head		: distance_head_type; --distance from head
	variable evict_candidate	: integer range 0 to ways-1 := 0;
	variable oldest				: integer := 0;
	begin
	for i in 0 to ways-1 loop
		d_from_head(i) := lru_depth+1; -- assume not found
		for j in 0 to lru_depth loop
			--if in_addresses(i) = in_lru_mem((in_head-j) mod 31) then --lru hit
			if in_addresses(i) = in_lru_mem(j) then --lru hit
				--if ((in_head-j) mod 31) > in_head then
				if j > in_head then
					--d_from_head(i) := 30 - ((in_head-j) mod 31) + in_head;
					d_from_head(i) := lru_depth - j + in_head;
					exit;
				else --elsif j <= in_head then
					--d_from_head(i) := in_head - ((in_head-j) mod 31);
					d_from_head(i) := in_head - j;
					exit;
				end if;
			end if;
		end loop;
	end loop;

	--search for not found or oldest
	oldest := d_from_head(0);
	evict_candidate := 0;
	
	for i in 0 to ways-1 loop
		if oldest <= d_from_head(i) then
			oldest := d_from_head(i);
			evict_candidate := i;
		end if;
	end loop;

	return evict_candidate;
end function search_lru_mem;

--===============================================================================================
-- Function to calculate age of way entries (4 ways+)
--===============================================================================================

impure function calc_ages (in_addresses : lru_address_out_type; in_lru_mem : lru_mem_type; in_head : integer) return distance_head_type is
	variable d_from_head : distance_head_type; --distance from head
	begin
	for i in 0 to ways-1 loop
		d_from_head(i) := lru_depth+1; -- assume not found
		for j in 0 to lru_depth loop
			--if in_addresses(i) = in_lru_mem((in_head-j) mod 31) then --lru hit
			if in_addresses(i) = in_lru_mem(j) then --lru hit
				--if ((in_head-j) mod 31) > in_head then
				if j > in_head then
					--d_from_head(i) := 30 - ((in_head-j) mod 31) + in_head;
					d_from_head(i) := lru_depth - j + in_head;
					exit;
				else-- j <= in_head then
					--d_from_head(i) := in_head - ((in_head-j) mod 31);
					d_from_head(i) := in_head - j;
					exit;
				end if;
			end if;
 		end loop;
	end loop;
	return d_from_head;
end function calc_ages;
 
--===============================================================================================
-- Function to calculate oldest way entry (4 ways+)
--===============================================================================================

impure function find_oldest (in_distances : distance_head_type) return integer is
	variable evict_candidate	: integer range 0 to ways-1 := 0;
	variable oldest				: integer := 0;
	begin
	--search for not found or oldest
	oldest := in_distances(0);
	evict_candidate := 0;
	for i in 0 to ways-1 loop
		if oldest < in_distances(i) then
			oldest := in_distances(i);
			evict_candidate := i;
		end if;
	end loop;
	return evict_candidate;
end function find_oldest;

--===============================================================================================
-- Function to calculate cost array
--===============================================================================================

--constant my_y_pos : integer range 0 to dim_y := integer(floor(real(my_tile_id)/real(dim_x)));
--constant my_x_pos : integer range 0 to dim_x := my_tile_id mod dim_x;

--signal scope_dim_y : integer := dim_y;
--signal scope_dim_x : integer := dim_x;
--signal scope_loc_y : integer := y_loc;
--signal scope_loc_x : integer := x_loc;

impure function calculate_costLUT (in_crmLUT : crmLUT_type) return crmLUT_type is
	variable x_pos		: integer := 0; -- range 0 to dim_x-1 := 0;
	variable y_pos		: integer := 0; -- range 0 to dim_y-1 := 0;
	variable checkdiv	: integer := 0;
	variable xdistance	: integer := 0; -- range 0 to dim_x := 0;
	variable ydistance	: integer := 0; -- range 0 to dim_y := 0;
	variable cost_LUT	: crmLUT_type := (others => 0);
	begin
	for i in 0 to in_crmLUT'high loop
		if (in_crmLUT(i) <= (dim_x*dim_y)-1) and (in_crmLUT(i)>=0) then
			checkdiv := in_crmLUT(i)-dim_x;
			y_pos := 0; --reset y_pos variable
			for j in 0 to dim_y loop  --division floor loop --y_pos := integer(floor(real(in_crmLUT(i))/real(dim_x)));
				if checkdiv >= 0 then
					y_pos := y_pos + 1;
				end if;
				checkdiv := checkdiv-dim_x;
			end loop;
			x_pos := in_crmLUT(i) mod dim_x;

			if x_pos > x_loc then
				xdistance := x_pos - x_loc;
			else
				xdistance := x_loc - x_pos;
			end if;

			if y_pos > y_loc then
				ydistance := y_pos - y_loc;
			else
				ydistance := y_loc - y_pos;
			end if;

			cost_LUT(i) := xdistance + ydistance;
		end if;
	end loop;
	return cost_LUT;
end function calculate_costLUT;

--===============================================================================================
-- Function to calculate minimum cost (distance) way bitvector
--===============================================================================================

type way_cost_type is array (0 to ways-1) of integer;

impure function calculate_lowest_cost (in_vector : set_data_type; in_costLUT: crmLUT_type) return integer is
	variable temp_bv		: std_logic_vector(datain'range) := (others => '0');
	variable mincost		: integer := 0;
	variable mincost_way	: integer range 0 to ways-1 := 0;
	variable way_costs		: way_cost_type;
	begin
	for i in 0 to (ways-1) loop
		temp_bv := in_vector(i);
		way_costs(i) := 0;
		for j in 0 to (dbits-3) loop -- data pattern <enable bit><exceed max tiles in cr><tiles logical> so we ignore enable and exceed bits
			if temp_bv(j) = '1' then
				way_costs(i) := way_costs(i) + in_costLUT(j); --sum the total cost of sharers in the bitvector
			end if;
		end loop;
	end loop;

	mincost := way_costs(0);

	for i in 0 to (ways-1) loop
		if mincost >= way_costs(i) then --way bitvector with less cost found
			mincost := way_costs(i);
			mincost_way := i;
		end if;
	end loop;
	return mincost_way;
end function calculate_lowest_cost;

--===============================================================================================
-- Function to calculate cost (distance) for all way bitvectors (4 ways+)
--===============================================================================================

impure function calculate_way_costs (in_vector : set_data_type; in_costLUT: crmLUT_type) return way_cost_type is
	variable temp_bv	: std_logic_vector(datain'range) := (others => '0');
	variable way_costs	: way_cost_type;
	begin
	for i in 0 to (ways-1) loop
		temp_bv := in_vector(i);
		way_costs(i) := 0;
		for j in 0 to (dbits-3) loop -- data pattern <enable bit><exceed max tiles in cr><tiles logical> so we ignore enable and exceed bits
			if temp_bv(j) = '1' then
				way_costs(i) := way_costs(i) + in_costLUT(j); --sum the total cost of sharers in the bitvector
			end if;
		end loop;
	end loop;

	return way_costs;
end function calculate_way_costs;
  
--===============================================================================================
-- Function to minimum cost among all way bitvectors (4 ways+)
--===============================================================================================
 
impure function find_min_way_cost (way_costs : way_cost_type) return integer is
	variable mincost		: integer := 0;
	variable mincost_way	: integer range 0 to ways-1 := 0;
	begin
	mincost := way_costs(0);
	for i in 0 to (ways-1) loop
		if mincost >= way_costs(i) then --way bitvector with less cost found
			mincost := way_costs(i);
			mincost_way := i;
		end if;
	end loop;
	return mincost_way;
end function find_min_way_cost;

--===============================================================================================
-- Function to decide which way entries should be protected by LRU
--===============================================================================================

impure function filter_lru_mem (in_addresses : lru_address_out_type; in_lru_mem : lru_mem_type) return std_logic_vector is
	variable lru_mem_hit		: distance_head_type; --hit array
	variable lru_filter			: std_logic_vector(ways downto 0); -- <all found or none><ways found>
	variable evict_candidate	: integer range 0 to ways-1 := 0;
	variable hit_count			: integer := 0;
	begin
	for i in 0 to ways-1 loop
		lru_filter(i+1) := '0'; -- assume not found
		for j in 0 to lru_depth loop
			if in_addresses(i) = in_lru_mem(j) then --lru hit
				lru_filter(i+1) := '1';
				hit_count := hit_count + 1;
				exit;
			end if;
		end loop;
	end loop;

	--search for not found
	if (hit_count = ways) or (hit_count = 0) then
		lru_filter(0) := '0'; --all found or not found
		return lru_filter;
	else
		lru_filter(0) := '1'; --only some were found, protection enabled
		return lru_filter;
	end if;
end function filter_lru_mem;

--===============================================================================================
-- Function to decide which way entries should be preserved by LNS
--===============================================================================================

impure function filter_lns (in_set : set_data_type; lru_protected_ways : std_logic_vector(ways downto 0)) return std_logic_vector is
	variable temp_bv		: std_logic_vector(datain'range) := (others => '0');
	variable tempcnt		: integer := 0;
	variable mincnt			: integer := 0;
	variable lns_filter		: std_logic_vector(ways downto 0) := (others => '0'); -- <several min sharers><min sharers>
	variable bv_count_array	: bv_count_array_type := (others => 0);
	begin
	for i in 0 to (ways-1) loop
		if (lru_protected_ways(0) = '1') and (lru_protected_ways(i+1) = '1') then -- lru protection
			bv_count_array(i) := region_size+2;
		else
			temp_bv := in_set(i);
			for j in 0 to (dbits-3) loop -- data pattern <enable bit><exceed max tiles in cr><tiles logical> so we ignore enable and exceed bits
				if (temp_bv(j) = '1') then
					bv_count_array(i) := bv_count_array(i)+1;
		  		end if;
			end loop;
		end if;
	end loop;

	mincnt := region_size+1;

	for i in 0 to (ways-1) loop
		if mincnt > bv_count_array(i) then
			mincnt := bv_count_array(i);
			lns_filter := (others=>'0'); -- clear array
			lns_filter(i+1) := '1'; -- single candidate to evict
		elsif mincnt = bv_count_array(i) then
			lns_filter(0) := '1'; --more than one candidate to evict
			lns_filter(i+1) := '1';
		else
		end if;
	end loop;
	return lns_filter;
end function filter_lns;

--===============================================================================================
-- Function to calculate minimum cost (distance) way bitvector that is not protected by LRU or LNS
--===============================================================================================

impure function filter_sdf (in_vector : set_data_type; in_costLUT: crmLUT_type; lns_protected_ways : std_logic_vector(ways downto 0)) return integer is
	variable temp_bv : std_logic_vector(datain'range) := (others => '0');
	variable mincost : integer := 0;
	variable mincost_way : integer range 0 to ways-1 := 0; -- 3 for testing purposes
	variable way_costs : way_cost_type;
	begin
	for i in 0 to (ways-1) loop
		if lns_protected_ways(i+1) = '1' then -- not protected by lns
			temp_bv := in_vector(i);
			way_costs(i) := 0;
			for j in 0 to (dbits-3) loop -- data pattern <enable bit><exceed max tiles in cr><tiles logical> so we ignore enable and exceed bits
				if temp_bv(j) = '1' then
					way_costs(i) := way_costs(i) + in_costLUT(j); --sum the total cost of sharers in the bitvector
				end if;
			end loop;
		else
			way_costs(i) := region_size*dim_x*dim_y;
		end if;
	end loop;

	mincost := way_costs(0);

	for i in 0 to (ways-1) loop
		if (mincost >= way_costs(i)) then
			mincost := way_costs(i);
			mincost_way := i;
		end if;
	end loop;
	return mincost_way;
end function filter_sdf;

--===============================================================================================
-- Constants, types and signals
--===============================================================================================

constant indexbits	: positive := log2ceil(depth);
constant tagbits	: positive := abits - indexbits;

constant all_ones	: std_logic_vector (ways-1 downto 0) := (others => '1');
constant all_zeros	: std_logic_vector (ways-1 downto 0) := (others => '0');
constant no_sharers	: std_logic_vector (region_size-2 downto 0) := (others => '0');


type cache_controller_type is record
	set_write		: std_logic;
	set_data_in		: std_logic_vector(datain'range);
	index			: std_logic_vector(indexbits-1 downto 0);
	tag				: std_logic_vector(tagbits-1 downto 0);
	dataout			: std_logic_vector((dbits-1) downto 0);
	evict_flag		: std_logic;
	evict_order		: std_logic_vector((ways-1) downto 0);
	evict_data		: std_logic_vector((dbits-1) downto 0);
	evict_address	: std_logic_vector((abits-1) downto 0);
	lru_head_sig	: integer;
	hit_array		: std_logic_vector((ways-1) downto 0);
	valid_entries	: std_logic_vector((ways-1) downto 0);
	bitvector_out	: set_data_type;
	lru_address_out	: lru_address_out_type;
	way_costs		: way_cost_type;
	way_ages		: distance_head_type;
	lru_protected	: std_logic_vector(ways downto 0); -- <protection enable><ways found in lru>
	lns_protected	: std_logic_vector(ways downto 0); -- <protection enable><multiple min ways found>
	protect_set		: std_logic;
end record;

constant cache_controller_type_default : cache_controller_type := ('0', (others => '0'), (others => '0'), (others => '0'), (others => '0'), '0', (others => '0'), (others => '0'), (others => '0'), 0, (others => '0'), (others => '0'), (others=>(others=>'0')), (others=>(others=>'0')), (others=>0), (others=>0), (others => '0'), (others => '0'), '0');

type state is (ready, waitsets, checksets, eviction_calc, evict_to_IM, evict_processing, hybrid_sdf_filter);
signal current_state	: state := ready;
signal next_state		: state := ready;

signal r,t : cache_controller_type := cache_controller_type_default;

-- arrays for sending and recieving data from sets
signal evict_order		: std_logic_vector((ways-1) downto 0) := (others => '0');
signal valid_entries	: std_logic_vector((ways-1) downto 0) := (others => '0');
signal hit_array		: std_logic_vector((ways-1) downto 0) := (others => '0');
--signal set_ready		: std_logic_vector((ways-1) downto 0) := (others => '0');
signal set_data_out		: set_data_type; -- so that we dont have multiple drivers for output signal
signal bitvector_out	: set_data_type; --for LNS eviction
signal evict_data_out	: set_data_type;
signal set_data_in		: std_logic_vector(datain'range) := (others => '0');
signal set_enable		: std_logic := '0';
signal set_write		: std_logic := '0';

signal index			: std_logic_vector(indexbits-1 downto 0) := (others => '0');
signal tag				: std_logic_vector(tagbits-1 downto 0) := (others => '0');

--for LRU eviction
signal r_lru_mem, t_lru_mem : lru_mem_type := (others=>(others=>'0'));
signal lru_address_out : lru_address_out_type := (others=>(others=>'0')); -- also used for evict_address out port

--for SDF eviction
signal r_costLUT, t_costLUT : crmLUT_type := (others=> 0);

--signal to change eviction policy at runtime
signal eviction_policy : integer;

begin

--===============================================================================================
-- Generate sets
--===============================================================================================

gen_sets: for i in 0 to (ways-1) generate
	set0: entity icrm.sync_ram_set
		generic map
			(tech => tech,
			indexbits => indexbits,
			tagbits	=> tagbits,
			dbits => dbits,
			depth => depth)
		port map
			(clk => clk,
			rst => rst,
			index1 => index,
			tag1 => tag,
			datain1 => set_data_in,
			dataout1 => set_data_out(i),
			we1 => set_write,
			en1 => set_enable,
			hit_flag => hit_array(i),
			valid_flag => valid_entries(i),
			evict_order_flag => evict_order(i),
			evict_data_out => evict_data_out(i),
			bitvector_out => bitvector_out(i), --for LNS evictions
			address_out => lru_address_out(i)); --for LRU evictions and evict_address out port
end generate gen_sets;

RamProc: process(current_state, enable, write, address, valid_entries, datain, hit_array, bitvector_out, r_costLUT, set_data_out, evict_order, lru_address_out, r, evict_data_out, r_lru_mem, eviction_policy, protect_set) is

variable search_index			: integer range 0 to ways := 0;
variable evict_data_out_var		: std_logic_vector(datain'range);
variable v						: cache_controller_type;
variable v_lru_mem				: lru_mem_type;
variable lru_protected			: std_logic_vector(ways downto 0);
variable lns_protected			: std_logic_vector(ways downto 0);
variable trunc_lns_protected	: std_logic_vector(ways-1 downto 0);


begin
	v := r;
	v_lru_mem := r_lru_mem;

	-- set default values
	busy <= '1';
	set_enable <= '1';
	next_state <= current_state;

	case current_state is
		when ready =>
			busy <= '0';
			set_enable <= '0';
			-- evict_flag <= '0'; -- removed: not compatible with handshake, user must check evict flag after each read (flag will remain set till next read/write request)
			if enable='1' then -- enable pulse from ram_2p_hs
				set_enable <= '1';  --set_enable to decouple signal changes from outside while the sets are functioning	
				if write = '1' then
					v.set_write := '1';
					v.set_data_in := datain;
				end if;
				v.protect_set:= protect_set;
				v.index := address(indexbits-1 downto 0);
				v.tag := address(address'high downto indexbits);
				--for LRU eviction
				if (eviction_policy = 2) or (eviction_policy = 4) then
					v.lru_head_sig := r.lru_head_sig + 1;
					v.lru_head_sig := v.lru_head_sig mod (lru_depth+1);
					--v_lru_mem(v.lru_head_sig) := address;
				end if;
				next_state <= waitsets;
			else
				next_state <= ready;
			end if;

		when waitsets =>
			--for LRU eviction
			if (eviction_policy = 2) or (eviction_policy = 4) then
				--v.lru_head_sig := r.lru_head_sig + 1;
				--v.lru_head_sig := v.lru_head_sig mod (lru_depth+1);
				v_lru_mem(r.lru_head_sig) := r.tag & r.index;
			end if;
			v.hit_array:= hit_array;
			v.valid_entries := valid_entries;
			v.bitvector_out := bitvector_out;
			v.lru_address_out := lru_address_out;
			next_state <= checksets;

		when checksets =>
			if r.hit_array = all_zeros then
				if r.protect_set = '1' then -- dont evict any entries
					v.set_write := '0';
					v.evict_flag := '0';
					set_enable <= '0';
					v.dataout := (others=>'0');
					next_state <= ready;
				else
					next_state <= evict_processing;
					-- search for invalid entries to avoid real eviction
					search_index := search_for_bit(r.valid_entries,'0');
					if search_index = ways then -- all the entries were valid (actual eviction necessary)
						if eviction_policy = 1 then  	-- 1: least number of sharers eviction
							v.evict_order(lowest_number_sharers(r.bitvector_out)):= '1';
						elsif eviction_policy = 2 then  -- 2: least recently used eviction
							if ways < 4 then
								v.evict_order(search_lru_mem(r.lru_address_out,r_lru_mem,r.lru_head_sig)) := '1';
							else
								v.way_ages := calc_ages(r.lru_address_out,r_lru_mem,r.lru_head_sig);
								next_state <= eviction_calc;
							end if;
						elsif eviction_policy = 3 then		-- 3: shortest distance first
							if ways < 4 then
								v.evict_order(calculate_lowest_cost(r.bitvector_out, r_costLUT)) := '1';
							else
								v.way_costs := calculate_way_costs(r.bitvector_out, r_costLUT);
								next_state <= eviction_calc;
							end if;
						elsif eviction_policy = 4 then		-- 4: hybrid eviction policy
							if ways < 4 then
								lru_protected := filter_lru_mem(r.lru_address_out,r_lru_mem);
								lns_protected := filter_lns(r.bitvector_out,lru_protected);
								if lns_protected(0) = '0' then -- we have a single way with minimum sharers (no lns protection)
									trunc_lns_protected := lns_protected(ways downto 1);
									v.evict_order(search_for_bit(trunc_lns_protected,'1')) := '1'; 
								else
									v.evict_order(filter_sdf(r.bitvector_out,r_costLUT,lns_protected)) := '1';
								end if;
							else
								v.lru_protected := filter_lru_mem(r.lru_address_out,r_lru_mem);
								next_state <= eviction_calc;  
							end if;
						else
							v.evict_order(0) := '1';	-- 5: dummy eviction of way 0
						end if;
					else -- one of the entries is invalid, no real eviction necessary
						v.evict_order(search_index) := '1';
					end if;
				end if;
			else
				--check hit array here with forloop function
				search_index := search_for_bit(r.hit_array,'1');
				v.dataout := set_data_out(search_index); -- search_index is whoever got a hit
				v.set_write := '0';
				v.evict_flag := '0';
				next_state <= ready;
				set_enable <= '0';
			end if;

		when eviction_calc => -- state only needed with 4 ways CBD and eviction policies 2,3 and 4
			if eviction_policy = 2 then
				v.evict_order(find_oldest(r.way_ages)) := '1';
				next_state <= evict_processing;
			elsif eviction_policy = 3 then
				v.evict_order(find_min_way_cost(r.way_costs)) := '1';
				next_state <= evict_processing;
			else
				v.lns_protected := filter_lns(r.bitvector_out,r.lru_protected);
				if v.lns_protected(0) = '0' then -- we have a single way with minimum sharers (no lns protection)
					trunc_lns_protected := v.lns_protected(ways downto 1);
					v.evict_order(search_for_bit(trunc_lns_protected,'1')) := '1';
					next_state <= evict_processing;
				else
					next_state <= hybrid_sdf_filter;
				end if;
			end if;
		
		when hybrid_sdf_filter => -- state only needed with 4 ways hybrid eviction
			v.evict_order(filter_sdf(r.bitvector_out,r_costLUT,r.lns_protected)) := '1';
			next_state <= evict_processing;

		when evict_processing =>
			next_state <= evict_to_IM;

		when evict_to_IM =>
			--send evict data and evict flag upwards
			if valid_entries = all_ones then -- if all entries were valid then, we must have had an eviction
				search_index := search_for_bit(r.evict_order,'1');
				evict_data_out_var := evict_data_out(search_index);
				if evict_data_out_var((region_size-2) downto 0) /= no_sharers then -- assert invalidations needed --supressed eviction
					v.evict_data := evict_data_out(search_index);
					v.evict_flag := '1'; --set flag for IM to generate invalidation due to eviction
					v.evict_address := lru_address_out(search_index);
				else
					v.evict_flag := '0';
				end if;
			else
				v.evict_flag := '0';
			end if;
			if r.set_write = '0' then
				-- if it was a read that caused the eviction then new entry data will be all zeros
				v.dataout := (others => '0');
			end if;
			set_enable <= '0';
			v.set_write := '0';
			v.evict_order := (others => '0');
			next_state <= ready;

	when others => next_state <= ready;

	end case;

	evict_address <= v.evict_address;
	evict_data <= v.evict_data;
	evict_order <= v.evict_order;
	evict_flag <= v.evict_flag;
	dataout <= v.dataout;
	set_write <= r.set_write;
	set_data_in <= r.set_data_in;
	index <= v.index; --send it earlier to ram so that read_data is available on time
	tag <= r.tag;

	t <= v;
	t_lru_mem <= v_lru_mem;

end process RamProc;

CalcDistance:process(crmLUT)
begin
	t_costLUT <= calculate_costLUT(crmLUT);
end process CalcDistance;

stateproc:process(clk)
begin
	if clk'event and clk='1' then
		if rst = '0' then
			current_state <= ready;
			-- also reset all flags and vectors if necessary in ready state
		else
			current_state <= next_state;
			r <= t;
			r_lru_mem <= t_lru_mem;
			r_costLUT <= t_costLUT;
			eviction_policy <= eviction_policy_IM;
		end if;
	end if;
end process stateproc;

end architecture RTL;
