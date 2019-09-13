library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.Numeric_Std.all;
library icrm;
use icrm.all;
library techmap;
use techmap.gencomp.all;

entity sync_ram_set is
	generic (
		tech		: integer;
		indexbits	: positive;
		tagbits		: positive;
		dbits		: positive;
		depth		: positive
	);

	port (
		clk				: in std_logic;
		rst				: in std_logic;
		index1			: in std_logic_vector((indexbits-1) downto 0);
		tag1		 	: in std_logic_vector((tagbits-1) downto 0);
		datain1			: in std_logic_vector((dbits-1) downto 0);
		dataout1 		: out std_logic_vector((dbits-1) downto 0);
		we1 	 		: in std_logic;
		en1	 			: in std_logic;
		hit_flag 		: out std_logic;
		valid_flag		: out std_logic;
		evict_order_flag: in std_logic;
		evict_data_out 	: out std_logic_vector((dbits-1) downto 0);
		bitvector_out	: out std_logic_vector((dbits-1) downto 0); --for LNS eviction
		address_out 	: out std_logic_vector((tagbits+indexbits-1) downto 0) --for LRU eviction
	);
end entity sync_ram_set;

architecture RTL of sync_ram_set is

type sync_ram_set_type is record
	hit_flag		: std_logic;
	valid_flag		: std_logic;
	bitvector_out	: std_logic_vector((dbits-1) downto 0);
	address_out		: std_logic_vector((tagbits+indexbits-1) downto 0);
	evict_data_out	: std_logic_vector((dbits-1) downto 0);
	dataout1		: std_logic_vector((dbits-1) downto 0);
end record;
constant sync_ram_set_type_default : sync_ram_set_type := ('0', '0', (others=>'0'), (others=>'0'), (others=>'0'), (others=>'0'));

signal r,t : sync_ram_set_type := sync_ram_set_type_default;

type state is (ready_state, resp_controller, wait_ack, execute_evict_order,wait_ack_evict);
signal current_state : state := ready_state;
signal next_state : state := ready_state;

--type ram_type is array (0 to depth-1) of std_logic_vector((dbits + tagbits) downto 0); -- no -1 because +1 for valid bit
--signal r_ram,t_ram : ram_type := (others=>(others=>'0'));

signal ram_write_en, ram_en : std_logic := '0';
signal ram_read_data : std_logic_vector((dbits + tagbits) downto 0);
signal ram_write_data : std_logic_vector((dbits + tagbits) downto 0);

begin

SetProc: process(current_state, en1, we1, evict_order_flag, tag1,index1,datain1,r, ram_read_data) is

variable read_variable	: std_logic_vector((dbits + tagbits) downto 0); -- no -1 because +1 for valid bit
variable write_variable	: std_logic_vector((dbits + tagbits) downto 0);
variable evict_variable	: std_logic_vector((dbits + tagbits) downto 0);
variable v : sync_ram_set_type;
--variable v_ram : ram_type;

--type ram_type is array (0 to depth-1) of std_logic_vector((dbits + tagbits) downto 0); -- no -1 because +1 for valid bit
--variable ram : ram_type := (others=>(others=>'0'));

begin
	read_variable := (others=>'0');
	v := r;
	--v_ram := r_ram;
	--ram_en <= '0';
	ram_write_en <= '0';
	ram_write_data <= (others=>'0');
	next_state <= current_state;

	case current_state is

		when ready_state =>
			v.hit_flag := '0';
			v.valid_flag := '0';
			if en1='1' then
				--ram_en<='1';
				next_state <= resp_controller;
			else
				next_state <= ready_state;
			end if;
 
		when resp_controller =>
			--read_variable := v_ram(to_integer(unsigned(index1)));
			read_variable := ram_read_data;
			if read_variable((dbits + tagbits) downto dbits+1) = tag1 and read_variable(dbits) = '1' then --format <tagbits><valid bit><data bits>
				v.hit_flag := '1';
				if we1 = '1' then
					--ram_en<='1';
					ram_write_en <= '1';
					read_variable(dbits-1 downto 0) := datain1;
					ram_write_data <= read_variable;
					--v_ram(to_integer(unsigned(index1))) := read_variable;
				else
					v.dataout1 := read_variable(dbits-1 downto 0);
				end if;
			else
				v.valid_flag := read_variable(dbits); --tell controller whether entry is valid or not (whether empty)
				v.bitvector_out := read_variable(dbits-1 downto 0); --for LNS eviction
				v.address_out := read_variable((dbits + tagbits) downto dbits+1) & index1; --for LRU eviction
			end if;
			next_state <= wait_ack;

		when wait_ack => -- all sets remain here except the one that is evicting
			if en1='0' then
				next_state <= ready_state;
			elsif evict_order_flag = '1' then
				next_state <= execute_evict_order;
			else
				next_state <= wait_ack;
			end if;

		when execute_evict_order => -- state to protect ram data from retriggering of wait_ack
			if r.valid_flag = '1' then
				--evict_variable := v_ram(to_integer(unsigned(index1)));
				evict_variable := ram_read_data;
				v.evict_data_out := evict_variable(dbits-1 downto 0);
			end if;
			write_variable((dbits + tagbits) downto dbits+1) :=  tag1;
			write_variable(dbits) :=  '1';
			if we1 = '1' then--format <tagbits><valid bit><data bits>
				write_variable(dbits-1 downto 0) := datain1;--this should never happen
			else
				write_variable(dbits-1 downto 0) := (others => '0');-- new data to start tracking
				v.dataout1 := (others => '0');
			end if;
			--ram_en<='1';
			ram_write_en <= '1';
			ram_write_data <= write_variable;
			--v_ram(to_integer(unsigned(index1))) := write_variable;-- might lead to a latch
			next_state <= wait_ack_evict;

		when wait_ack_evict =>
			if en1='0' then
				next_state <= ready_state;
			else
				next_state <= wait_ack_evict;
			end if;

		when others => next_state <= ready_state;

		end case;

	hit_flag <= v.hit_flag;
	valid_flag <= v.valid_flag;
	bitvector_out <= v.bitvector_out;
	address_out <= v.address_out;
	evict_data_out <= v.evict_data_out;
	dataout1 <= v.dataout1;

	t <= v;
	--t_ram<=v_ram;
end process SetProc;

stateproc:process(clk)
begin
	if clk'event and clk='1' then
		if rst = '0' then
			current_state <= ready_state;
			-- reset all flags and vectors
		else
			current_state <= next_state;
			r <= t;
			--r_ram<=t_ram;
		end if;
	end if;
end process stateproc;

ram0: entity icrm.sync_ram
	generic map (
		indexbits => indexbits,
		tagbits => tagbits,
		dbits=>dbits,
		depth=>depth
	)
	port map (
		clk => clk,
		ram_address => index1,
		ram_data_in => ram_write_data,
		ram_data_out => ram_read_data,
		ram_wr_en => ram_write_en
	);

--ram0: entity techmap.syncram
--generic map
--(tech => tech,
--abits => indexbits,
--dbits => tagbits + dbits + 1) --+1 valid bit
--port map
--(clk => clk,
--address => index1,
--datain => ram_write_data,
--dataout => ram_read_data,
--enable => ram_en,
--write => ram_write_en);

end architecture RTL;
