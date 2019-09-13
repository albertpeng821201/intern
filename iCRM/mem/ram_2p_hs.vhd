library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.ALL;
library grlib;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
library techmap;
use techmap.gencomp.all;
library top;
use top.config.all;
library icrm;
use icrm.all;
use icrm.crm_pkg.all;


entity ram_2p_hs is
	generic (
		tech		: integer;
		abits		: integer;
		dbits		: integer;
		my_tile_id	: integer;
		x_loc		: integer;
		y_loc		: integer;
		ways		: positive;
		depth		: positive
	);

	port (
		clk					: in std_logic;
		rst					: in std_logic;
		crmLUT				: in crmLUT_type;
		protect_set			: in std_logic;
		address1			: in std_logic_vector((abits-1) downto 0);
		datain1				: in std_logic_vector((dbits-1) downto 0);
		dataout1			: out std_logic_vector((dbits-1) downto 0);
		en1					: in std_logic;
		we1					: in std_logic;
		address2			: in std_logic_vector((abits-1) downto 0);
		datain2				: in std_logic_vector((dbits-1) downto 0);
		dataout2			: out std_logic_vector((dbits-1) downto 0);
		en2					: in std_logic;
		we2					: in std_logic;
		address3			: in std_logic_vector((abits-1) downto 0);
		datain3				: in std_logic_vector((dbits-1) downto 0);
		dataout3			: out std_logic_vector((dbits-1) downto 0);
		en3					: in std_logic;
		we3					: in std_logic;
		req1				: in std_logic;
		req2				: in std_logic;
		req3				: in std_logic;
		gnt1				: out std_logic;
		gnt2				: out std_logic;
		gnt3				: out std_logic;
		evict_flag_IM		: out std_logic;
		evict_data_IM		: out std_logic_vector((dbits-1) downto 0);
	 	evict_address_IM	: out std_logic_vector((abits-1) downto 0);
		rd_data_change1		: out std_logic;
		rd_data_change2		: out std_logic;
		rd_data_change3		: out std_logic;
		eviction_policy_IM	: in integer
	);

end entity ram_2p_hs;

architecture RTL of ram_2p_hs is

type ram_2p_hs_type is record
	gnt1			: std_logic;
	gnt2			: std_logic;
	gnt3			: std_logic;
	ram_enable		: std_logic;
	ram_addr		: std_logic_vector((abits-1) downto 0);
	ram_write		: std_logic;
	ram_data_wr		: std_logic_vector((dbits-1) downto 0);
	dataout1		: std_logic_vector((dbits-1) downto 0);
	dataout2		: std_logic_vector((dbits-1) downto 0);
	dataout3		: std_logic_vector((dbits-1) downto 0);
	rd_data_change1	: std_logic;
	rd_data_change2	: std_logic;
	rd_data_change3	: std_logic;
end record;
constant ram_2p_hs_type_default : ram_2p_hs_type := ('0', '0', '0', '0', (others=>'0'), '0', (others=>'0'), (others=>'0'), (others=>'0'), (others=>'0'), '0', '0', '0');

signal r,t : ram_2p_hs_type := ram_2p_hs_type_default;

type state_type is (idle,ready,busy); --type of state machine.
signal current_s,next_s,previous_s: state_type;
signal ram_addr : std_logic_vector((abits-1) downto 0);
signal ram_data_wr, ram_data_rd : std_logic_vector((dbits-1) downto 0); 
signal ram_enable, ram_write, busysig : std_logic;

begin

process (clk,rst)
begin
	if(rst /= '1') then
		--flag <= '0';
		current_s <= idle;	
	elsif(clk='1' and clk'Event) then
		current_s <= next_s;
		previous_s <= current_s;
		r <= t;
	end if;
end process;


process(current_s, previous_s, req1, req2, req3, en1, en2, en3, busysig, address1, address2, address3, datain1, datain2, datain3, we1, we2, we3, ram_data_rd, r)

variable v : ram_2p_hs_type;

begin
	v := r;

	-- set default values
	next_s <= current_s;

	case current_s is
		when idle =>
			if(req1 = '1') then -- priority to IM
				next_s <= ready;
				v.gnt1 := '1';
			elsif(req2 = '1') then
				next_s <= ready;
				v.gnt2 := '1';
			elsif(req3 = '1') then
				next_s <= ready;
				v.gnt3 := '1';
			else
				next_s <= idle;
			end if;

		when ready =>
			if r.gnt1 = '1' then
				if req1 = '0' then
					next_s <= idle;
					v.gnt1 := '0';
				elsif en1 = '1' then
					--busy1 <= '1'; --causes infinite delta iterations for consecutive requests
					v.ram_enable := '1'; -- should be only a pulse
					v.ram_addr := address1;
					if we1 = '1' then
						v.ram_write := '1'; -- should be only a pulse
						v.ram_data_wr := datain1;
					end if;
					next_s <= busy;
				end if;
			elsif r.gnt2 = '1' then
				if req2 = '0' then
					next_s <= idle;
					v.gnt2 := '0';
				elsif en2 = '1' then
					v.ram_enable := '1'; -- should be only a pulse
					v.ram_addr := address2;
					if we2 = '1' then
						v.ram_write := '1'; -- should be only a pulse
						v.ram_data_wr := datain2;
					end if;
					next_s <= busy;
				end if;
			elsif r.gnt3 = '1' then
				if req3 = '0' then
					next_s <= idle;
					v.gnt3 := '0';
				elsif en3 = '1' then
					v.ram_enable := '1'; -- should be only a pulse
					v.ram_addr := address3;
					if we3 = '1' then
						v.ram_write := '1'; -- should be only a pulse
						v.ram_data_wr := datain3;
					end if;
					next_s <= busy;
				end if;
			else
				next_s <= idle; -- should never get here
			end if;
	
		when busy =>
			v.ram_enable := '0'; --request already handed over to directory (pulse)
			v.ram_write := '0';
			if previous_s = ready then
				next_s <= busy;-- skip one cycle because signal changes are not actual reads
			else
				if busysig = '0' then
					if r.gnt1 = '1' then
						v.rd_data_change1 := not r.rd_data_change1; --flip bit to indicate request complete
						v.dataout1 := ram_data_rd; --put the read value out regardless whether request was a write/read
						if en1 = '1' then --back to back request
							v.ram_enable := '1'; -- should be only a pulse
							v.ram_addr := address1;
			 				if we1 = '1' then
								v.ram_write := '1'; -- should be only a pulse
								v.ram_data_wr := datain1;
							end if;
						next_s <= busy;
						else
						next_s <= ready;
						end if;
					elsif r.gnt2 = '1' then
						v.rd_data_change2 := not r.rd_data_change2; --flip bit to indicate request complete
						v.dataout2 := ram_data_rd; --put the read value out regardless whether request was a write/read
						if en2 = '1' then --back to back request
							v.ram_enable := '1'; -- should be only a pulse
							v.ram_addr := address2;
			 				if we2 = '1' then
								v.ram_write := '1'; -- should be only a pulse
								v.ram_data_wr := datain2;
							end if;
							next_s <= busy;
						else
							next_s <= ready;
						end if;
					else
						v.rd_data_change3 := not r.rd_data_change3; --flip bit to indicate request complete
						v.dataout3 := ram_data_rd; --put the read value out regardless whether request was a write/read
						if en3 = '1' then --back to back request
							v.ram_enable := '1'; -- should be only a pulse
							v.ram_addr := address3;
						 	if we3 = '1' then
								v.ram_write := '1'; -- should be only a pulse
								v.ram_data_wr := datain3;
							end if;
							next_s <= busy;
						else
							next_s <= ready;
						end if;
					end if;
			 else
				next_s <= busy;
			 end if;
		 end if;

		when others =>
			next_s <= idle;

	end case;

	gnt1 <= v.gnt1;
	gnt2 <= v.gnt2;
	gnt3 <= v.gnt3;
	ram_enable <= v.ram_enable;
	ram_addr <= v.ram_addr;
	ram_write <= v.ram_write;
	ram_data_wr <= v.ram_data_wr;
	dataout1 <= v.dataout1;
	dataout2 <= v.dataout2;
	dataout3 <= v.dataout3;
	rd_data_change1 <= v.rd_data_change1;
	rd_data_change2 <= v.rd_data_change2;
	rd_data_change3 <= v.rd_data_change3;

	t <= v;
end process;

my_ram : entity icrm.cache_controller -- cache dir implementation
	generic map(
		tech => tech,
		abits => abits,
		dbits => dbits,
		my_tile_id => my_tile_id,
		x_loc => x_loc,
		y_loc => y_loc,
		ways => ways,
		depth => depth
		)
	port map(
		clk => clk,
		rst => rst,
		address => ram_addr,
		datain => ram_data_wr,
		dataout => ram_data_rd,
		enable => ram_enable,
		write => ram_write,
		crmLUT => crmLUT,
		evict_flag => evict_flag_IM,
		evict_data => evict_data_IM,
		evict_address => evict_address_IM,
		busy => busysig,
		eviction_policy_IM => eviction_policy_IM,
		protect_set => protect_set
	);
end architecture RTL;
