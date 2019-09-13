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


entity ram_2p is
    generic (
        tech  : integer;
        abits : integer;
        dbits : integer
    );

    port (
        clk     	: in  std_logic;
        rst     	: in  std_logic;
        address1	: in  std_logic_vector((abits-1) downto 0);
        datain1 	: in  std_logic_vector((dbits-1) downto 0);
        dataout1	: out std_logic_vector((dbits-1) downto 0);
        en1   		: in std_logic;
        we1     	: in  std_logic;
        address2	: in  std_logic_vector((abits-1) downto 0);
        datain2 	: in  std_logic_vector((dbits-1) downto 0);
        dataout2	: out std_logic_vector((dbits-1) downto 0);
        en2     	: in std_logic;
        we2     	: in  std_logic;
        address3	: in  std_logic_vector((abits-1) downto 0);
        datain3 	: in  std_logic_vector((dbits-1) downto 0);
        dataout3	: out std_logic_vector((dbits-1) downto 0);
        en3     	: in std_logic;
        we3     	: in  std_logic;
        req1    	: in std_logic;
        req2    	: in std_logic;
        req3    	: in std_logic;
        gnt1    	: out std_logic;
        gnt2    	: out std_logic;
        gnt3    	: out std_logic;
	--busy1		: out std_logic;
	--busy2		: out std_logic;
	--busy3		: out std_logic;
	evict_flag_IM 	: out std_logic;
	evict_data_IM	: out std_logic_vector((dbits-1) downto 0);
	rd_data_change1	: out std_logic;
	rd_data_change2	: out std_logic;
	rd_data_change3	: out std_logic
    );
end entity ram_2p;

architecture RTL of ram_2p is

type ram_2p_hs_type is record
   	gnt1 : std_logic;
   	gnt2 : std_logic;
   	gnt3 : std_logic;
	--busy1 : std_logic;
	--busy2 : std_logic;
	--busy3 : std_logic;
	ram_enable : std_logic;
	ram_addr : std_logic_vector((abits-1) downto 0);
	ram_write : std_logic;
	ram_data_wr : std_logic_vector((dbits-1) downto 0);
	dataout1 : std_logic_vector((dbits-1) downto 0);
	dataout2 : std_logic_vector((dbits-1) downto 0);
	dataout3 : std_logic_vector((dbits-1) downto 0);
	rd_data_change1 : std_logic;
	rd_data_change2 : std_logic;
	rd_data_change3 : std_logic;
end record;
constant ram_2p_hs_type_default : ram_2p_hs_type := ('0', '0', '0', '0', (others=>'0'), '0', (others=>'0'), (others=>'0'), (others=>'0'), (others=>'0'), '0', '0', '0');

signal r,t : ram_2p_hs_type := ram_2p_hs_type_default;

type state_type is (idle,ready,ping,pong);  --type of state machine.
signal current_s,next_s: state_type;
-- signal sel :  std_logic_vector(1 downto 0); -- if sel =00 then port1 is granted access if sel = 01 then port2  sel= 10 then for 3 port is granted the access
signal ram_addr : std_logic_vector((abits-1) downto 0);
signal ram_data_wr, ram_data_rd : std_logic_vector((dbits-1) downto 0); 
signal ram_enable, ram_write, busysig : std_logic;

begin
process (clk,rst)  
begin
    if(rst /= '1') then
		current_s <= idle;
    elsif(clk='1' and clk'Event) then
		current_s <= next_s;
		r <= t;
    end if;  
end process;

process(current_s, r, req1, req2, req3, en1, en2, en3, address1, address2, address3, datain1, datain2, datain3, we1, we2, we3)

variable v : ram_2p_hs_type;

begin
    v := r;

    -- set default values
    next_s <= current_s;

    case current_s is
        when idle =>
            if(req1 = '1') then -- priority to IM
                next_s <= ping;
				v.gnt1 := '1';
				--v.busy1 := '0';
				v.rd_data_change1 := '0'; --flip bit to indicate request complete
            elsif(req2 = '1') then
                next_s <= ping;
				v.gnt2 := '1';
				--v.busy2 := '0';
				v.rd_data_change2 := '0'; --flip bit to indicate request complete
            elsif(req3 = '1') then
                next_s <= ping;
				v.gnt3 := '1';
				--v.busy3 := '0';
				v.rd_data_change3 := '0'; --flip bit to indicate request complete
			else
                next_s <= idle;
            end if;

        when ping =>
            if r.gnt1 = '1' then
				--v.busy1 := '0';
				v.rd_data_change1 := '0'; --flip bit to indicate request complete
				if req1 = '0' then
					next_s <= idle;
					v.gnt1 := '0';
				elsif en1 = '1' then
					next_s <= pong;
				end if;
            elsif r.gnt2 = '1' then
				--v.busy2 := '0';
				v.rd_data_change2 := '0'; --flip bit to indicate request complete
				if req2 = '0' then
					next_s <= idle;
					v.gnt2 := '0';
				elsif en2 = '1' then
					next_s <= pong;
				end if;
            elsif r.gnt3 = '1' then
				--v.busy3 := '0';
				v.rd_data_change3 := '0'; --flip bit to indicate request complete
				if req3 = '0' then
					next_s <= idle;
					v.gnt3 := '0';
				elsif en3 = '1' then
					next_s <= pong;
				end if;
            else
                next_s <= idle; -- should never get here
            end if;

		when pong =>
			if r.gnt1 = '1' then
				--v.busy1 := '0';
				v.rd_data_change1 := '1'; --flip bit to indicate request complete
				if req1 = '0' then
					next_s <= idle;
					v.gnt1 := '0';
				elsif en1 = '1' then
					next_s <= ping;
				end if;
			elsif r.gnt2 = '1' then
				--v.busy2 := '0';
				v.rd_data_change2 := '1'; --flip bit to indicate request complete
				if req2 = '0' then
					next_s <= idle;
					v.gnt2 := '0';
				elsif en2 = '1' then
					next_s <= ping;
				end if;
			elsif r.gnt3 = '1' then
				--v.busy3 := '0';
				v.rd_data_change3 := '1'; --flip bit to indicate request complete
				if req3 = '0' then
					next_s <= idle;
					v.gnt3 := '0';
				elsif en3 = '1' then
					next_s <= ping;
				end if;
			else
				next_s <= idle; -- should never get here
			end if;

		when others =>
			next_s <= idle;
    end case;

gnt1 <= v.gnt1;
gnt2 <= v.gnt2;
gnt3 <= v.gnt3;
--busy1 <= v.busy1;
--busy2 <= v.busy2;
--busy3 <= v.busy3;
rd_data_change1 <= v.rd_data_change1;
rd_data_change2 <= v.rd_data_change2;
rd_data_change3 <= v.rd_data_change3;

t <= v;
end process;

ram_addr <=		
			address1 when r.gnt1 ='1' else
			address2 when r.gnt2 ='1' else
			address3 when r.gnt3 ='1' else
			(others => '0');

ram_data_wr <=		
			datain1 when r.gnt1 ='1' else
			datain2 when r.gnt2 ='1' else
			datain3 when r.gnt3 ='1' else
			(others => '0');

ram_enable  <=		
			en1 when r.gnt1 ='1' else
			en2 when r.gnt2 ='1' else
			en3 when r.gnt3 ='1' else
			'0';

ram_write  <=		
			we1 when r.gnt1 ='1' else
			we2 when r.gnt2 ='1' else
			we3 when r.gnt3 ='1' else
			'0';

dataout1 <=  ram_data_rd when r.gnt1 ='1' else (others => '0') ;

dataout2 <=  ram_data_rd when r.gnt2 ='1' else (others => '0') ;

dataout3 <=  ram_data_rd when r.gnt3 ='1' else (others => '0') ;

crm_syncram : entity techmap.syncram
    generic map(
        tech => tech,
        abits => abits,
        dbits => dbits
    )
    port map(
        clk => clk,
        address => ram_addr,
        datain => ram_data_wr,
        dataout => ram_data_rd,
        enable => ram_enable,
        write => ram_write
    );

end architecture RTL;
