-- Author:	
-- Description:	this module generates burst transfers. Connected to an instantiation of ahbmst
------------------------------------------------------------------------------
-- tile_interface 	IP
-- VENDOR:      		LIS
-- DEVICE:      		LIS_NA
-- VERSION:     		1.0
-------------------------------------------------------------------------------
use STD.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_textio.all;
USE ieee.numeric_std.ALL;
library grlib;
library work;
use grlib.amba.all;
use grlib.stdlib.all;


entity burst_generator is
	  generic (
    my_tile_id : integer := 0;
    traffic_addr : std_logic_vector(31 downto 0);  -- global address only
    traffic_mask : std_logic_vector(31 downto 0) );

  port (
    rst_ahb             : in  std_ulogic;
    clk_ahb             : in  std_ulogic;
    dmai 		: out ahb_dma_in_type;
    dmao 		: in ahb_dma_out_type;
    crm_en 			: in   std_ulogic
       	 );
end;

architecture burst_gen_arch of burst_generator is 

type state is (s0, s1, s2, s3, s4, s5, s6, s7);
signal current_s, next_s : state;
signal rand_addr, rand_data, myaddr, mydata : std_logic_vector(31 downto 0);
signal flag : std_ulogic := '0'; -- flag => 1 => address is in local TLM



component random  is
generic ( width : integer :=  32 ); 

port (
      clk : in std_logic;
      random_num : out std_logic_vector (width-1 downto 0)   --output vector            
    );
end component;

begin

flag <= '1' when to_integer(unsigned(traffic_addr(29 downto 24))) = my_tile_id else
	'0' when to_integer(unsigned(traffic_addr(29 downto 24))) /= my_tile_id;

dmai.burst <= '0';
dmai.irq <= '0';
dmai.size <= "010";
dmai.busy <= '0';
dmai.wdata <= mydata;
mydata <= rand_data;
dmai.address <= myaddr;
myaddr <= traffic_addr or ((not traffic_mask ) and rand_addr) when flag = '0' else
		(x"80" & traffic_addr(23 downto 0)) or ((not traffic_mask ) and rand_addr) when flag = '1';
process1: process (clk_ahb)
  begin  

	if(clk_ahb='1' and clk_ahb'Event) then
		if  (rst_ahb /= '1') then
		--flag <= '0';
			current_s <= s0;
		else

			current_s <= next_s;

		end if;
	end if;
  end process process1; 

process(current_s, crm_en, dmao.ready)
	 variable L : line; 
	begin

	case current_s is
		when s0 => if(crm_en = '1') then
			next_s <= s1;
									
			else
			next_s <= s0;
			
			end if;
		when s1 =>  if(dmao.ready = '1' ) then
				deallocate (L);
				write(L, string'("Tile ")); 
				write(L, my_tile_id);
				write(L, string'(" value load at address ")); 
				write(L, myaddr);
				write(L, string'(" is "));
				write(L, dmao.rdata);
				report L.all severity WARNING; 
				next_s <= s2;
			    else
				next_s <= s1;
			    end if;

		when s2 => if(dmao.ready = '1' ) then
				deallocate (L);
				write(L, string'("Tile ")); 
				write(L, my_tile_id);
				write(L, string'(" value store at address ")); 
				write(L, myaddr);
				write(L, string'(" is "));
				write(L, mydata);
				report L.all severity WARNING; 
				next_s <= s0;
			    else
				next_s <= s2;
			    end if;
		
		when others => next_s <= s0;
	end case;
end process;


process(current_s)
	begin
	case current_s is
		when s0 => dmai.start <= '0';
			   dmai.write <= '0';
		when s1 => dmai.start <= '1';
			   dmai.write <= '0';
		when s2 => dmai.start <= '1';
			   dmai.write <= '1';
		when others => dmai.start <= '0';
			   dmai.write <= '0';
	end case;
end process;
	

address_random : random generic map ( width => 32 )
port map (clk_ahb, rand_addr);

data_random : random generic map ( width => 32 )
port map (clk_ahb, rand_data);

end architecture;
