-- Author:	
-- Description:	this module generates burst transfers. Connected to an instantiation of ahbmst
------------------------------------------------------------------------------
-- tile_interface 	IP
-- VENDOR:      		LIS
-- DEVICE:      		LIS_NA
-- VERSION:     		1.0
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
library grlib;
library work;
use grlib.amba.all;
use work.ina_package.all;
use grlib.stdlib.all;
library work;
use work.config.all;

entity burst_generator is
  port (
    rst_ahb             : in  std_ulogic;
    clk_ahb             : in  std_ulogic;
    dmai 			    : out ahb_dma_in_type;
    dmao 				: in ahb_dma_out_type	  
       	 );
end;

architecture burst_gen_arch of burst_generator is 


--FSM for reading data fifos 
type burst_generator_state_type is (IDLE,
									START_STATE,
                              WAIT_SPLIT,
                              INC_DATA,
                              WAIT_UNTIL_SPLIT,
                              START_BURST,
                              -- Reading states
                              wait_b4_load_req,
                              load_req,
                              load_req_granted,
                              wait_after_load
                              ------------------
 --                             
                      		 );
--FSM signals
constant start_address: std_logic_vector(AHBDW-1 downto 0):=(x"43000000");
signal burst_gen_state,burst_gen_state_temp :burst_generator_state_type; 
signal data_index_reg, data_index_sig,idle_count_sig,idle_count_reg: integer:=0; --range 0 to burst_size:=0;
signal data_sig,data_reg: integer:=1;
signal wait_count_sig,wait_count_reg,num:integer:=0;
signal dmai_address, dmai_wdata,dma_intermed_data: std_logic_vector(AHBDW-1 downto 0);
signal dmai_start: std_logic;

begin	

burst_gen_comb:process( rst_ahb,
                          	 burst_gen_state,
                          	 dmao,
                          	 data_index_reg,
                          	 data_reg, --added
                          	 wait_count_reg,
                          	 idle_count_reg          
                 		  )
variable burst_gen_state_var: burst_generator_state_type;
variable data_index: integer range 0 to burst_size:=1;
variable idle_count: integer:=0; --range 0 to burst_size:=0;
variable data: integer:=0;
variable wait_count: integer :=0;                      		
begin
-- Initial assignments
data:= data_reg;
burst_gen_state_var := burst_gen_state;
data_index:= data_index_reg;
wait_count:= wait_count_reg;
dmai.irq<='0';
dmai.size<= HSIZE_WORD;
dmai.busy<='0';
idle_count:= idle_count_reg;
--dmai_address<=start_address;
dmai_start<='0';

case burst_gen_state_var is
  	  
  when IDLE => 
  
    wait_count:=0;    
    dmai_address<=(others => '0');
       dmai.burst<='0';
  	   dmai_start<='0';
  	   data_index:=1;
  	   if rst_ahb = '1' then  
  	   idle_count:= idle_count+1;
  	   end if;
  	   if idle_count = 500 then
   	   		burst_gen_state_var:= START_STATE;
  	   end if;
  	   
  	   
 when START_STATE=>
 if burst_size>1 then
 	 dmai.burst<='1';
 	 else dmai.burst<='0';
 	 end if;
 	   idle_count:= 0;   	  
  	  dmai_start<='1';
  	  dmai_address<=start_address;
  	 
  	  dmai_wdata<= conv_std_logic_vector(data,AHBDW);
  	  dmai.write<='1';
  	 if dmao.active = '1' then 
  	  dmai_address(9 downto 0)<= dmao.haddr; -- inserted
  	  data:= data+1;
  	  dmai_wdata<= conv_std_logic_vector(data,AHBDW);
  	   if burst_size>1 then
 	   burst_gen_state_var:= START_BURST;-- change was made here  
 	  else  burst_gen_state_var:= WAIT_UNTIL_SPLIT;
 	 end if;  	 	  
  	end if; 	  

  	   
  when START_BURST =>
  if burst_size>1 then
 	 dmai.burst<='1';
 	 else dmai.burst<='0';
 	 end if;
 	   dmai_start<='1';
  	   dmai.write<='1';  	  	   
	   if (dmao.ready = '1') then
	   dmai_address(9 downto 0)<= dmao.haddr;
  	   data_index:= data_index+1; 
  	   data:= data+1; 
	   end if;	   
  	   dmai_wdata<= conv_std_logic_vector(data,AHBDW); 	   	   
  	   if data_index= burst_size then
  	   burst_gen_state_var:= WAIT_UNTIL_SPLIT;
  	   dmai_start<='1';	--start deasserted at the end of current burst transfer
  	   end if;
  	   
	 when WAIT_UNTIL_SPLIT =>
	 dmai_address<=start_address;	 
	 dmai.burst<='0';
	   dmai_start<='0';
	   wait_count:=wait_count+1;
	   if wait_count= 8 then
	   burst_gen_state_var:= WAIT_SPLIT;
	   end if;
  	   
	 when WAIT_SPLIT =>
	 wait_count:=0;
	 dmai.burst<='0';
  	   dmai_start<='0';  	  
  	   dmai.write<='1';  	   
  	 -- if dmao.active = '1' then -- if (dmao.active and (not dmao.retry) )='1'  then 
  	   burst_gen_state_var:= INC_DATA;   
  	 --  end if;
  	   
  	 when INC_DATA=>
  	 dmai.burst<='0';
  	 dmai_start<='0';
  	 num<=num+1;
  	 burst_gen_state_var:=wait_b4_load_req;
  	 
  	 
  	 ---------------------Reading part starts here----------------
  	 when wait_b4_load_req =>
  	 dmai.burst<='0';
  	 dmai_start <='0';
  	 dmai_address<=(others => '0');
  	 idle_count:= idle_count + 1;
  	 if idle_count = 500 then
   	   		burst_gen_state_var:= load_req;
  	 end if;
  	 
  	 when load_req =>
  	  if burst_size>1 then
 	 dmai.burst<='1';
 	 else dmai.burst<='0';
 	 end if;
  	 dmai_address<=start_address;
  	 dmai_start <='1';
  	 idle_count:= 0;
  	 dmai.write<='0';
  	   	  if dmao.active = '1' then 
  	    	  --burst_gen_state_var:= load_req_granted;-- change was made here  	
  	    	 burst_gen_state_var:= wait_after_load  ;
  	      end if; 
  	      
  	 when load_req_granted =>
  	 dmai_address<=start_address;
	  if burst_size>1 then
 	 dmai.burst<='1';
 	 else dmai.burst<='0';
 	 end if;
	   dmai_start<='0';
	   wait_count:=wait_count+1;
	   if wait_count= 2 then
	   burst_gen_state_var:= wait_after_load;
	   end if;
  	 
  	 when wait_after_load =>
  	 dmai_address<=(others => '0');
  	 wait_count:=0;
	 dmai.burst<='0';
  	 dmai_start<='0';  	  
  	 dmai.write<='0';  	   
  	 burst_gen_state_var:= IDLE;
               
	when others => 
	null;

end case;

if rst_ahb = '0' then  
burst_gen_state_var:= IDLE;
end if;

burst_gen_state_temp<= burst_gen_state_var;
data_index_sig<= data_index;
idle_count_sig<= idle_count;
wait_count_sig<= wait_count;
data_sig<=data;

end process burst_gen_comb;


--- Sequential process to assign values on each pos clk_ahb edge	
sequential: process (clk_ahb)
begin
if rising_edge(clk_ahb) then
dmai.address<= dmai_address;
dmai.start<=dmai_start;
dmai.wdata<=dmai_wdata;
burst_gen_state <= burst_gen_state_temp;
data_index_reg  <=data_index_sig;
idle_count_reg<= idle_count_sig;
wait_count_reg<= wait_count_sig;
data_reg<=data_sig;
end if;
end process sequential;

end burst_gen_arch;                   		 

