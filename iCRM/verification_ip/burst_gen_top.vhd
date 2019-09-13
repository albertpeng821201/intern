use STD.textio.all;
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use ieee.std_logic_textio.all;
library grlib;
library work;
use grlib.amba.all;
use grlib.stdlib.all;
use grlib.devices.all;
library icrm;
use icrm.all;


entity burst_gen_top is
generic
(
 hindex: integer :=0;
 my_tile_id : integer := 0;
 traffic_addr : std_logic_vector(31 downto 0);  -- global address only
 traffic_mask : std_logic_vector(31 downto 0)
 );
port 
	(
		clk_ahb: in std_logic;
		rst_ahb: in std_logic;
		ahb_out: out ahb_mst_out_type;
		ahb_in: in ahb_mst_in_type;
		crm_en    : in   std_ulogic
	);
end;

architecture behav of burst_gen_top is
--Signal decleration
--constant hindex: integer:=5; -- PUT THIS AS A GENERIC IN TOP MODULE LATER ON
signal dmao: ahb_dma_out_type; 
signal dmai : ahb_dma_in_type;--response from burst generator
signal idle_trans, hlock: std_ulogic;
component icrm_ahbmst
	generic (
    hindex  : integer := 0;
    hirq    : integer := 0;
    venid   : integer := VENDOR_GAISLER;
    devid   : integer := 0;
    version : integer := 0;
    chprot  : integer := 3;
    incaddr : integer := 0); 
   port (
      rst  : in  std_ulogic;
      clk  : in  std_ulogic;
      dmai : in ahb_dma_in_type;
      dmao : out ahb_dma_out_type;
      ahbi : in  ahb_mst_in_type;
      ahbo : out ahb_mst_out_type;
      idle_trans : in std_ulogic;
      hlock : in std_ulogic
      );
end component;

component burst_generator is
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
end component;


begin

idle_trans <= '0';
hlock <= '0';
UUT_BURST_GEN: burst_generator generic map (my_tile_id => my_tile_id, traffic_addr =>  traffic_addr, traffic_mask => traffic_mask) 
  port map(
    rst_ahb             => rst_ahb,
    clk_ahb             => clk_ahb,
    dmai 		=> dmai,
    dmao 		=> dmao,
    crm_en			=> crm_en     	  
     );
     
UUT_AMBA_MASTER: icrm_ahbmst
generic map ( hindex=> hindex, chprot => 15) 
	port map(
	 rst   => rst_ahb,
      clk  => clk_ahb,
      dmai => dmai,
      dmao => dmao,
      ahbi => ahb_in,
      ahbo => ahb_out,
      idle_trans => idle_trans,
      hlock => hlock
	);

end;
