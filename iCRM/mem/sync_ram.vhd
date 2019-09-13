library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.Numeric_Std.all;

entity sync_ram is
	generic (
		indexbits	: positive;
		tagbits		: positive;
		dbits		: positive;
		depth		: positive
	);

	port (
		clk			: in  std_logic;
		ram_address	: in  std_logic_vector((indexbits-1) downto 0);
		ram_data_in	: in  std_logic_vector((dbits + tagbits) downto 0);
		ram_data_out: out std_logic_vector((dbits + tagbits) downto 0);
		ram_wr_en	: in  std_logic
	);
end entity sync_ram;

architecture RTL of sync_ram is

type ram_type is array (0 to depth-1) of std_logic_vector((dbits + tagbits) downto 0); -- no -1 because +1 for valid bit
--signal ram_probe : ram_type;

begin

RamProc: process(clk) is

variable ram : ram_type := (others=>(others=>'0'));

begin

	if rising_edge(clk) then
		if ram_wr_en = '1' then
			ram(to_integer(unsigned(ram_address))) := ram_data_in;
			ram_data_out <= (others=>'0');
		else
			ram_data_out <= ram(to_integer(unsigned(ram_address)));
		end if;
		--ram_probe <= ram;
	end if;

end process RamProc;

end architecture RTL;
