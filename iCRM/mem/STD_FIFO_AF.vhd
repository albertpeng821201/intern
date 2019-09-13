library IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;
library icrm;
use icrm.crm_pkg.all;
use icrm.all;

entity STD_FIFO_AF is
        Generic (
                constant DATA_WIDTH : positive := 8;
                constant FIFO_DEPTH : positive := 256;
				constant CRITICAL_LEVEL      : positive := 256;
                constant SAFE_LEVEL          : positive := 1
        );
        Port (
                CLK                 : in  STD_LOGIC;
                RST                 : in  STD_LOGIC;
                WriteEn             : in  STD_ULOGIC;
                DataIn              : in  STD_LOGIC_VECTOR(DATA_WIDTH - 1 downto 0);
                ReadEn              : in  STD_ULOGIC;
                DataOut             : out STD_LOGIC_VECTOR(DATA_WIDTH - 1 downto 0);
                Empty               : out STD_ULOGIC;
                Full                : out STD_ULOGIC;
                Almost_full         : out STD_ULOGIC;
				FifoLevel			: out STD_LOGIC_VECTOR(15 downto 0);
                fifoDepth           : in natural;
                fifoCriticalLevel   : in natural;
                fifoSafeLevel       : in natural
        );
end STD_FIFO_AF;

architecture Behavioral of STD_FIFO_AF is

signal Almost_full_sig : std_ulogic;
signal FifoLevel_sig : std_logic_vector(15 downto 0);

begin

FifoLevel <= FifoLevel_sig;
Almost_full <= Almost_full_sig;

-- Memory Pointer Process
-- FIFO_DEPTH is the synthesized fifo depth
-- fifoDepth is the used fifo depth which is configurable
fifo_proc : process (CLK)
type FIFO_Memory is array (0 to FIFO_DEPTH - 1) of STD_LOGIC_VECTOR(DATA_WIDTH - 1 downto 0);
variable Memory : FIFO_Memory;

variable Head : natural range 0 to FIFO_DEPTH - 1;
variable Tail : natural range 0 to FIFO_DEPTH - 1;
variable count : natural range 0 to FIFO_DEPTH - 1;
variable Looped : boolean;
begin
	if rising_edge(CLK) then
		if RST /= '1' then -- active low
			Head := 0;
			Tail := 0;

			Looped := false;
			FifoLevel_sig <= (others => '0');
			Almost_full_sig <= '0';
			Full  <= '0';
			Empty <= '1';
		else
			if(Looped = false) then
					count := Head - Tail;
			else
					count := Head + fifoDepth - 1 - Tail;
			end if;
			FifoLevel_sig <= std_logic_vector(to_unsigned(count,FifoLevel_sig'length));

			if(count >= fifoCriticalLevel) then
					Almost_full_sig <= '1';
			elsif(count <= fifoSafeLevel) then
					Almost_full_sig <= '0';
			end if;

			if (ReadEn = '1') then
				if ((Looped = true) or (Head /= Tail)) then
					-- Update data output
					DataOut <= Memory(Tail);

					-- Update Tail pointer as needed
					if (Tail = fifoDepth - 1) then
							Tail := 0;

							Looped := false;
					else
							Tail := Tail + 1;
					end if;
				end if;
			end if;

			if (WriteEn = '1') then
				if ((Looped = false) or (Head /= Tail)) then
					-- Write Data to Memory
					Memory(Head) := DataIn;

					-- Increment Head pointer as needed
					if (Head = fifoDepth - 1) then
							Head := 0;

							Looped := true;
					else
							Head := Head + 1;
					end if;
				end if;
			end if;

			-- Update Empty and Full flags
			if (Head = Tail) then
				if Looped then
						Full <= '1';
				else
						Empty <= '1';
				end if;
			else
				Empty	<= '0';
				Full	<= '0';
			end if;
		end if;
	end if;
end process;

end Behavioral;
