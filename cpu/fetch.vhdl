library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.isa.all;

entity fetch is
	port (
		clock       : in std_logic;
		instruction : out t_instruction;
		address     : in t_address
	);
end fetch;

architecture behavioral of fetch is
	signal memory : t_memory_bank := (
		0 => OP_MOV_I & GPR0 & "0000000000000001",
		1 => OP_MOV_I & GPR1 & "0000000000000001",
		2 => OP_ADD   & GPR0 & GPR0 & GPR1 & "000000",
		3 => OP_MOV_I & PC   & "0000000000000001",
		others => (others => '0')
	);
begin
	process(clock)
	begin
		if (falling_edge(clock)) then
			instruction <= memory(to_integer(unsigned(address)));
		end if;
	end process;
end behavioral;
