library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.isa.all;
use work.debug.all;

entity cu is
	port (
		clock : in std_logic;
		write : in std_logic;

		imm_enable : in std_logic;
		immediate  : in t_register;

		idx_a : in t_rindex;
		idx_b : in t_rindex;
		idx_c : in t_rindex;

		reg_c : in  t_register;
		reg_a : out t_register;
		reg_b : out t_register;

		address : out t_address := (others => '0')
	);
end cu;

architecture behavioral of cu is
	signal registers : t_register_file := (others => (others => '0'));

	impure function get_register( idx : in t_rindex ) return t_register is
	begin
		return registers(to_integer(unsigned(idx)));
	end get_register;

	procedure set_register( signal registers : out t_register_file; constant idx : t_rindex; constant value : t_register ) is
	begin
		registers(to_integer(unsigned(idx))) <= value;
	end set_register;

begin
	process(clock)
		variable previous_idx : t_rindex := GPR0;
		variable imm_register : t_register := (others => '0');
	begin
		if falling_edge(clock) then
			-- we need to update pc every cycle.
			registers(to_integer(unsigned(PC))) <= std_logic_vector(unsigned(get_register(PC)) + 1);

			reg_a <= get_register(idx_a);
			reg_b <= get_register(idx_b);

			if write = '1' then
				set_register(registers, previous_idx, reg_c);

				-- Bypass the input signal
				if idx_a = previous_idx then
					reg_a <= reg_c;
				end if;

				if idx_b = previous_idx then
					reg_b <= reg_c;
				end if;
			end if;

			if imm_enable = '1' then
				-- saving the immediate value in a special register and output the data in the reg_b data channel.
				imm_register := immediate;
				reg_b <= imm_register;
			end if;

			previous_idx := idx_c;
		elsif rising_edge(clock) then
			address <= std_logic_vector(resize(unsigned(get_register(PC)), t_address'length));
		end if;
	end process;
end behavioral;
