library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.isa.all;

entity cu_tb is
	constant NUMBER_OF_REGISTERS : natural := ((2**t_rindex'length)-1);
end cu_tb;

architecture behavioral of cu_tb is
	component cu
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

			address : out t_address
		);
	end component cu;

	signal clock : std_logic;
	signal write : std_logic;
	signal imm_enable : std_logic;
	signal immediate  : t_register;
	signal idx_a   : t_rindex;
	signal idx_b   : t_rindex;
	signal idx_c   : t_rindex;
	signal reg_c   : t_register;
	signal reg_a   : t_register;
	signal reg_b   : t_register;
	signal address : t_address;
begin
	cu_m: cu port map( clock, write, imm_enable, immediate, idx_a, idx_b, idx_c, reg_c, reg_a, reg_b, address );

	process
		variable nTests : integer := 1;

		procedure tick is
		begin
			clock <= '0';
			wait for 1 ns;
			clock <= '1';
			wait for 1 ns;
			clock <= '0';
		end;

		procedure init is
		begin
			reg_c <= MAX_INT;
			write <= '1';
			imm_enable <= '0';
			immediate  <= (others => '0');

			for idx in 0 to NUMBER_OF_REGISTERS loop
				idx_c <= std_logic_vector(to_unsigned(idx, idx_c'length));
				tick;
			end loop;

			for idx in 0 to NUMBER_OF_REGISTERS loop
				idx_a <= std_logic_vector(to_unsigned(idx, idx_c'length));
				tick;
				assert (reg_a = MAX_INT) report "Initialization has failed.";
			end loop;
		end;

		procedure test( constant expect_a, expect_b : in t_register; constant data : t_register; constant a, b, c : in t_rindex; constant imm : in t_register; constant w,i : std_logic )  is
		begin
			reg_c <= data;
			idx_a <= a;
			idx_b <= b;
			idx_c <= c;
			write <= w;
			imm_enable <= i;
			immediate <= imm;
			tick;

			assert (( reg_a = expect_a ) and( reg_b = expect_b)) report "[T: " & integer'image(nTests) & "] Result value different from expected.";
			nTests := nTests + 1;
		end;
	begin
		init;
		test( MAX_INT, MAX_INT, ZERO   , GPR0, GPR0, GPR0, ZERO   , '1', '0');
		test( ZERO   , ZERO   , ZERO   , GPR0, GPR0, GPR1, ZERO   , '1', '0');
		test( MAX_INT, MAX_INT, ZERO   , GPR1, GPR1, GPR1, ZERO   , '0', '0');
		test( ZERO   , ZERO   , ZERO   , GPR1, GPR1, GPR1, ZERO   , '1', '0');
		test( MIN_INT, MAX_INT, MIN_INT, GPR1, GPR2, GPR3, ZERO   , '1', '0');
		test( MAX_INT, MIN_INT, MAX_INT, GPR5, GPR2, GPR3, MIN_INT, '1', '1');
		wait;
	end process;
end behavioral;
