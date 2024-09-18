library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.isa.all;

entity alu_tb is
end alu_tb;

architecture behavioral of alu_tb is
	component alu
		port (
			clock  : in std_logic;
			opcode : in t_opcode;
			reg_a  : in t_register;
			reg_b  : in t_register;
			reg_c  : out t_register;
			write  : out std_logic
		);
	end component alu;

	signal opcode : t_opcode;
	signal clock  : std_logic;
	signal reg_a  : t_register;
	signal reg_b  : t_register;
	signal reg_c  : t_register;
	signal write  : std_logic;
begin
	alu_m: alu port map( clock, opcode, reg_a, reg_b, reg_c, write );

	process
		variable nTests : integer := 1;
		procedure init is
		begin
			opcode <= ALU_NOP;
			clock  <= '0';
		end init;

		function to_string ( data: std_logic_vector) return string is
			variable result : string (1 to data'length) := (others => NUL);
		begin
			for i in data'range loop
				result(i+1) := std_logic'image(data(i))(2);
			end loop;

			return result;
		end function;

		procedure test( constant op: in std_logic_vector; a, b, expect_result: in t_register; expect_write: in std_logic ) is
		begin
			opcode <= op;
			reg_a <= a;
			reg_b <= b;

			wait for 10 ns;
			clock  <= '1';
			wait for 10 ns;
			clock  <= '0';

			assert (write = expect_write) report "[T:" & integer'image(nTests) & "] - Invalid Write output.";
			assert (reg_c = expect_result) report "[T:" & integer'image(nTests) & "], [OP: " & to_string(op) & "], [A:" & to_string(a) & "], [B:" & to_string(b) & "], [O: " & to_string(reg_c ) & "], [E: " & to_string(expect_result) & "];";

			nTests := nTests + 1;
		end test;

	begin
		init;
		test(ALU_AND  , MAX_UINT, MAX_UINT, MAX_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', others => '0'), '1');
		test(ALU_AND  , MIN_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_P => '1', ALU_FLAG_Z => '1', others => '0'), '1');
		test(ALU_AND  , MAX_UINT, MIN_UINT, MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_P => '1', ALU_FLAG_Z => '1', others => '0'), '1');
		test(ALU_AND  , MIN_UINT, MIN_UINT, MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_P => '1', ALU_FLAG_Z => '1', others => '0'), '1');

		test(ALU_NAND , MAX_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_P => '1', ALU_FLAG_Z => '1', others => '0'), '1');
		test(ALU_NAND , MIN_UINT, MAX_UINT, MAX_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', others => '0'), '1');
		test(ALU_NAND , MAX_UINT, MIN_UINT, MAX_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', others => '0'), '1');
		test(ALU_NAND , MIN_UINT, MIN_UINT, MAX_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', others => '0'), '1');

		test(ALU_OR, MAX_UINT, MAX_UINT, MAX_UINT, '1');
		test(ALU_OR, MIN_UINT, MAX_UINT, MAX_UINT, '1');
		test(ALU_OR, MAX_UINT, MIN_UINT, MAX_UINT, '1');
		test(ALU_OR, MIN_UINT, MIN_UINT, MIN_UINT, '1');

		test(ALU_NOR, MAX_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_NOR, MIN_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_NOR, MAX_UINT, MIN_UINT, MIN_UINT, '1');
		test(ALU_NOR, MIN_UINT, MIN_UINT, MAX_UINT, '1');

		test(ALU_XOR, MAX_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_XOR, MIN_UINT, MAX_UINT, MAX_UINT, '1');
		test(ALU_XOR, MAX_UINT, MIN_UINT, MAX_UINT, '1');
		test(ALU_XOR, MIN_UINT, MIN_UINT, MIN_UINT, '1');

		test(ALU_XNOR, MAX_UINT, MAX_UINT, MAX_UINT, '1');
		test(ALU_XNOR, MIN_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_XNOR, MAX_UINT, MIN_UINT, MIN_UINT, '1');
		test(ALU_XNOR, MIN_UINT, MIN_UINT, MAX_UINT, '1');

		test(ALU_SHL  , MAX_UINT, (reg_a'right => '1', others => '0'), (reg_a'right => '0', others => '1'), '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_P => '1', ALU_FLAG_S => '1', ALU_FLAG_C => '1', others => '0'), '1');
		test(ALU_SHL  , MAX_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_Z => '1', ALU_FLAG_P => '1', others => '0'), '1');
		test(ALU_SHL  , (reg_a'right => '1', others => '0'), (GPR_BASE_LEN => '1', others => '0'), MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_Z => '1', ALU_FLAG_C => '1', ALU_FLAG_P => '1', others => '0'), '1');

		test(ALU_SHR  , MAX_UINT, (reg_a'right => '1', others => '0'), (reg_a'left => '0', others => '1'), '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_C => '1', others => '0'), '1');
		test(ALU_SHR  , MIN_UINT, MAX_UINT, MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_Z => '1', ALU_FLAG_P => '1', others => '0'), '1');
		test(ALU_SHR  , (reg_a'left => '1', others => '0'), (GPR_BASE_LEN => '1', others => '0'), MIN_UINT, '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_Z => '1', ALU_FLAG_C => '1', ALU_FLAG_P => '1', others => '0'), '1');

		test(ALU_STORS, MIN_UINT, MIN_UINT, MIN_UINT, '0');

		test(ALU_ROL  , (reg_a'left => '1',others => '0'), (GPR_BASE_LEN => '1', others => '0'), (reg_a'left => '1',others => '0'), '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', ALU_FLAG_P => '1', others => '0'), '1');
		test(ALU_ROL  , (reg_a'left => '1',others => '0'), (reg_a'right => '1', others => '0'), (reg_a'right => '1',others => '0'), '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, MIN_UINT, '1');

		test(ALU_ROR  , (reg_a'left => '1',others => '0'), (GPR_BASE_LEN => '1', others => '0'), (reg_a'left => '1',others => '0'), '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', ALU_FLAG_P => '1', others => '0'), '1');
		test(ALU_ROR  , (reg_a'right => '1',others => '0'), (reg_a'right => '1', others => '0'), (reg_a'left => '1',others => '0'), '1');
		test(ALU_LOADS, MIN_UINT, MIN_UINT, (ALU_FLAG_S => '1', ALU_FLAG_P => '1', others => '0'), '1');

		test(ALU_CMP  , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVE , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVNP, MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVNS, MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVG , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVGE, MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVB , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVBE, MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVLE, MAX_UINT, MAX_INT , MAX_INT, '1');

		test(ALU_CMP  , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVL , MAX_UINT, MAX_INT , MAX_INT, '1');
		test(ALU_MOVNP, MAX_UINT, ZERO    , ZERO, '1');
		test(ALU_MOVP , MAX_UINT, MIN_INT , MIN_INT, '1');
		test(ALU_MOVS , MAX_UINT, ZERO    , ZERO, '1');
		test(ALU_MOVNS, MAX_UINT, ZERO    , ZERO, '1');

		test(ALU_CMP  , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVA , MAX_UINT, MAX_INT , MAX_INT, '1');

		test(ALU_CMP  , MAX_UINT, MAX_INT , ZERO, '0');
		test(ALU_MOVAE, MAX_UINT, MAX_INT , MAX_INT, '1');

		test(ALU_ABS  , ZERO    , MAX_UINT, ONE, '1');
		test(ALU_MOVP , MAX_UINT, MIN_INT , ZERO, '0');
		test(ALU_MOVS , MAX_UINT, MIN_INT , ZERO, '0');

		test(ALU_ABS  , ZERO    , MAX_INT , MAX_INT, '1');
		test(ALU_MOVP , MAX_UINT, MIN_INT , ZERO, '0');
		test(ALU_MOVS , MAX_UINT, MIN_INT , ZERO, '0');

		test(ALU_NEG  , ZERO    , MAX_UINT, ONE, '1');
		test(ALU_NEG  , ZERO    , ONE     , MAX_UINT, '1');
		test(ALU_MOVNS, MAX_UINT, MIN_INT , ZERO, '0');

		test(ALU_NOT  , ZERO    , ZERO    , MAX_UINT, '1');
		test(ALU_NOT  , ZERO    , MAX_UINT, ZERO    , '1');
		test(ALU_NOT  , ZERO    , MAX_INT , MIN_INT , '1');

		test(ALU_ADD  , ZERO    , ONE     , ONE, '1');
		test(ALU_MOVP , MAX_UINT, MIN_INT , ZERO, '0');
		test(ALU_MOVS , MAX_UINT, MIN_INT , ZERO, '0');

		test(ALU_SUB  , ZERO    , ONE     , MAX_UINT, '1');
		test(ALU_MOVNS, MAX_UINT, MIN_INT , ZERO, '0');
		test(ALU_MOVP , MAX_UINT, MIN_INT , ZERO, '0');
		test(ALU_MOVL , MAX_UINT, MIN_INT , MIN_INT, '1');

		test(ALU_SUB  , MIN_INT , ONE     , MAX_INT, '1');
		test(ALU_MOVO , MAX_UINT, MIN_INT , MIN_INT, '1');
		test(ALU_ADD  , MAX_INT , ONE     , MIN_INT, '1');
		test(ALU_MOVO , MAX_UINT, MIN_INT , MIN_INT, '1');

		test(ALU_NOP, MAX_UINT, MAX_UINT, ZERO, '0');
		wait;
	end process;
end behavioral;
