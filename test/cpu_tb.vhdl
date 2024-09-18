library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.isa.all;
use work.debug.all;

entity cpu_tb is
end cpu_tb;

architecture behavioral of cpu_tb is
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

	component decoder
		port (
			clock        : in std_logic;
			instruction  : in t_instruction;
			imm_enable   : out std_logic;
			immediate    : out t_register;
			idx_a        : out t_rindex;
			idx_b        : out t_rindex;
			idx_c        : out t_rindex;
			opcode       : out t_opcode
		);
	end component decoder;

	component fetch
		port (
			clock       : in std_logic;
			instruction : out t_instruction;
			address     : in t_address
		);
	end component fetch;

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

	signal instruction : t_instruction;
	signal idx_a       : t_rindex;
	signal idx_b       : t_rindex;
	signal idx_c       : t_rindex;
	signal reg_a       : t_register;
	signal reg_b       : t_register;
	signal reg_c       : t_register;
	signal reg_o       : t_register;
	signal opcode      : t_opcode;
	signal write       : std_logic;
	signal immediate   : t_register;
	signal imm_enable  : std_logic;
	signal address     : t_address;
	signal clock       : std_logic;
begin
	decoder_m: decoder port map( clock, instruction, imm_enable, immediate, idx_a, idx_b, idx_c, opcode );
	cu_m: cu port map( clock, write, imm_enable, immediate, idx_a, idx_b, idx_c, reg_c, reg_a, reg_b, address );
	alu_m: alu port map( clock, opcode, reg_a, reg_b, reg_c, write );

	process
		procedure tick is
		begin
			clock <= '0';
			wait for 2 ns;
			clock <= '1';
			wait for 2 ns;
		end;

		procedure test( constant inst : in t_instruction; constant expect_reg_c : in t_register; constant expected_pc : in integer ) is
		begin
			instruction <= inst;
			tick;
			print(address);
			assert ((reg_c = expect_reg_c) and ( to_integer(signed(address)) = expected_pc));
		end;
	begin
		test( OP_MOV_I & GPR0 & "0000000000000000", ZERO, 0 );
		test( OP_ADD_I & GPR0 & GPR0 & "00000000001", ZERO, 1 );
		test( OP_SUB_I & GPR0 & GPR0 & "00000000010", ONE, 2 );
		test( OP_NOT   & GPR0 & GPR0 & GPR0 & "101010", MAX_UINT, 3 );
		test( OP_MOV_I & GPR1 & "0000000000000001", ZERO, 4 );
		test( OP_SUB   & GPR0 & GPR0 & GPR1 & "101010", ONE, 5 );
		test( OP_ADD   & GPR0 & GPR0 & GPR1 & "101010", MAX_UINT, 6 );
		test( OP_XOR   & PC   & PC   & PC   & "000000", ZERO, 7 );
		test( OP_NOP   & "000000000000000000000", ZERO, 8 );
		test( OP_NOP   & "000000000000000000000", ZERO, 0 );
		wait;
	end process;
end;
