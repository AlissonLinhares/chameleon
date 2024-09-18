library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.isa.all;

entity cpu is
	port (
		clock : in std_logic := '0';
		data  : inout t_instruction
	);
end cpu;

architecture behavioral of cpu is
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
begin
	fetch_m: fetch port map( clock, instruction, address );
	decoder_m: decoder port map( clock, instruction, imm_enable, immediate, idx_a, idx_b, idx_c, opcode );
	cu_m: cu port map( clock, write, imm_enable, immediate, idx_a, idx_b, idx_c, reg_c, reg_a, reg_b, address );
	alu_m: alu port map( clock, opcode, reg_a, reg_b, reg_c, write );
end behavioral;
