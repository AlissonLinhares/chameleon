library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.isa.all;

entity decoder is
	port (
		clock        : in std_logic;
		instruction  : in t_instruction;
		imm_enable   : out std_logic  := '0';
		immediate    : out t_register := (others => '0');
		idx_a        : out t_rindex   := (others => '0');
		idx_b        : out t_rindex   := (others => '0');
		idx_c        : out t_rindex   := (others => '0');
		opcode       : out t_opcode   := ALU_NOP
	);
end decoder;

architecture behavioral of decoder is
begin
	process(clock)
		variable opcode_buffer : t_opcode := ALU_NOP;
	begin
		if rising_edge(clock) then
			case (instruction(INST_GFLAG_RANGE)) is
				when G0_FORMAT =>
					immediate  <= std_logic_vector(resize(signed(instruction(INST_IMM21_RANGE)),immediate'length));
					imm_enable <= '1';
				when G1_FORMAT =>
					immediate  <= std_logic_vector(resize(signed(instruction(INST_IMM16_RANGE)),immediate'length));
					imm_enable <= '1';
				when G2_FORMAT =>
					immediate  <= std_logic_vector(resize(signed(instruction(INST_IMM11_RANGE)),immediate'length));
					imm_enable <= '1';
				when G3_FORMAT =>
					immediate  <= std_logic_vector(resize(signed(instruction(INST_RESE_RANGE)) ,immediate'length));
					imm_enable <= '0';
				when others =>
					imm_enable <= '0';
					immediate  <= (others => '0');
			end case;

			opcode_buffer := instruction(INST_OPCODE_RANGE);
			idx_c <= instruction(INST_REG2_RANGE);
			idx_b <= instruction(INST_REG0_RANGE);
			idx_a <= instruction(INST_REG1_RANGE);
		elsif falling_edge(clock) then
			opcode <= opcode_buffer;
		end if;
	end process;
end;
