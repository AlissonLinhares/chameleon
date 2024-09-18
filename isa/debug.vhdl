library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.isa.all;

package debug is
	procedure print( constant data: std_logic );
	procedure print( constant data: std_logic_vector );
	procedure print_inst( constant data: t_instruction );
end;

package body debug is
	procedure print( constant data: std_logic ) is
	begin
		report std_logic'image(data);
	end procedure;

	procedure print( constant data: std_logic_vector ) is
		variable result : string (data'length downto 1) := (others => NUL);
	begin
		for i in data'range loop
			result(i + 1) := std_logic'image(data(i))(2);
		end loop;

		report result;
	end procedure;

	procedure print_inst( constant data: t_instruction ) is
	begin
		case data(t_operation'range) is
			when OP_SYS     => report "SYS";
			when OP_LOAD    => report "LOAD";
			when OP_STORE   => report "STORE";
			when OP_CMP     => report "CMP";
			when OP_AND     => report "AND";
			when OP_NAND    => report "NAND";
			when OP_OR      => report "OR";
			when OP_NOR     => report "NOR";
			when OP_NOT     => report "NOT";
			when OP_XOR     => report "XOR";
			when OP_XNOR    => report "XNOR";
			when OP_SHL     => report "SHL";
			when OP_SHR     => report "SHR";
			when OP_ROL     => report "ROL";
			when OP_ROR     => report "ROR";
			when OP_NEG     => report "NEG";
			when OP_ADD     => report "ADD";
			when OP_SUB     => report "SUB";
			when OP_MUL     => report "MUL";
			when OP_MULU    => report "MULU";
			when OP_DIV     => report "DIV";
			when OP_DIVU    => report "DIVU";
			when OP_MOD     => report "MOD";
			when OP_ABS     => report "ABS";
			when OP_MOV     => report "MOV";
			when OP_MOVE    => report "MOVE";
			when OP_MOVNE   => report "MOVNE";
			when OP_MOVS    => report "MOVS";
			when OP_MOVNS   => report "MOVNS";
			when OP_MOVO    => report "MOVO";
			when OP_MOVNO   => report "MOVNO";
			when OP_MOVP    => report "MOVP";
			when OP_MOVNP   => report "MOVNP";
			when OP_MOVA    => report "MOVA";
			when OP_MOVBE   => report "MOVBE";
			when OP_MOVAE   => report "MOVAE";
			when OP_MOVB    => report "MOVB";
			when OP_MOVG    => report "MOVG";
			when OP_MOVLE   => report "MOVLE";
			when OP_MOVGE   => report "MOVGE";
			when OP_MOVL    => report "MOVL";
			when OP_LOADS   => report "LOADS";
			when OP_STORS   => report "STORS";
			when OP_NOP     => report "NOP";
			when OP_SYS_I   => report "SYS_I";
			when OP_LOAD_I  => report "LOAD_I";
			when OP_STORE_I => report "STORE_I";
			when OP_CMP_I   => report "CMP_I";
			when OP_AND_I   => report "AND_I";
			when OP_NAND_I  => report "NAND_I";
			when OP_OR_I    => report "OR_I";
			when OP_NOR_I   => report "NOR_I";
			when OP_NOT_I   => report "NOT_I";
			when OP_XOR_I   => report "XOR_I";
			when OP_XNOR_I  => report "XNOR_I";
			when OP_SHL_I   => report "SHL_I";
			when OP_SHR_I   => report "SHR_I";
			when OP_ROL_I   => report "ROL_I";
			when OP_ROR_I   => report "ROR_I";
			when OP_NEG_I   => report "NEG_I";
			when OP_ADD_I   => report "ADD_I";
			when OP_SUB_I   => report "SUB_I";
			when OP_MUL_I   => report "MUL_I";
			when OP_MULU_I  => report "MULU_I";
			when OP_DIV_I   => report "DIV_I";
			when OP_DIVU_I  => report "DIVU_I";
			when OP_MOD_I   => report "MOD_I";
			when OP_ABS_I   => report "ABS_I";
			when OP_MOV_I   => report "MOV_I";
			when OP_MOVE_I  => report "MOVE_I";
			when OP_MOVNE_I => report "MOVNE_I";
			when OP_MOVS_I  => report "MOVS_I";
			when OP_MOVNS_I => report "MOVNS_I";
			when OP_MOVO_I  => report "MOVO_I";
			when OP_MOVNO_I => report "MOVNO_I";
			when OP_MOVP_I  => report "MOVP_I";
			when OP_MOVNP_I => report "MOVNP_I";
			when OP_MOVA_I  => report "MOVA_I";
			when OP_MOVBE_I => report "MOVBE_I";
			when OP_MOVAE_I => report "MOVAE_I";
			when OP_MOVB_I  => report "MOVB_I";
			when OP_MOVG_I  => report "MOVG_I";
			when OP_MOVLE_I => report "MOVLE_I";
			when OP_MOVGE_I => report "MOVGE_I";
			when OP_MOVL_I  => report "MOVL_I";
			when OP_STORS_I => report "STORS_I";
			when others  => report "UNKNOWN";
		end case;
	end procedure;
end package body;
