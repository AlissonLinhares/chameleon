library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.isa.all;

entity alu is
	port (
		clock  : in std_logic := '0';
		opcode : in t_opcode := ALU_NOP;
		reg_a  : in t_register;
		reg_b  : in t_register;
		reg_c  : out t_register;
		write  : out std_logic
	);
end alu;

architecture behavioral of alu is
	signal status : t_status := (others => '0');

	-- @Description: logical "and".
	-- @Flags: Z, S, P
	procedure op_and( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := a and b;
	end op_and;

	-- @Description: inverted "and" operation.
	-- @Flags: Z, S, P
	procedure op_nand( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := a nand b;
	end op_nand;

	-- @Description: logical inclusive "or".
	-- @Flags: Z, S, P
	procedure op_or( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := a or b;
	end op_or;

	-- @Description: inverted "or" operation.
	-- @Flags: Z, S, P
	procedure op_nor( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := a nor b;
	end op_nor;

	-- @Description: logical "not".
	-- @Flags: Z, S, P
	procedure op_not( variable result : out t_register; signal a : in t_register ) is
	begin
		result := not a;
	end op_not;

	-- @Description: logical exclusive "or".
	-- @Flags: Z, S, P
	procedure op_xor( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := a xor b;
	end op_xor;

	-- @Description: inverted exclusive "or".
	-- @Flags: Z, S, P
	procedure op_xnor( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := a xnor b;
	end op_xnor;

	-- @Description: shifts the bits in "a" to left "b" times. The Carry Flag contains the last bit shifted out.
	-- @Flags: C,Z,S,P
	procedure op_shl( variable result : out t_register; signal a, b: in t_register; signal status : out t_status ) is
		variable cache : std_logic_vector (result'length downto 0);
	begin
		cache := std_logic_vector(('0' & signed(a)) sll to_integer(resize(unsigned(b),GPR_BASE_LEN + 1)));
		status(ALU_FLAG_C) <= cache(cache'left);
		result := cache(result'range);
	end op_shl;

	-- @Description: shifts the bits in "a" to right "b" times. The Carry Flag contains the last bit shifted out.
	-- @Flags: C,Z,S,P
	procedure op_shr( variable result : out t_register; signal a, b: in t_register; signal status : out t_status ) is
		variable cache : std_logic_vector (result'length downto 0);
	begin
		cache := std_logic_vector((signed(a) & '0') srl to_integer(resize(unsigned(b),GPR_BASE_LEN + 1)));
		status(ALU_FLAG_C) <= cache(cache'right);
		result := cache(result'length downto 1);
	end op_shr;

	-- @Description: rotates bits in "a" to the left "b" times. All data pushed out to the left side re-entering on the right side.
	-- @Flags: Z,S,P
	procedure op_rol( variable result : out t_register; signal a, b : in t_register ) is
	begin
		result := std_logic_vector(signed(a) rol to_integer(resize(unsigned(b),GPR_BASE_LEN + 1)));
	end op_rol;

	-- @Description: rotates bits in "a" to the right "b" times. All data pushed out to the right side re-entering on the left side.
	-- @Flags: Z,S,P
	procedure op_ror( variable result : out t_register; signal a, b: in t_register ) is
	begin
		result := std_logic_vector(signed(a) ror to_integer(resize(unsigned(b),GPR_BASE_LEN + 1)));
	end op_ror;

	-- @Description: two's complement negation (-a).
	-- @Flags: Z,S,P
	procedure op_neg( variable result : out t_register; signal a: in t_register ) is
	begin
		result := std_logic_vector(unsigned(not(a)) + 1);
	end op_neg;

	-- @Description: arithmetic addition. (TODO: optimize this procedure.)
	-- @Flags: O,C,Z,S,P
	procedure op_add( variable result : out t_register; signal a, b: in t_register; signal status : out t_status ) is
		variable cache : std_logic_vector (result'length downto 0);
		variable total : t_register;
	begin
		cache := std_logic_vector(resize(unsigned(a),a'length + 1) + resize(unsigned(b),b'length + 1));
		total := std_logic_vector(resize(unsigned(cache),total'length));

		status(ALU_FLAG_C) <= cache(cache'left);
		status(ALU_FLAG_O) <= (a(a'left) and b(b'left) and (not(total(total'left)))) or (not(a(a'left)
					or b(b'left)) and total(total'left));

		result := total;
	end op_add;

	-- @Description: arithmetic subtraction. (TODO: optimize this procedure to use the add circuit.)
	-- @Flags: O,C,Z,S,P
	procedure op_sub( variable result : out t_register; signal a, b: in t_register; signal status : out t_status ) is
		variable cache : std_logic_vector (result'length downto 0);
		variable total : t_register;
	begin
		cache := std_logic_vector(resize(unsigned(a),a'length + 1) - resize(unsigned(b),b'length + 1));
		total := std_logic_vector(resize(unsigned(cache),total'length));

		status(ALU_FLAG_C) <= cache(cache'left);
		status(ALU_FLAG_O) <= (a(a'left) and (not b(b'left)) and (not(total(total'left)))) or (not(a(a'left) or (not b(b'left))) and total(total'left));

		result := total;
	end op_sub;

	-- @Description: Subtracts a from b and updates the status flags.
	-- @Flags: C, Z, S, P, O
	procedure op_cmp( variable result : out t_register; signal a, b: in t_register; signal status : out t_status; variable wflag : out std_logic ) is
	begin
		op_sub( result, a, b, status ); wflag := '0';
	end op_cmp;

	-- @Description: halfword arithmetic signed multiplication.
	-- @Flags: Z, S, P
	procedure op_mul( variable result : out t_register; signal a : in t_register; signal b : in t_register; signal status : out t_status ) is
	begin
		result := std_logic_vector(resize(signed(a),GPR_HALF_WORD) * resize(signed(b),GPR_HALF_WORD));
	end op_mul;

	-- @Description: halfword arithmetic unsigned multiplication.
	-- @Flags: Z, S, P
	procedure op_mulu( variable result : out t_register; signal a : in t_register; signal b : in t_register; signal status : out t_status ) is
	begin
		result := std_logic_vector(resize(unsigned(a),GPR_HALF_WORD) * resize(unsigned(b),GPR_HALF_WORD));
	end op_mulu;

	-- @Description: arithmetic signed division.
	-- @Flags: Z, S, P
	procedure op_div( variable result : out t_register; signal a : in t_register; signal b : in t_register ) is
	begin
		result := std_logic_vector(signed(a) / signed(b));
	end op_div;

	-- @Description: arithmetic unsigned division.
	-- @Flags: Z, S, P
	procedure op_divu( variable result : out t_register; signal a : in t_register; signal b : in t_register ) is
	begin
		result := std_logic_vector(unsigned(a) / unsigned(b));
	end op_divu;

	procedure op_mod( variable result : out t_register; signal a : in t_register; signal b : in t_register ) is
	begin
		result := std_logic_vector(unsigned(a) rem unsigned(b));
	end op_mod;

	procedure op_abs( variable result : out t_register; signal b : in t_register ) is
	begin
		result := std_logic_vector(abs signed(b));
	end op_abs;

	procedure op_mov( variable result : out t_register; signal b : in t_register ) is
	begin
		result := b;
	end op_mov;

	procedure op_move( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_Z);
	end op_move;

	procedure op_movne( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := not status(ALU_FLAG_Z);
	end op_movne;

	procedure op_movs( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_S);
	end op_movs;

	procedure op_movns( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := not status(ALU_FLAG_S);
	end op_movns;

	procedure op_movo( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_O);
	end op_movo;

	procedure op_movno( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := not status(ALU_FLAG_O);
	end op_movno;

	procedure op_movp( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_P);
	end op_movp;

	procedure op_movnp( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := not status(ALU_FLAG_P);
	end op_movnp;

	procedure op_mova( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := (not status(ALU_FLAG_C)) and (not status(ALU_FLAG_Z));
	end op_mova;

	procedure op_movbe( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_C) or status(ALU_FLAG_Z);
	end op_movbe;

	procedure op_movae( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := not status(ALU_FLAG_C);
	end op_movae;

	procedure op_movb( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_C);
	end op_movb;

	procedure op_movg( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := (not status(ALU_FLAG_Z)) and (status(ALU_FLAG_S) xnor status(ALU_FLAG_O));
	end op_movg;

	procedure op_movle( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_Z) or (status(ALU_FLAG_S) xor status(ALU_FLAG_O));
	end op_movle;

	procedure op_movge( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_O) xnor status(ALU_FLAG_S);
	end op_movge;

	procedure op_movl( variable result : out t_register; signal b : in t_register; signal status : in t_status; variable wflag : out std_logic ) is
	begin
		result := b; wflag := status(ALU_FLAG_S) xor status(ALU_FLAG_O);
	end op_movl;

	procedure op_loads( variable result : out t_register; signal status : in t_status ) is
	begin
		result := std_logic_vector(resize(unsigned(status), result'length));
	end op_loads;

	procedure op_stors( variable result : out t_register; signal a : in t_register; signal status : out t_status; variable wflag : out std_logic ) is
	begin
		status <= std_logic_vector(resize(unsigned(a), status'length)); wflag  := '0';
	end op_stors;
begin
	process(clock)
		variable result : t_register;
		variable wflag  : std_logic;
	begin
		if rising_edge(clock) then
			wflag  := '1';

			case opcode is
				when ALU_CMP   => op_cmp   ( result, reg_a, reg_b, status, wflag );
				when ALU_AND   => op_and   ( result, reg_a, reg_b );
				when ALU_NAND  => op_nand  ( result, reg_a, reg_b );
				when ALU_OR    => op_or    ( result, reg_a, reg_b );
				when ALU_NOR   => op_nor   ( result, reg_a, reg_b );
				when ALU_NOT   => op_not   ( result,        reg_b );
				when ALU_XOR   => op_xor   ( result, reg_a, reg_b );
				when ALU_XNOR  => op_xnor  ( result, reg_a, reg_b );
				when ALU_SHL   => op_shl   ( result, reg_a, reg_b, status );
				when ALU_SHR   => op_shr   ( result, reg_a, reg_b, status );
				when ALU_ROL   => op_rol   ( result, reg_a, reg_b );
				when ALU_ROR   => op_ror   ( result, reg_a, reg_b );
				when ALU_NEG   => op_neg   ( result,        reg_b );
				when ALU_ADD   => op_add   ( result, reg_a, reg_b, status );
				when ALU_SUB   => op_sub   ( result, reg_a, reg_b, status );
				when ALU_MUL   => op_mul   ( result, reg_a, reg_b, status );
				when ALU_MULU  => op_mulu  ( result, reg_a, reg_b, status );
				when ALU_DIV   => op_div   ( result, reg_a, reg_b );
				when ALU_DIVU  => op_divu  ( result, reg_a, reg_b );
				when ALU_MOD   => op_mod   ( result, reg_a, reg_b );
				when ALU_ABS   => op_abs   ( result,        reg_b );
				when ALU_MOV   => op_mov   ( result,        reg_b );
				when ALU_MOVE  => op_move  ( result,        reg_b, status, wflag );
				when ALU_MOVNE => op_movne ( result,        reg_b, status, wflag );
				when ALU_MOVS  => op_movs  ( result,        reg_b, status, wflag );
				when ALU_MOVNS => op_movns ( result,        reg_b, status, wflag );
				when ALU_MOVO  => op_movo  ( result,        reg_b, status, wflag );
				when ALU_MOVNO => op_movno ( result,        reg_b, status, wflag );
				when ALU_MOVP  => op_movp  ( result,        reg_b, status, wflag );
				when ALU_MOVNP => op_movnp ( result,        reg_b, status, wflag );
				when ALU_MOVA  => op_mova  ( result,        reg_b, status, wflag );
				when ALU_MOVBE => op_movbe ( result,        reg_b, status, wflag );
				when ALU_MOVAE => op_movae ( result,        reg_b, status, wflag );
				when ALU_MOVB  => op_movb  ( result,        reg_b, status, wflag );
				when ALU_MOVG  => op_movg  ( result,        reg_b, status, wflag );
				when ALU_MOVLE => op_movle ( result,        reg_b, status, wflag );
				when ALU_MOVGE => op_movge ( result,        reg_b, status, wflag );
				when ALU_MOVL  => op_movl  ( result,        reg_b, status, wflag );
				when ALU_LOADS => op_loads ( result,               status );
				when ALU_STORS => op_stors ( result,        reg_b, status, wflag );
				when others => result := (others => '0'); wflag := '0';
			end case;

			if ((opcode /= ALU_LOADS) and (wflag = '1')) or (opcode = ALU_CMP) then
				if result = (result'range => '0') then
					status(ALU_FLAG_Z) <= '1';
				else
					status(ALU_FLAG_Z) <= '0';
				end if;

				status(ALU_FLAG_S) <= result(result'left);
				status(ALU_FLAG_P) <= not(result(result'right));
			end if;

			if wflag = '1' then
				reg_c <= result;
			else
				reg_c <= ZERO;
			end if;

			write <= wflag;
		end if;
	end process;
end behavioral;
