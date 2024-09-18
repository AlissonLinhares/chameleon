--           [ 31|30 29|28 27|26 25 24 23 22 21|20 19 18 17 16|15 14 13 12 11|  10 9 8 7 6  | 5 4 3 2 1 0  ]
-- Format G0 [  V|  G  |  U  |   Opcode (6)    |                     Immediate (21)                        ]
-- Format G1 [  V|  G  |  U  |   Opcode (6)    | Register (5) |                   Immediate(16)            ]
-- Format G2 [  V|  G  |  U  |   Opcode (6)    | Register (5) | Register (5) |           Immediate(11)     ]
-- Format G3 [  V|  G  |  U  |   Opcode (6)    | Register (5) | Register (5) | Register (5) | RESERVED (6) ]
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

package isa is
	subtype t_opcode        is std_logic_vector(5 downto 0);   -- 64 addressable operations.
	subtype t_operation     is std_logic_vector(31 downto 21);
	subtype t_status        is std_logic_vector(4 downto 0);   -- 5 Flags (CSZOP)
	subtype t_register      is std_logic_vector(63 downto 0);  -- 64 bit word.
	subtype t_rindex        is std_logic_vector(4 downto 0);   -- 32 addressable registers.
	subtype t_instruction   is std_logic_vector(31 downto 0);  -- 32 bit instruction size.
	subtype t_address       is std_logic_vector(7 downto 0);   -- (2 ^ 8) * 4 = 1024 bytes

	type t_control_signal   is (SIG_CONTINUE, SIG_WAIT, SIG_FLUSH);
	type t_register_file    is array(63 downto 0) of t_register;
	type t_memory_bank      is array(natural range ((2**t_address'length) - 1) downto 0) of t_instruction;

	subtype INST_IMM11_RANGE  is natural range 10 downto 0;
	subtype INST_IMM16_RANGE  is natural range 15 downto 0;
	subtype INST_IMM21_RANGE  is natural range 20 downto 0;
	subtype INST_RESE_RANGE   is natural range  5 downto 0;
	subtype INST_REG0_RANGE   is natural range 10 downto 6;
	subtype INST_REG1_RANGE   is natural range 15 downto 11;
	subtype INST_REG2_RANGE   is natural range 20 downto 16;
	subtype INST_OPCODE_RANGE is natural range 26 downto 21;
	subtype INST_UFLAG_RANGE  is natural range 28 downto 27;
	subtype INST_GFLAG_RANGE  is natural range 30 downto 29;
	subtype INST_VFLAG_RANGE  is natural range 31 downto 31;

	constant G0_FORMAT     : std_logic_vector(INST_GFLAG_RANGE) := "00";
	constant G1_FORMAT     : std_logic_vector(INST_GFLAG_RANGE) := "01";
	constant G2_FORMAT     : std_logic_vector(INST_GFLAG_RANGE) := "10";
	constant G3_FORMAT     : std_logic_vector(INST_GFLAG_RANGE) := "11";

	constant ULA           : std_logic_vector(INST_UFLAG_RANGE) := "00";
	constant LSU           : std_logic_vector(INST_UFLAG_RANGE) := "01";
	constant SU            : std_logic_vector(INST_UFLAG_RANGE) := "10";
	constant FPU           : std_logic_vector(INST_UFLAG_RANGE) := "11";

	constant GPR_BASE_LEN   : integer := 6;                 -- 2^6 = 64 bits
	constant GPR_WORD       : integer := 2**GPR_BASE_LEN;   -- 64 bit half word.
	constant GPR_HALF_WORD  : integer := GPR_WORD / 2;      -- 32 bit half word.

	constant MAX_UINT       : t_register := (others => '1');
	constant MIN_UINT       : t_register := (others => '0');
	constant MIN_INT        : t_register := (t_register'left => '1', others => '0');
	constant MAX_INT        : t_register := (t_register'left => '0', others => '1');
	constant ONE            : t_register := (t_register'right => '1',others => '0');
	constant ZERO           : t_register := MIN_UINT;

	constant GPR0           : t_rindex := "00000";
	constant GPR1           : t_rindex := "00001";
	constant GPR2           : t_rindex := "00010";
	constant GPR3           : t_rindex := "00011";
	constant GPR4           : t_rindex := "00100";
	constant GPR5           : t_rindex := "00101";
	constant GPR6           : t_rindex := "00110";
	constant GPR7           : t_rindex := "00111";
	constant GPR8           : t_rindex := "01000";
	constant GPR9           : t_rindex := "01001";
	constant GPR10          : t_rindex := "01010";
	constant GPR11          : t_rindex := "01011";
	constant GPR12          : t_rindex := "01100";
	constant GPR13          : t_rindex := "01101";
	constant GPR14          : t_rindex := "01110";
	constant GPR15          : t_rindex := "01111";
	constant GPR16          : t_rindex := "10000";
	constant GPR17          : t_rindex := "10001";
	constant GPR18          : t_rindex := "10010";
	constant GPR19          : t_rindex := "10011";
	constant GPR20          : t_rindex := "10100";
	constant GPR21          : t_rindex := "10101";
	constant GPR22          : t_rindex := "10110";
	constant GPR23          : t_rindex := "10111";
	constant GPR24          : t_rindex := "11000";
	constant GPR25          : t_rindex := "11001";
	constant GPR26          : t_rindex := "11010";
	constant GPR27          : t_rindex := "11011";
	constant GPR28          : t_rindex := "11100";
	constant GPR29          : t_rindex := "11101";
	constant GPR30          : t_rindex := "11110";
	constant GPR31          : t_rindex := "11111";

	constant PC             : t_rindex := GPR31; -- Programming counter.
	constant STP            : t_rindex := GPR30; -- Stack point.

	constant ALU_FLAG_O     : integer := 0;  -- Overflow flag.
	constant ALU_FLAG_S     : integer := 1;  -- Signed result.
	constant ALU_FLAG_Z     : integer := 2;  -- Zero result.
	constant ALU_FLAG_C     : integer := 3;  -- Carry bit.
	constant ALU_FLAG_P     : integer := 4;  -- Parity flag.

	constant ALU_CMP        : t_opcode := "000000"; --   -  ,<reg>,<reg>  ; Compare two values and changes the status registers.
	constant ALU_AND        : t_opcode := "000001"; -- <reg>,<reg>,<reg>  ; Logical "and".
	constant ALU_NAND       : t_opcode := "000010"; -- <reg>,<reg>,<reg>  ; Logical complement of "and".
	constant ALU_OR         : t_opcode := "000011"; -- <reg>,<reg>,<reg>  ; Logical "or".
	constant ALU_NOR        : t_opcode := "000100"; -- <reg>,<reg>,<reg>  ; Logical complement of "or".
	constant ALU_NOT        : t_opcode := "000101"; -- <reg>,<reg>,  -    ; Complement.
	constant ALU_XOR        : t_opcode := "000110"; -- <reg>,<reg>,<reg>  ; Logical exclusive "or".
	constant ALU_XNOR       : t_opcode := "000111"; -- <reg>,<reg>,<reg>  ; Logical complement of exclusive "or".
	constant ALU_SHL        : t_opcode := "001000"; -- <reg>,<reg>,<reg>  ; Signed left shift.
	constant ALU_SHR        : t_opcode := "001001"; -- <reg>,<reg>,<reg>  ; Signed shift right.
	constant ALU_ROL        : t_opcode := "001010"; -- <reg>,<reg>,<reg>  ; Rotate left.
	constant ALU_ROR        : t_opcode := "001011"; -- <reg>,<reg>,<reg>  ; Rotate right.
	constant ALU_NEG        : t_opcode := "001100"; -- <reg>,<reg>,  -    ; Negate number.
	constant ALU_ADD        : t_opcode := "001101"; -- <reg>,<reg>,<reg>  ; Signed addition.
	constant ALU_SUB        : t_opcode := "001110"; -- <reg>,<reg>,<reg>  ; Signed subtraction.
	constant ALU_MUL        : t_opcode := "001111"; -- <reg>,<reg>,<reg>  ; Signed multiplication.
	constant ALU_MULU       : t_opcode := "010000"; -- <reg>,<reg>,<reg>  ; Unsigned multiplication.
	constant ALU_DIV        : t_opcode := "010001"; -- <reg>,<reg>,<reg>  ; Signed division.
	constant ALU_DIVU       : t_opcode := "010010"; -- <reg>,<reg>,<reg>  ; Unsigned division.
	constant ALU_MOD        : t_opcode := "010011"; -- <reg>,<reg>,<reg>  ; Division remainder.
	constant ALU_ABS        : t_opcode := "010100"; -- <reg>,<reg>,  -    ; Absolute value.
	constant ALU_MOV        : t_opcode := "010101"; -- <reg>,<reg>,  -    ; Move data.
	constant ALU_MOVE       : t_opcode := "010110"; -- <reg>,<reg>,  -    ; Move data if zero/equal.       (Z == 1)
	constant ALU_MOVNE      : t_opcode := "010111"; -- <reg>,<reg>,  -    ; Move data if not zero/equal.   (Z == 0)
	constant ALU_MOVS       : t_opcode := "011000"; -- <reg>,<reg>,  -    ; Move data if signed.           (S == 1)           (signed)
	constant ALU_MOVNS      : t_opcode := "011001"; -- <reg>,<reg>,  -    ; Move data if not signed.       (S == 0)
	constant ALU_MOVO       : t_opcode := "011010"; -- <reg>,<reg>,  -    ; Move data if overflow.         (O == 1)           (signed)
	constant ALU_MOVNO      : t_opcode := "011011"; -- <reg>,<reg>,  -    ; Move data if not overflow.     (O == 0)           (signed)
	constant ALU_MOVP       : t_opcode := "011100"; -- <reg>,<reg>,  -    ; Move data if parity.           (P == 1)
	constant ALU_MOVNP      : t_opcode := "011101"; -- <reg>,<reg>,  -    ; Move data if no parity.        (P == 0)
	constant ALU_MOVA       : t_opcode := "011110"; -- <reg>,<reg>,  -    ; Move data if above.            (C == 0 && Z == 0)
	constant ALU_MOVBE      : t_opcode := "111111"; -- <reg>,<reg>,  -    ; Move data if below.            (C == 1 || Z == 1)
	constant ALU_MOVAE      : t_opcode := "100000"; -- <reg>,<reg>,  -    ; Move data if above equal.      (C == 0)
	constant ALU_MOVB       : t_opcode := "100001"; -- <reg>,<reg>,  -    ; Move data if below.            (C == 1)
	constant ALU_MOVG       : t_opcode := "100010"; -- <reg>,<reg>,  -    ; Move data if greater.          (Z == 0 && S == O) (signed)
	constant ALU_MOVLE      : t_opcode := "100011"; -- <reg>,<reg>,  -    ; Move data if less or equal.    (Z == 1 || S != O) (signed)
	constant ALU_MOVGE      : t_opcode := "100100"; -- <reg>,<reg>,  -    ; Move data if greater or equal. (S == O)           (signed)
	constant ALU_MOVL       : t_opcode := "100101"; -- <reg>,<reg>,  -    ; Move data if less.             (S != O)           (signed)
	constant ALU_LOADS      : t_opcode := "100110"; -- <reg>,  -  ,  -    ; Load the status register.
	constant ALU_STORS      : t_opcode := "100111"; --   -  ,<reg>,  -    ; Store the data into the status register.
	constant ALU_NOP        : t_opcode := "111111";

	constant LSU_LOAD       : t_opcode := "000000"; -- <reg>,<reg>,  -    ; Load data from memory.
	constant LSU_STORE      : t_opcode := "000001"; -- <reg>,<reg>,  -    ; Store data into memory.

	constant SU_SYS         : t_opcode := "000000"; -- <reg>,<reg>,  -    ; Syscall.

	constant OP_SYS        : t_operation := "0" & G2_FORMAT & SU  & SU_SYS   ; -- <reg>,<reg>,  -    ; Syscall.
	constant OP_LOAD       : t_operation := "0" & G2_FORMAT & LSU & LSU_LOAD ; -- <reg>,<reg>,  -    ; Load data from memory.
	constant OP_STORE      : t_operation := "0" & G2_FORMAT & LSU & LSU_STORE; -- <reg>,<reg>,  -    ; Store data into memory.
	constant OP_CMP        : t_operation := "0" & G3_FORMAT & ULA & ALU_CMP  ; --   0  ,<reg>,<reg>  ; Compare two values and changes the status registers.
	constant OP_AND        : t_operation := "0" & G3_FORMAT & ULA & ALU_AND  ; -- <reg>,<reg>,<reg>  ; Logical "and".
	constant OP_NAND       : t_operation := "0" & G3_FORMAT & ULA & ALU_NAND ; -- <reg>,<reg>,<reg>  ; Logical complement of "and".
	constant OP_OR         : t_operation := "0" & G3_FORMAT & ULA & ALU_OR   ; -- <reg>,<reg>,<reg>  ; Logical "or".
	constant OP_NOR        : t_operation := "0" & G3_FORMAT & ULA & ALU_NOR  ; -- <reg>,<reg>,<reg>  ; Logical complement of "or".
	constant OP_NOT        : t_operation := "0" & G3_FORMAT & ULA & ALU_NOT  ; -- <reg>,  -  ,<reg>  ; Complement.
	constant OP_XOR        : t_operation := "0" & G3_FORMAT & ULA & ALU_XOR  ; -- <reg>,<reg>,<reg>  ; Logical exclusive "or".
	constant OP_XNOR       : t_operation := "0" & G3_FORMAT & ULA & ALU_XNOR ; -- <reg>,<reg>,<reg>  ; Logical complement of exclusive "or".
	constant OP_SHL        : t_operation := "0" & G3_FORMAT & ULA & ALU_SHL  ; -- <reg>,<reg>,<reg>  ; Signed left shift.
	constant OP_SHR        : t_operation := "0" & G3_FORMAT & ULA & ALU_SHR  ; -- <reg>,<reg>,<reg>  ; Signed shift right.
	constant OP_ROL        : t_operation := "0" & G3_FORMAT & ULA & ALU_ROL  ; -- <reg>,<reg>,<reg>  ; Rotate left.
	constant OP_ROR        : t_operation := "0" & G3_FORMAT & ULA & ALU_ROR  ; -- <reg>,<reg>,<reg>  ; Rotate right.
	constant OP_NEG        : t_operation := "0" & G3_FORMAT & ULA & ALU_NEG  ; -- <reg>,<reg>,<reg>  ; Negate number.
	constant OP_ADD        : t_operation := "0" & G3_FORMAT & ULA & ALU_ADD  ; -- <reg>,<reg>,<reg>  ; Signed addition.
	constant OP_SUB        : t_operation := "0" & G3_FORMAT & ULA & ALU_SUB  ; -- <reg>,<reg>,<reg>  ; Signed subtraction.
	constant OP_MUL        : t_operation := "0" & G3_FORMAT & ULA & ALU_MUL  ; -- <reg>,<reg>,<reg>  ; Signed multiplication.
	constant OP_MULU       : t_operation := "0" & G3_FORMAT & ULA & ALU_MULU ; -- <reg>,<reg>,<reg>  ; Unsigned multiplication.
	constant OP_DIV        : t_operation := "0" & G3_FORMAT & ULA & ALU_DIV  ; -- <reg>,<reg>,<reg>  ; Signed division.
	constant OP_DIVU       : t_operation := "0" & G3_FORMAT & ULA & ALU_DIVU ; -- <reg>,<reg>,<reg>  ; Unsigned division.
	constant OP_MOD        : t_operation := "0" & G3_FORMAT & ULA & ALU_MOD  ; -- <reg>,<reg>,<reg>  ; Division remainder.
	constant OP_ABS        : t_operation := "0" & G3_FORMAT & ULA & ALU_ABS  ; -- <reg>,  -  ,<reg>  ; Absolute value.
	constant OP_MOV        : t_operation := "0" & G3_FORMAT & ULA & ALU_MOV  ; -- <reg>,  -  ,<reg>  ; Move data.
	constant OP_MOVE       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVE ; -- <reg>,  -  ,<reg>  ; Move data if zero/equal.       (Z == 1)
	constant OP_MOVNE      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVNE; -- <reg>,  -  ,<reg>  ; Move data if not zero/equal.   (Z == 0)
	constant OP_MOVS       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVS ; -- <reg>,  -  ,<reg>  ; Move data if signed.           (S == 1)           (signed)
	constant OP_MOVNS      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVNS; -- <reg>,  -  ,<reg>  ; Move data if not signed.       (S == 0)
	constant OP_MOVO       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVO ; -- <reg>,  -  ,<reg>  ; Move data if overflow.         (O == 1)           (signed)
	constant OP_MOVNO      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVNO; -- <reg>,  -  ,<reg>  ; Move data if not overflow.     (O == 0)           (signed)
	constant OP_MOVP       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVP ; -- <reg>,  -  ,<reg>  ; Move data if parity.           (P == 1)
	constant OP_MOVNP      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVNP; -- <reg>,  -  ,<reg>  ; Move data if no parity.        (P == 0)
	constant OP_MOVA       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVA ; -- <reg>,  -  ,<reg>  ; Move data if above.            (C == 0 && Z == 0)
	constant OP_MOVBE      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVBE; -- <reg>,  -  ,<reg>  ; Move data if below.            (C == 1 || Z == 1)
	constant OP_MOVAE      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVAE; -- <reg>,  -  ,<reg>  ; Move data if above equal.      (C == 0)
	constant OP_MOVB       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVB ; -- <reg>,  -  ,<reg>  ; Move data if below.            (C == 1)
	constant OP_MOVG       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVG ; -- <reg>,  -  ,<reg>  ; Move data if greater.          (Z == 0 && S == O) (signed)
	constant OP_MOVLE      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVLE; -- <reg>,  -  ,<reg>  ; Move data if less or equal.    (Z == 1 || S != O) (signed)
	constant OP_MOVGE      : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVGE; -- <reg>,  -  ,<reg>  ; Move data if greater or equal. (S == O)           (signed)
	constant OP_MOVL       : t_operation := "0" & G3_FORMAT & ULA & ALU_MOVL ; -- <reg>,  -  ,<reg>  ; Move data if less.             (S != O)           (signed)
	constant OP_LOADS      : t_operation := "0" & G1_FORMAT & ULA & ALU_LOADS; -- <reg>,  -  ,  -    ; Load the status register.
	constant OP_STORS      : t_operation := "0" & G3_FORMAT & ULA & ALU_STORS; --   0  ,  -  ,<reg>  ; Store the data into the status register.
	constant OP_NOP        : t_operation := "0" & G0_FORMAT & ULA & ALU_NOP  ; --   -  ,  -  ,  -    ; NOP.
	constant OP_SYS_I      : t_operation := "0" & G1_FORMAT & SU  & SU_SYS   ; -- <reg>,<imm16>      ; Syscall.
	constant OP_LOAD_I     : t_operation := "0" & G1_FORMAT & LSU & LSU_LOAD ; -- <reg>,<imm16>      ; Load data from memory.
	constant OP_STORE_I    : t_operation := "0" & G1_FORMAT & LSU & LSU_STORE; -- <reg>,<imm16>      ; Store data into memory.
	constant OP_CMP_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_CMP  ; --   0  ,<reg>,<imm11>; Compare two values and changes the status registers.
	constant OP_AND_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_AND  ; -- <reg>,<reg>,<imm11>; Logical "and".
	constant OP_NAND_I     : t_operation := "0" & G2_FORMAT & ULA & ALU_NAND ; -- <reg>,<reg>,<imm11>; Logical complement of "and".
	constant OP_OR_I       : t_operation := "0" & G2_FORMAT & ULA & ALU_OR   ; -- <reg>,<reg>,<imm11>; Logical "or".
	constant OP_NOR_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_NOR  ; -- <reg>,<reg>,<imm11>; Logical complement of "or".
	constant OP_NOT_I      : t_operation := "0" & G1_FORMAT & ULA & ALU_NOT  ; -- <reg>,<imm16>      ; Complement.
	constant OP_XOR_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_XOR  ; -- <reg>,<reg>,<imm11>; Logical exclusive "or".
	constant OP_XNOR_I     : t_operation := "0" & G2_FORMAT & ULA & ALU_XNOR ; -- <reg>,<reg>,<imm11>; Logical complement of exclusive "or".
	constant OP_SHL_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_SHL  ; -- <reg>,<reg>,<imm11>; Signed left shift.
	constant OP_SHR_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_SHR  ; -- <reg>,<reg>,<imm11>; Signed shift right.
	constant OP_ROL_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_ROL  ; -- <reg>,<reg>,<imm11>; Rotate left.
	constant OP_ROR_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_ROR  ; -- <reg>,<reg>,<imm11>; Rotate right.
	constant OP_NEG_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_NEG  ; -- <reg>,<reg>,<imm11>; Negate number.
	constant OP_ADD_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_ADD  ; -- <reg>,<reg>,<imm11>; Signed addition.
	constant OP_SUB_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_SUB  ; -- <reg>,<reg>,<imm11>; Signed subtraction.
	constant OP_MUL_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_MUL  ; -- <reg>,<reg>,<imm11>; Signed multiplication.
	constant OP_MULU_I     : t_operation := "0" & G2_FORMAT & ULA & ALU_MULU ; -- <reg>,<reg>,<imm11>; Unsigned multiplication.
	constant OP_DIV_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_DIV  ; -- <reg>,<reg>,<imm11>; Signed division.
	constant OP_DIVU_I     : t_operation := "0" & G2_FORMAT & ULA & ALU_DIVU ; -- <reg>,<reg>,<imm11>; Unsigned division.
	constant OP_MOD_I      : t_operation := "0" & G2_FORMAT & ULA & ALU_MOD  ; -- <reg>,<reg>,<imm11>; Division remainder.
	constant OP_ABS_I      : t_operation := "0" & G1_FORMAT & ULA & ALU_ABS  ; -- <reg>,<imm16>      ; Absolute value.
	constant OP_MOV_I      : t_operation := "0" & G1_FORMAT & ULA & ALU_MOV  ; -- <reg>,<imm16>      ; Move data.
	constant OP_MOVE_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVE ; -- <reg>,<imm16>      ; Move data if zero/equal.       (Z == 1)
	constant OP_MOVNE_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVNE; -- <reg>,<imm16>      ; Move data if not zero/equal.   (Z == 0)
	constant OP_MOVS_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVS ; -- <reg>,<imm16>      ; Move data if signed.           (S == 1)           (signed)
	constant OP_MOVNS_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVNS; -- <reg>,<imm16>      ; Move data if not signed.       (S == 0)
	constant OP_MOVO_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVO ; -- <reg>,<imm16>      ; Move data if overflow.         (O == 1)           (signed)
	constant OP_MOVNO_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVNO; -- <reg>,<imm16>      ; Move data if not overflow.     (O == 0)           (signed)
	constant OP_MOVP_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVP ; -- <reg>,<imm16>      ; Move data if parity.           (P == 1)
	constant OP_MOVNP_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVNP; -- <reg>,<imm16>      ; Move data if no parity.        (P == 0)
	constant OP_MOVA_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVA ; -- <reg>,<imm16>      ; Move data if above.            (C == 0 && Z == 0)
	constant OP_MOVBE_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVBE; -- <reg>,<imm16>      ; Move data if below.            (C == 1 || Z == 1)
	constant OP_MOVAE_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVAE; -- <reg>,<imm16>      ; Move data if above equal.      (C == 0)
	constant OP_MOVB_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVB ; -- <reg>,<imm16>      ; Move data if below.            (C == 1)
	constant OP_MOVG_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVG ; -- <reg>,<imm16>      ; Move data if greater.          (Z == 0 && S == O) (signed)
	constant OP_MOVLE_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVLE; -- <reg>,<imm16>      ; Move data if less or equal.    (Z == 1 || S != O) (signed)
	constant OP_MOVGE_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVGE; -- <reg>,<imm16>      ; Move data if greater or equal. (S == O)           (signed)
	constant OP_MOVL_I     : t_operation := "0" & G1_FORMAT & ULA & ALU_MOVL ; -- <reg>,<imm16>      ; Move data if less.             (S != O)           (signed)
	constant OP_STORS_I    : t_operation := "0" & G1_FORMAT & ULA & ALU_STORS; --   0  ,<imm16>      ; Store the data into the status register.
end package isa;
