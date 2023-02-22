// NOTE(NeGate): We'll be providing tooling from my disx86, that's all gonna go
// here. eventually the entire disassembler will be in here.
typedef enum TB_X86_InstrFlags {
	// uses xmm registers for the reg array
	X86_INSTR_XMMREG = (1u << 0u),

	// r/m is a memory operand
	X86_INSTR_USE_MEMOP = (1u << 1u),

	// r/m is a rip-relative address (X86_INSTR_USE_MEMOP is always set when this is set)
	X86_INSTR_USE_RIPMEM = (1u << 2u),

	// LOCK prefix is present
	X86_INSTR_LOCK = (1u << 3u),

	// uses a signed immediate
	X86_INSTR_IMMEDIATE = (1u << 4u),

	// absolute means it's using the 64bit immediate (cannot be applied while a memory operand is active)
	X86_INSTR_ABSOLUTE = (1u << 5u),

	// set if the r/m can be found on the right hand side
	X86_INSTR_DIRECTION = (1u << 6u),

	// uses the second data type because the instruction is weird like MOVSX or MOVZX
	X86_INSTR_TWO_DATA_TYPES = (1u << 7u)
} TB_X86_InstrFlags;
