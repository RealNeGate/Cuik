// Kinds of passes
X(PEEPHOLES,       "Peepholes")
X(MEMORY,          "Memory")
X(OPTIMISTIC,      "Optimistic")
X(COMPACT,         "Compact")
// Loop opts
X(LOOP_FIND,       "LoopFinding")
X(LOOP_ROTATE,     "LoopRotation")
X(SUPERWORD,       "SLP")
X(STRENGTH_REDUCE, "StrengthReduce")
// Interprocedural
X(INLINE,          "Inliner")
X(IPSCCP,          "IPSCCP")
// Codegen
X(MACH_ISEL,       "MachineISel")
X(MACH_GCM,        "MachineGCM")
X(MACH_LCM,        "MachineLCM")
X(MACH_RA,         "MachineRA")
X(MACH_BB_SCHED,   "MachineBBSched")
X(MACH_EMIT,       "MachineEmit")
X(MACH_ASM_PRINT,  "MachineAsmPrint")
#undef X
