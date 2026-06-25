(namespace mips

    (reg_class "GPR"
        ZR                      // zero reg.
        AT                      // reserved for assembler.
        V0 V1                   // returns.
        A0 A1 A2 A3 A4 A5 A6 A7 // call params.
        T4 T5 T6 T7             // temporaries (volatile)
        S0 S1 S2 S3 S4 S5 S6 S7 // temporaries (non-volatile)
        T8 T9                   // temporaries (volatile)
        K0 K1                   // kernel regs.
        GP                      // global ptr
        SP                      // stack ptr
        FP                      // frame ptr
        RA                      // return addr
    )

    (reg_class "FPR"
        F0  F1  F2  F3  F4  F5  F6  F7
        F8  F9  F10 F11 F12 F13 F14 F15
        F16 F17 F18 F19 F20 F21 F22 F23
        F24 F25 F26 F27 F28 F29 F30 F31
    )

)
