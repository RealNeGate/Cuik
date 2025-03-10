matmul:
        movi    v0.4s, #128, lsl #24
        mov     x8, xzr
        add     x9, x1, #8
        stp     q0, q0, [x0]
        stp     q0, q0, [x0, #32]
        stp     q0, q0, [x0, #64]
        stp     q0, q0, [x0, #96]
        stp     q0, q0, [x0, #128]
        stp     q0, q0, [x0, #160]
        stp     q0, q0, [x0, #192]
        stp     q0, q0, [x0, #224]
        stp     q0, q0, [x0, #256]
        stp     q0, q0, [x0, #288]
        stp     q0, q0, [x0, #320]
        stp     q0, q0, [x0, #352]
        stp     q0, q0, [x0, #384]
        stp     q0, q0, [x0, #416]
        stp     q0, q0, [x0, #448]
        stp     q0, q0, [x0, #480]
        stp     q0, q0, [x0, #512]
        stp     q0, q0, [x0, #544]
        stp     q0, q0, [x0, #576]
        stp     q0, q0, [x0, #608]
        stp     q0, q0, [x0, #640]
        stp     q0, q0, [x0, #672]
        stp     q0, q0, [x0, #704]
        stp     q0, q0, [x0, #736]
        stp     q0, q0, [x0, #768]
        stp     q0, q0, [x0, #800]
        stp     q0, q0, [x0, #832]
        stp     q0, q0, [x0, #864]
        stp     q0, q0, [x0, #896]
        stp     q0, q0, [x0, #928]
        stp     q0, q0, [x0, #960]
        stp     q0, q0, [x0, #992]
.LBB0_1:
        lsl     x11, x8, #4
        mov     x10, xzr
        mov     x15, x0
        orr     x12, x11, #0x10
        orr     x13, x11, #0x20
        orr     x14, x11, #0x30
.LBB0_2:
        orr     x16, x10, x11
        orr     x17, x10, x12
        orr     x18, x10, x13
        orr     x1, x10, x14
        add     x16, x2, x16, lsl #2
        add     x17, x2, x17, lsl #2
        add     x18, x2, x18, lsl #2
        add     x1, x2, x1, lsl #2
        mov     x3, x9
        mov     x4, x15
        mov     w5, #16
.LBB0_3:
        ldp     s2, s3, [x3, #-8]
        subs    x5, x5, #1
        ldr     q0, [x16]
        ldr     q1, [x4]
        fmla    v1.4s, v0.4s, v2.s[0]
        ldr     q0, [x17]
        fmla    v1.4s, v0.4s, v3.s[0]
        ldr     q0, [x18]
        ldp     s2, s3, [x3], #64
        fmla    v1.4s, v0.4s, v2.s[0]
        ldr     q0, [x1]
        fmla    v1.4s, v0.4s, v3.s[0]
        str     q1, [x4], #64
        b.ne    .LBB0_3
        cmp     x10, #12
        add     x10, x10, #4
        add     x15, x15, #16
        b.lo    .LBB0_2
        cmp     x8, #12
        add     x8, x8, #4
        add     x9, x9, #16
        b.lo    .LBB0_1
        ret
