#include <stdio.h>

// Compile a variety of emitter bullshit to show that it works correctly
bool tb_x64_test_suite(void) {
    TB_CGEmitter e = { .capacity = 4096, .data = tb_platform_heap_alloc(4096) };

    FOREACH_N(i, 0, XADD+1) {
        static const GPR gprs[] = { 0, 8, 12 };
        static const TB_DataType dts[] = { TB_TYPE_I8, TB_TYPE_I16, TB_TYPE_I32, TB_TYPE_I64 };

        // LEA doesn't allow for GPR->GPR
        if (i == LEA) continue;

        // GPR -> GPR
        FOREACH_N(j, 0, 3) {
            Val a = val_gpr(TB_TYPE_I64, gprs[j]);

            FOREACH_N(k, 0, 3) {
                Val b = val_gpr(TB_TYPE_I64, gprs[k]);

                FOREACH_N(l, 0, 4) {
                    inst2(&e, i, &a, &b, dts[l]);
                }
            }
        }
    }

    FILE* f = fopen("foo.bin", "wb");
    fwrite(e.data, e.count, 1, f);
    fclose(f);
    return true;
}
