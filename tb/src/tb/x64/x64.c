// This entire module is one translation unit so that it doesn't have to worry
// about C's crappy support for public and private interfaces. Because it's a
// separate TU completely from the rest of the project, it means that we can use
// tiny function names so long as they don't internally collide since they're static.
#define USING_FAST_PATH (0)
#define X64_OLD_PATH
#include "x64.h"

// used to add patches since there's separate arrays per thread
static thread_local size_t s_local_thread_id;

#include "x64_emitter.h"
#include "x64_proepi.h"
#include "x64_fast.h"

#include "x64_complex.h"

#if 0
#define DEBUG_LOG(...) printf(__VA_ARGS__)
#else
#define DEBUG_LOG(...) ((void)0)
#endif

static size_t x64_emit_call_patches(TB_Module* restrict m) {
    size_t r = 0;
    FOREACH_N(i, 0, m->max_threads) {
        TB_SymbolPatch* patches = m->thread_info[i].symbol_patches;

        dyn_array_for(j, patches) {
            TB_SymbolPatch* patch = &patches[j];

            if (patch->target->tag == TB_SYMBOL_FUNCTION) {
                TB_FunctionOutput* out_f = patch->source->output;
                assert(out_f && "Patch cannot be applied to function with no compiled output");

                // x64 thinks of relative addresses as being relative
                // to the end of the instruction or in this case just
                // 4 bytes ahead hence the +4.
                size_t actual_pos = out_f->code_pos + out_f->prologue_length + patch->pos + 4;

                uint32_t p = ((TB_Function*) patch->target)->output->code_pos - actual_pos;
                memcpy(&out_f->code[out_f->prologue_length + patch->pos], &p, sizeof(uint32_t));
                r += 1;
            }
        }
    }

    return r;
}

static int get_data_type_size(const TB_DataType dt) {
    assert(dt.width <= 2 && "Vector width too big!");

    switch (dt.type) {
        case TB_INT: {
            // round up bits to a byte
            bool is_big_int = dt.data > 64;
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

            return ((bits+7) / 8) << dt.width;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            return s << dt.width;
        }
        case TB_PTR: {
            return 8;
        }
        default: {
            tb_unreachable();
            return 0;
        }
    }
}

static void x64_get_data_type_size(TB_DataType dt, TB_CharUnits* out_size, TB_CharUnits* out_align) {
    switch (dt.type) {
        case TB_INT: {
            // round up bits to a byte
            bool is_big_int = dt.data > 64;
            int bits = is_big_int ? ((dt.data + 7) / 8) : tb_next_pow2(dt.data);

            *out_size  = ((bits+7) / 8) << dt.width;
            *out_align = is_big_int ? 8 : bits/8;
            break;
        }
        case TB_FLOAT: {
            int s = 0;
            if (dt.data == TB_FLT_32) s = 4;
            else if (dt.data == TB_FLT_64) s = 8;
            else tb_unreachable();

            *out_size = s << dt.width;
            *out_align = s;
            break;
        }
        case TB_PTR: {
            *out_size = 8;
            *out_align = 8;
            break;
        }
        default: tb_unreachable();
    }
}

#if _MSC_VER
_Pragma("warning (push)") _Pragma("warning (disable: 4028)")
#endif
ICodeGen tb__x64_codegen = {
    .minimum_addressable_size = 8,
    .pointer_size = 64,

    .get_data_type_size  = x64_get_data_type_size,
    .emit_call_patches   = x64_emit_call_patches,
    .emit_prologue       = x64_emit_prologue,
    .emit_epilogue       = x64_emit_epilogue,
    .emit_win64eh_unwind_info = x64_emit_win64eh_unwind_info,

    .fast_path = x64_fast_compile_function,
    .complex_path = x64_complex_compile_function
};
#if _MSC_VER
_Pragma("warning (pop)")
#endif
