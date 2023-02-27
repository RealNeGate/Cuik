#include "../tb_internal.h"

size_t tb_helper_write_section(TB_Module* m, size_t write_pos, TB_ModuleSection* section, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);
    uint8_t* data = &output[pos];

    switch (section->kind) {
        case TB_MODULE_SECTION_TEXT:
        TB_FOR_FUNCTIONS(f, m) {
            TB_FunctionOutput* out_f = f->output;

            if (out_f != NULL) {
                memcpy(output + out_f->code_pos, out_f->code, out_f->code_size);
            }
        }
        break;

        case TB_MODULE_SECTION_DATA:
        case TB_MODULE_SECTION_TLS:
        dyn_array_for(i, section->globals) {
            TB_Global* restrict g = section->globals[i];

            memset(&data[g->pos], 0, g->size);
            FOREACH_N(k, 0, g->obj_count) {
                if (g->objects[k].type == TB_INIT_OBJ_REGION) {
                    memcpy(&data[g->pos + g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
                }
            }
        }
        break;

        default:
        tb_todo();
        break;
    }

    return write_pos + section->total_size;
}
