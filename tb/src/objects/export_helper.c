#include "../tb_internal.h"

size_t tb_helper_write_section(size_t write_pos, TB_ModuleSection* section, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);
    uint8_t* data = &output[pos];

    dyn_array_for(i, section->functions) {
        TB_Function* restrict f = section->functions[i];
        TB_FunctionOutput* out_f = f->output;

        if (out_f != NULL) {
            memcpy(output + out_f->code_pos, out_f->code, out_f->code_size);
        }
    }

    dyn_array_for(i, section->globals) {
        TB_Global* restrict g = section->globals[i];

        memset(&data[g->pos], 0, g->size);
        FOREACH_N(k, 0, g->obj_count) {
            if (g->objects[k].type == TB_INIT_OBJ_REGION) {
                memcpy(&data[g->pos + g->objects[k].offset], g->objects[k].region.ptr, g->objects[k].region.size);
            }
        }
    }

    return write_pos + section->total_size;
}

size_t tb_helper_write_rodata_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);

    /*uint8_t* rdata = &output[pos];
    FOREACH_N(i, 0, m->max_threads) {
        FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
            TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
            memcpy(&rdata[p->rdata_pos], p->data, p->length);
        }
    }*/

    return write_pos + m->rdata.total_size;
}
