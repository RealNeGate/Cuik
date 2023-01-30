#include "../tb_internal.h"

size_t tb_helper_write_text_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;
        if (out_f != NULL) {
            memcpy(output + write_pos, out_f->code, out_f->code_size);
            write_pos += out_f->code_size;
        }
    }

    return write_pos;
}

size_t tb_helper_write_data_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);
    uint8_t* data = &output[pos];

    FOREACH_N(i, 0, m->max_threads) {
        pool_for(TB_Global, g, m->thread_info[i].globals) {
            if (g->storage != TB_STORAGE_DATA) continue;

            TB_Initializer* init = g->init;

            // clear out space
            memset(&data[g->pos], 0, init->size);

            FOREACH_N(k, 0, init->obj_count) {
                if (init->objects[k].type == TB_INIT_OBJ_REGION) {
                    memcpy(&data[g->pos + init->objects[k].offset], init->objects[k].region.ptr, init->objects[k].region.size);
                }
            }
        }
    }

    return write_pos + m->data_region_size;
}

size_t tb_helper_write_rodata_section(size_t write_pos, TB_Module* m, uint8_t* output, uint32_t pos) {
    assert(write_pos == pos);

    uint8_t* rdata = &output[pos];
    FOREACH_N(i, 0, m->max_threads) {
        FOREACH_N(j, 0, dyn_array_length(m->thread_info[i].const_patches)) {
            TB_ConstPoolPatch* p = &m->thread_info[i].const_patches[j];
            memcpy(&rdata[p->rdata_pos], p->data, p->length);
        }
    }

    return write_pos + m->rdata_region_size;
}

size_t tb_helper_get_text_section_layout(TB_Module* m, size_t symbol_id_start) {
    size_t id = symbol_id_start, offset = 0;
    TB_FOR_FUNCTIONS(f, m) {
        TB_FunctionOutput* out_f = f->output;

        if (out_f) {
            f->super.symbol_id = id;
            out_f->code_pos = offset;

            offset += out_f->code_size;
            id += 1;
        }
    }

    return offset;
}
