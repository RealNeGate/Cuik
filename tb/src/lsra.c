// Linear scan register allocator:
#include "codegen.h"

void tb__lsra(Ctx* restrict ctx, TB_Arena* arena) {
    // create physical intervals

    // init intervals from dataflow

    // linear scan:
    //   expire old => allocate free or spill/split => rinse & repeat.

    // move resolver:
    //   when a split happens, all indirect paths that cross the split will have
    //   moves inserted.
    tb_todo();
}
