#ifndef TB_A64_H
#define TB_A64_H

#include <stdint.h>
#include <stdbool.h>

#ifndef TB_API
#  ifdef __cplusplus
#    define TB_EXTERN extern "C"
#  else
#    define TB_EXTERN
#  endif
#  ifdef TB_DLL
#    ifdef TB_IMPORT_DLL
#      define TB_API TB_EXTERN __declspec(dllimport)
#    else
#      define TB_API TB_EXTERN __declspec(dllexport)
#    endif
#  else
#    define TB_API TB_EXTERN
#  endif
#endif

typedef struct {
    uint16_t opcode;
    uint8_t length;

    // reg operands
    uint8_t regs[4];
} TB_A64_Inst;

TB_API bool tb_a64_disasm(TB_A64_Inst* restrict inst, size_t length, const uint8_t* data);

#endif /* TB_A64_H */
