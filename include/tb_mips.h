#ifndef TB_MIPS_H
#define TB_MIPS_H

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
    uint32_t raw;
} TB_MIPS_Inst;

TB_API bool tb_mips_disasm(TB_MIPS_Inst* restrict inst, size_t length, const uint8_t* data);

#endif /* TB_MIPS_H */
