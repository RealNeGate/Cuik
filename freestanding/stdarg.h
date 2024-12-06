#pragma once

#ifdef _WIN32
// Win64 ABI
typedef char* va_list;

#define va_start(ap, a) __va_start(&ap, a)
#define va_arg(ap, ty)  __va_arg(&ap, ty)
#define va_end() ((void)0)
#define va_copy(destination, source) ((destination) = (source))
#else
// SystemV ABI
typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    void *overflow_arg_area;
    void *reg_save_area;
} __va_elem;

typedef __va_elem va_list[1];

static void *__va_arg_mem(__va_elem *ap, int sz, int align) {
    void *p = ap->overflow_arg_area;
    if (align > 8)
        p = (p + 15) / 16 * 16;
    ap->overflow_arg_area = ((unsigned long)p + sz + 7) / 8 * 8;
    return p;
}

static void *__va_arg_gp(__va_elem *ap, int sz, int align) {
    if (ap->gp_offset >= 48)
        return __va_arg_mem(ap, sz, align);

    void *r = ap->reg_save_area + ap->gp_offset;
    ap->gp_offset += 8;
    return r;
}

static void *__va_arg_fp(__va_elem *ap, int sz, int align) {
    if (ap->fp_offset >= 112)
        return __va_arg_mem(ap, sz, align);

    void *r = ap->reg_save_area + ap->fp_offset;
    ap->fp_offset += 8;
    return r;
}

#define va_arg(ap, ty) *((ty*) __va_arg(ap, ty))
#define va_start(ap, a) __va_start(ap, a)
#define va_end(ap)
#define va_copy(dest, src) ((dest)[0] = (src)[0])

#define __GNUC_VA_LIST 1
typedef va_list __gnuc_va_list;
#endif
