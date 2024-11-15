#include <cuik_lex.h>

static ptrdiff_t find_last_slash(const char* str) {
    size_t len = strlen(str);
    for (size_t i = len; i--;) {
        if (str[i] == '/' || str[i] == '\\') return i;
    }

    return -1;
}

static ptrdiff_t find_ext(size_t len, const char* str) {
    for (size_t i = len; i--;) {
        if (str[i] == '/' || str[i] == '\\') return -1;
        if (str[i] == '.') return i;
    }

    return -1;
}

bool cuik_path_set_no_ext(Cuik_Path* restrict dst, const char* src) {
    ptrdiff_t slash_pos = find_ext(strlen(src), src);
    if (slash_pos >= 0) {
        // copy everything before that last slash, then normalize the slash to the OS
        memcpy(dst->data, src, slash_pos);
        dst->data[slash_pos] = CUIK_PATH_SLASH_SEP;
        dst->length = slash_pos + 1;
    } else {
        dst->length = 0;
        dst->data[0] = '\0';
    }

    return true;
}

bool cuik_path_set(Cuik_Path* restrict dst, const char* src) {
    size_t len = src ? strlen(src) : 0;
    if (len >= FILENAME_MAX) {
        return false;
    }

    memcpy(dst->data, src, len);
    dst->data[len] = 0;
    dst->length = len;
    return true;
}

bool cuik_path_set_ext(Cuik_Path* restrict dst, Cuik_Path* restrict src, size_t ext_len, const char* ext) {
    ptrdiff_t dot_pos = find_ext(src->length, src->data);

    return cuik_path_append2(dst, dot_pos >= 0 ? dot_pos : src->length, src->data, ext_len, ext);
}

bool cuik_path_set_dir(Cuik_Path* restrict dst, const char* src) {
    ptrdiff_t slash_pos = find_last_slash(src);
    if (slash_pos >= 0) {
        // copy everything before that last slash, then normalize the slash to the OS
        memcpy(dst->data, src, slash_pos);
        dst->data[slash_pos] = CUIK_PATH_SLASH_SEP;
        dst->length = slash_pos + 1;
    } else {
        dst->length = 0;
        dst->data[0] = '\0';
    }

    return true;
}

bool cuik_path_append2(Cuik_Path* restrict dst, size_t a_len, const char a[], size_t b_len, const char b[]) {
    size_t end = a_len + b_len;
    if (end >= FILENAME_MAX) {
        return false;
    }

    memcpy(dst->data, a, a_len);
    memcpy(dst->data + a_len, b, b_len);
    dst->data[end] = 0;
    dst->length = end;
    return true;
}

bool cuik_path_append(Cuik_Path* restrict dst, const Cuik_Path* restrict a, size_t b_len, const char b[]) {
    size_t end = a->length + b_len;
    if (end >= FILENAME_MAX) {
        return false;
    }

    memcpy(dst->data, a->data, a->length);
    memcpy(dst->data + a->length, b, b_len);
    dst->data[end] = 0;
    dst->length = end;
    return true;
}

bool cuik_path_has_ext(const Cuik_Path* restrict src, const char* ext) {
    size_t ext_len = strlen(ext);

    return src->length > ext_len + 1
        && src->data[src->length - ext_len - 1] == '.'
        && memcmp(&src->data[src->length - ext_len], ext, ext_len) == 0;
}

bool cuik_path_has_ext2(const Cuik_Path* restrict src) {
    for (size_t i = src->length; i--;) {
        if (src->data[i] == '/') return false;
        if (src->data[i] == '\\') return false;

        if (src->data[i] == '.') return true;
    }

    return false;
}

bool cuik_path_is_in(const Cuik_Path* restrict dst, const char* src) {
    size_t src_len = src ? strlen(src) : 0;
    if (src_len > 0 && (src[src_len - 1] == '/' || src[src_len - 1] == '\\')) {
        src_len -= 1;
    }

    return dst->length >= src_len + 1
        && memcmp(dst->data, src, src_len) == 0
        && (dst->data[src_len] == '/' || dst->data[src_len] == '\\');
}
