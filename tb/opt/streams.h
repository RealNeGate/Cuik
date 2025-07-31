
typedef struct {
    OutStream header;
    FILE* file;
} FileOutStream;

static void fos_write(OutStream* s, size_t n, const char* data) {
    FileOutStream* fos = (FileOutStream*) s;
    fwrite(data, n, 1, fos->file);
}

static int fos_writef(OutStream* s, const char* fmt, va_list ap) {
    FileOutStream* fos = (FileOutStream*) s;
    return vfprintf(fos->file, fmt, ap);
}

static void stdout_color(OutStream* s, int ansi_code) {
    fprintf(stdout, "\x1b[%dm", ansi_code);
}

static void stdout_write(OutStream* s, size_t n, const char* data) {
    fwrite(data, n, 1, stdout);
}

static int stdout_writef(OutStream* s, const char* fmt, va_list ap) {
    return vfprintf(stdout, fmt, ap);
}

static OutStream OUT_STREAM_DEFAULT = { stdout_color, stdout_write, stdout_writef };

static FileOutStream fos_make(FILE* fp) {
    return (FileOutStream){ { NULL, fos_write, fos_writef }, fp };
}

typedef struct {
    OutStream header;
    size_t cnt, cap;
    char* data;
} BufferOutStream;

static void* bos_reserve(OutStream* s, size_t n) {
    BufferOutStream* bos = (BufferOutStream*) s;

    size_t end = bos->cnt + n;
    if (end >= bos->cap) {
        TB_ASSERT(bos->cap);
        do {
            bos->cap *= 2;
        } while (end >= bos->cap);
        bos->data = cuik_realloc(bos->data, bos->cap);
    }

    char* dst = &bos->data[bos->cnt];
    bos->cnt = end;
    return dst;
}

static void bos_write(OutStream* s, size_t n, const char* data) {
    void* dst = bos_reserve(s, n);
    memcpy(dst, data, n);
}

static int bos_writef(OutStream* s, const char* fmt, va_list ap) {
    BufferOutStream* bos = (BufferOutStream*) s;

    // reserve size
    int n = vsnprintf(&bos->data[bos->cnt], bos->cap - bos->cnt, fmt, ap);
    TB_ASSERT(n >= 0);

    // do we need more space?
    int len = n + 1;
    if (len >= bos->cap - bos->cnt) {
        // write into full space
        void* dst = bos_reserve(s, len);
        TB_ASSERT(len < bos->cap - bos->cnt);

        vsnprintf(dst, len, fmt, ap);
        bos->cnt -= 1; // snprintf writes a null terminator, ignore it
    } else {
        bos->cnt += n;
    }
    return n;
}

static BufferOutStream bos_make(void) {
    BufferOutStream os = { .header = { NULL, bos_write, bos_writef }, .cap = 4096 };
    os.data = cuik_malloc(4096);
    return os;
}

static void s_newline(OutStream* s) {
    if (s->quoted) {
        s->write(s, 4, "\\r\\n");
    } else {
        s->write(s, 1, "\n");
    }
}

static void s_color(OutStream* s, int ansi_code) {
    #if TB_OPTDEBUG_ANSI
    if (s->color) {
        s->color(s, ansi_code);
    }
    #endif
}

static int s_writef(OutStream* s, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int n = s->writef(s, fmt, ap);
    va_end(ap);
    return n;
}

