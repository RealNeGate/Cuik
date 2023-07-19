#ifndef SPALL_AUTO_H
#define SPALL_AUTO_H

// THIS IS EXPERIMENTAL, BUT VERY HANDY
// *should* work on clang/msvc on Windows, Mac, and Linux

#define SPALL_IS_WINDOWS 0
#define SPALL_IS_MSVC    0
#define SPALL_IS_DARWIN  0
#define SPALL_IS_LINUX   0

#if defined(_WIN32)
#undef SPALL_IS_WINDOWS
#define SPALL_IS_WINDOWS 1
#if !defined(__clang__)
#undef SPALL_IS_MSVC
#define SPALL_IS_MSVC 1
#endif
#elif defined(__APPLE__)
#undef SPALL_IS_DARWIN
#define SPALL_IS_DARWIN 1
#elif defined(__linux__)
#undef SPALL_IS_LINUX
#define SPALL_IS_LINUX 1
#endif

#ifdef __cplusplus
extern "C" {
    #endif

    #include <stdint.h>
    #include <stddef.h>
    #include <stdbool.h>

    bool spall_auto_init(char *filename);
    void spall_auto_quit(void);
    bool spall_auto_thread_init(uint32_t thread_id, size_t buffer_size);
    void spall_auto_thread_quit(void);

    #if SPALL_IS_MSVC
    #ifndef _PROCESSTHREADSAPI_H_
    extern __declspec(dllimport) int(__stdcall TlsSetValue)(unsigned long dwTlsIndex, void* lpTlsValue);
    #endif

    extern unsigned long spall_auto__tls_index; // DWORD
    #define spall__thread_on() TlsSetValue(spall_auto__tls_index, (void *)1)
    #define spall__thread_off() TlsSetValue(spall_auto__tls_index, (void *)0)
    #define _Thread_local __declspec( thread )
    #else
    #define spall__thread_off() 0
    #define spall__thread_on() 0
    #endif

    #define spall_auto_thread_init(thread_id, buffer_size) (spall_auto_thread_init(thread_id, buffer_size), spall__thread_on())
    #define spall_auto_thread_quit() (spall__thread_off(), spall_auto_thread_quit())

    #define SPALL_DEFAULT_BUFFER_SIZE (128 * 1024 * 1024)

    #ifdef __cplusplus
}
#endif
#endif // endif SPALL_AUTO_H

#ifdef SPALL_AUTO_IMPLEMENTATION
#ifndef SPALL_AUTO_IMPLEMENTED_H
#define SPALL_AUTO_IMPLEMENTED_H

#ifdef __cplusplus
extern "C" {
    #endif

    #include <stdint.h>
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <x86intrin.h>

    #if !SPALL_IS_WINDOWS
    #include <stdatomic.h>
    #include <time.h>
    #include <pthread.h>
    #include <unistd.h>
    #include <errno.h>
    #elif SPALL_IS_MSVC
    static DWORD spall_auto__tls_index = 0xFFFFFFFF;
    #endif

    #if SPALL_IS_WINDOWS
    #include <windows.h>
    #include <process.h>

    typedef ptrdiff_t ssize_t;
    typedef HANDLE Spall_ThreadHandle;

    #define spall_thread_start(t) ((t)->writer.thread = (HANDLE) _beginthread(spall_writer, 0, t))
    #define spall_thread_end(t)   WaitForSingleObject((t)->writer.thread, INFINITE)
    #else
    typedef pthread_t Spall_ThreadHandle;
    #define spall_thread_start(t) pthread_create(&(t)->writer.thread, NULL, spall_writer, (void *) (t))
    #define spall_thread_end(t)   pthread_join((t)->writer.thread, NULL)
    #endif

    #if SPALL_IS_MSVC
    #define _CRT_SECURE_NO_WARNINGS
    #define SPALL_NOINSTRUMENT // Can't noinstrument on MSVC!
    #define SPALL_FORCEINLINE __forceinline

    #define Spall_Atomic volatile
    #else
    #define SPALL_NOINSTRUMENT __attribute__((no_instrument_function))
    #define SPALL_FORCEINLINE __attribute__((always_inline))

    #define Spall_Atomic _Atomic
    #define __debugbreak() __builtin_trap()
    #endif

    #define SPALL_FN static inline SPALL_NOINSTRUMENT
    #define SPALL_MIN(a, b) (((a) < (b)) ? (a) : (b))

    #pragma pack(push, 1)

    typedef struct SpallHeader {
        uint64_t magic_header; // = 0xABADF00D
        uint64_t version; // = 1
        double   timestamp_unit;
        uint64_t known_address; // Address for spall_auto_init, for skew-correction
        uint16_t program_path_len;
    } SpallHeader;

    enum {
        SpallEventType_Invalid    = 0,
        SpallEventType_MicroBegin = 1,
        SpallEventType_MicroEnd   = 2,
    };

    typedef struct SpallMicroBeginEvent {
        uint64_t type_when;
        uint64_t address;
        uint64_t caller;
    } SpallMicroBeginEvent;

    typedef struct SpallMicroEndEvent {
        uint64_t type_when;
    } SpallMicroEndEvent;

    typedef struct SpallBufferHeader {
        uint32_t size;
        uint32_t tid;
    } SpallBufferHeader;

    #pragma pack(pop)

    typedef struct SpallProfile {
        double stamp_scale;
        FILE *file;
    } SpallProfile;

    typedef Spall_Atomic uint64_t Spall_Futex;
    typedef struct SpallBuffer {
        uint8_t *data;
        size_t   length;

        // if true, write to upper-half, else lower-half
        size_t sub_length;
        bool   write_half;

        struct {
            Spall_Atomic bool     is_running;
            Spall_ThreadHandle    thread;
            Spall_Atomic uint64_t ptr;
            Spall_Atomic size_t   size;
        } writer;

        size_t   head;
        uint32_t thread_id;
    } SpallBuffer;


    // Cross-platform wrappers
    #if SPALL_IS_LINUX
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <unistd.h>
    #include <fcntl.h>
    #include <sys/syscall.h>
    #include <sys/mman.h>
    #include <asm/unistd.h>
    #include <linux/futex.h>
    #include <linux/limits.h>
    #include <linux/perf_event.h>

    SPALL_FN bool get_program_path(char **out_path) {
        char path[PATH_MAX] = {0};
        uint32_t size = sizeof(path);

        ssize_t buff_len = (ssize_t)readlink("/proc/self/exe", path, size - 1);
        if (buff_len == -1) {
            *out_path = NULL;
            return false;
        }

        char *post_path = (char *)calloc(PATH_MAX, 1);
        if (realpath(path, post_path) == NULL) {
            free(post_path);
            *out_path = NULL;
            return false;
        }

        *out_path = post_path;
        return true;
    }

    SPALL_FN uint64_t mul_u64_u32_shr(uint64_t cyc, uint32_t mult, uint32_t shift) {
        __uint128_t x = cyc;
        x *= mult;
        x >>= shift;
        return (uint64_t)x;
    }

    SPALL_FN long perf_event_open(struct perf_event_attr *hw_event, pid_t pid, int cpu, int group_fd, unsigned long flags) {
        return syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
    }

    SPALL_FN double get_rdtsc_multiplier(void) {
        struct perf_event_attr pe = {
            .type = PERF_TYPE_HARDWARE,
            .size = sizeof(struct perf_event_attr),
            .config = PERF_COUNT_HW_INSTRUCTIONS,
            .disabled = 1,
            .exclude_kernel = 1,
            .exclude_hv = 1
        };

        int fd = (int)perf_event_open(&pe, 0, -1, -1, 0);
        if (fd == -1) {
            perror("perf_event_open failed");
            return 1;
        }
        void *addr = mmap(NULL, 4*1024, PROT_READ, MAP_SHARED, fd, 0);
        if (!addr) {
            perror("mmap failed");
            return 1;
        }
        struct perf_event_mmap_page *pc = (struct perf_event_mmap_page *)addr;
        if (pc->cap_user_time != 1) {
            fprintf(stderr, "Perf system doesn't support user time\n");
            return 1;
        }
        double nanos = (double)mul_u64_u32_shr(1000000000000000ull, pc->time_mult, pc->time_shift);
        double multiplier = nanos / 1000000000000000.0;
        return multiplier;
    }


    SPALL_FN SPALL_FORCEINLINE void spall_signal(Spall_Futex *addr) {
        long ret = syscall(SYS_futex, addr, FUTEX_WAKE | FUTEX_PRIVATE_FLAG, 1, NULL, NULL, 0);
        if (ret == -1) {
            perror("Futex wake");
            __debugbreak();
        }
    }

    SPALL_FN SPALL_FORCEINLINE void spall_wait(Spall_Futex *addr, Spall_Futex val) {
        for (;;) {
            long ret = syscall(SYS_futex, addr, FUTEX_WAIT | FUTEX_PRIVATE_FLAG, val, NULL, NULL, 0);
            if (ret == -1) {
                if (errno != EAGAIN) {
                    perror("Futex wait");
                    __debugbreak();
                } else {
                    return;
                }
            } else if (ret == 0) {
                return;
            }
        }
    }

    #elif SPALL_IS_DARWIN

    #include <mach-o/dyld.h>
    #include <sys/types.h>
    #include <sys/sysctl.h>

    SPALL_FN double get_rdtsc_multiplier(void) {
        uint64_t freq;
        size_t size = sizeof(freq);

        sysctlbyname("machdep.tsc.frequency", &freq, &size, NULL, 0);
        return 1000000000.0 / (double)freq;
    }

    SPALL_FN bool get_program_path(char **out_path) {
        char pre_path[1025];
        uint32_t size = sizeof(pre_path);
        if (_NSGetExecutablePath(pre_path, &size) == -1) {
            *out_path = NULL;
            return false;
        }

        char *post_path = (char *)malloc(1025);
        if (realpath(pre_path, post_path) == NULL) {
            free(post_path);
            *out_path = NULL;
            return false;
        }

        *out_path = post_path;
        return true;
    }

    #define UL_COMPARE_AND_WAIT 0x00000001
    #define ULF_WAKE_ALL        0x00000100
    #define ULF_NO_ERRNO        0x01000000

    /* timeout is specified in microseconds */
    int __ulock_wait(uint32_t operation, void *addr, uint64_t value, uint32_t timeout);
    int __ulock_wake(uint32_t operation, void *addr, uint64_t wake_value);

    SPALL_FN SPALL_FORCEINLINE void spall_signal(Spall_Futex *addr) {
        for (;;) {
            int ret = __ulock_wake(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, addr, 0);
            if (ret >= 0) {
                return;
            }
            ret = -ret;
            if (ret == EINTR || ret == EFAULT) {
                continue;
            }
            if (ret == ENOENT) {
                return;
            }
            printf("futex signal fail?\n");
            __debugbreak();
        }
    }

    SPALL_FN SPALL_FORCEINLINE void spall_wait(Spall_Futex *addr, Spall_Futex val) {
        for (;;) {
            int ret = __ulock_wait(UL_COMPARE_AND_WAIT | ULF_NO_ERRNO, addr, val, 0);
            if (ret >= 0) {
                return;
            }
            ret = -ret;
            if (ret == EINTR || ret == EFAULT) {
                continue;
            }
            if (ret == ENOENT) {
                return;
            }

            printf("futex wait fail? %d\n", ret);
            __debugbreak();
        }
    }

    #elif SPALL_IS_WINDOWS

    SPALL_FN bool get_program_path(char **out_path) {
        char *post_path = (char *)calloc(MAX_PATH, 1);
        if (GetModuleFileNameA(NULL, post_path, MAX_PATH) == 0) {
            *out_path = NULL;
            return false;
        }

        *out_path = post_path;
        return true;
    }

    SPALL_FN SPALL_FORCEINLINE double get_rdtsc_multiplier(void) {

        // Cache the answer so that multiple calls never take the slow path more than once
        static double multiplier = 0;
        if (multiplier) {
            return multiplier;
        }

        uint64_t tsc_freq = 0;

        // Get time before sleep
        uint64_t qpc_begin = 0; QueryPerformanceCounter((LARGE_INTEGER *)&qpc_begin);
        uint64_t tsc_begin = __rdtsc();

        Sleep(2);

        // Get time after sleep
        uint64_t qpc_end = qpc_begin + 1; QueryPerformanceCounter((LARGE_INTEGER *)&qpc_end);
        uint64_t tsc_end = __rdtsc();

        // Do the math to extrapolate the RDTSC ticks elapsed in 1 second
        uint64_t qpc_freq = 0; QueryPerformanceFrequency((LARGE_INTEGER *)&qpc_freq);
        tsc_freq = (tsc_end - tsc_begin) * qpc_freq / (qpc_end - qpc_begin);

        multiplier = 1000000000.0 / (double)tsc_freq;
        return multiplier;
    }

    SPALL_FN SPALL_FORCEINLINE void spall_signal(Spall_Futex *addr) {
        WakeByAddressSingle((void *)addr);
    }

    SPALL_FN SPALL_FORCEINLINE void spall_wait(Spall_Futex *addr, Spall_Futex val) {
        WaitOnAddress(addr, (void *)&val, sizeof(val), INFINITE);
    }

    #endif

    // Auto-tracing impl
    static SpallProfile spall_ctx;
    static _Thread_local SpallBuffer *spall_buffer = NULL;
    static _Thread_local bool spall_thread_running = false;

    #if SPALL_IS_WINDOWS
    SPALL_FN void spall_writer(void *arg) {
        #else
        SPALL_FN void *spall_writer(void *arg) {
            #endif

            SpallBuffer *buffer = (SpallBuffer *)arg;
            while (buffer->writer.is_running) {
                spall_wait(&buffer->writer.ptr, 0);
                if (!buffer->writer.is_running) { break; }
                if (buffer->writer.ptr == 0) { continue; }

                size_t size = buffer->writer.size;
                void *buffer_ptr = (void *)buffer->writer.ptr;
                buffer->writer.ptr = 0;

                fwrite(buffer_ptr, size, 1, spall_ctx.file);
            }

            #if !SPALL_IS_WINDOWS
            return NULL;
            #endif
        }

        SPALL_FN SPALL_FORCEINLINE bool spall__file_write(void *p, size_t n) {
            spall_buffer->writer.size = n;
            spall_buffer->writer.ptr = (uint64_t)p;
            spall_signal(&spall_buffer->writer.ptr);

            while (spall_buffer->writer.ptr != 0) { _mm_pause(); }

            return true;
        }

        SPALL_FN SPALL_FORCEINLINE bool spall_buffer_flush(void) {
            if (!spall_buffer) return false;

            size_t data_start = spall_buffer->write_half ? spall_buffer->sub_length : 0;

            SpallBufferHeader *sbp = (SpallBufferHeader *)(spall_buffer->data + data_start);
            if (spall_buffer->head > 0) {
                sbp->size = (uint32_t)(spall_buffer->head - sizeof(SpallBufferHeader));
                if (!spall__file_write(spall_buffer->data + data_start, spall_buffer->head)) return false;

                spall_buffer->write_half = !spall_buffer->write_half;
            }

            data_start = spall_buffer->write_half ? spall_buffer->sub_length : 0;
            sbp = (SpallBufferHeader *)(spall_buffer->data + data_start);
            sbp->size = 0;
            sbp->tid = spall_buffer->thread_id;

            spall_buffer->head = sizeof(SpallBufferHeader);
            return true;
        }

        SPALL_FN SPALL_FORCEINLINE bool spall_buffer_micro_begin(uint64_t addr, uint64_t caller) {
            size_t ev_size = sizeof(SpallMicroBeginEvent);
            if ((spall_buffer->head + ev_size) > spall_buffer->sub_length) {
                if (!spall_buffer_flush()) {
                    return false;
                }
            }

            size_t data_start = spall_buffer->write_half ? spall_buffer->sub_length : 0;
            SpallMicroBeginEvent *ev = (SpallMicroBeginEvent *)((spall_buffer->data + data_start) + spall_buffer->head);

            uint64_t mask = ((uint64_t)0xFF) << (8 * 7);
            uint64_t type_b = ((uint64_t)(uint8_t)SpallEventType_MicroBegin) << (8 * 7);
            uint64_t when = __rdtsc();
            ev->type_when = (~mask & when) | type_b;
            ev->address = addr;
            ev->caller = caller;

            spall_buffer->head += ev_size;
            return true;
        }
        SPALL_FN SPALL_FORCEINLINE bool spall_buffer_micro_end(void) {
            uint64_t when = __rdtsc();

            size_t ev_size = sizeof(SpallMicroEndEvent);
            if ((spall_buffer->head + ev_size) > spall_buffer->sub_length) {
                if (!spall_buffer_flush()) {
                    return false;
                }
            }

            size_t data_start = spall_buffer->write_half ? spall_buffer->sub_length : 0;
            SpallMicroEndEvent *ev = (SpallMicroEndEvent *)(((char *)spall_buffer->data + data_start) + spall_buffer->head);

            uint64_t mask = ((uint64_t)0xFF) << (8 * 7);
            uint64_t type_b = ((uint64_t)(uint8_t)SpallEventType_MicroEnd) << (8 * 7);
            ev->type_when = (~mask & when) | type_b;

            spall_buffer->head += ev_size;
            return true;
        }


        SPALL_NOINSTRUMENT SPALL_FORCEINLINE bool (spall_auto_thread_init)(uint32_t thread_id, size_t buffer_size) {
            if (buffer_size < 512) { return false; }
            if (spall_buffer != NULL) { return false; }

            spall_buffer = (SpallBuffer *)calloc(sizeof(SpallBuffer), 1);
            spall_buffer->data = (uint8_t *)malloc(buffer_size);
            spall_buffer->length = buffer_size;
            spall_buffer->thread_id = thread_id;
            spall_buffer->sub_length = buffer_size / 2;

            // removing initial page-fault bubbles to make the data a little more accurate, at the cost of thread spin-up time
            memset(spall_buffer->data, 1, spall_buffer->length);

            spall_buffer->writer.is_running = true;
            spall_thread_start(spall_buffer);

            spall_buffer_flush();
            spall_thread_running = true;
            return true;
        }

        void (spall_auto_thread_quit)(void) {
            #if SPALL_IS_MSVC
            TlsSetValue(spall_auto__tls_index, (void *)0);
            #endif
            spall_thread_running = false;
            spall_buffer_flush();

            spall_buffer->writer.is_running = false;
            spall_buffer->writer.ptr = 1;
            spall_signal(&spall_buffer->writer.ptr);
            spall_thread_end(spall_buffer);

            free(spall_buffer->data);
            free(spall_buffer);
            spall_buffer = NULL;
        }

        bool spall_auto_init(char *filename) {
            if (!filename) return false;
            memset(&spall_ctx, 0, sizeof(spall_ctx));

            spall_ctx.file = fopen(filename, "wb"); // TODO: handle utf8 and long paths on windows
            if (spall_ctx.file) { // basically freopen() but we don't want to force users to lug along another macro define
                fclose(spall_ctx.file);
                spall_ctx.file = fopen(filename, "ab");
            }
            if (!spall_ctx.file) { return false; }

            spall_ctx.stamp_scale = get_rdtsc_multiplier();
            SpallHeader header = {0};
            header.magic_header = 0xABADF00D;
            header.version = 1;
            header.timestamp_unit = spall_ctx.stamp_scale;
            header.known_address = (uint64_t)spall_auto_init;

            char *program_path;
            if (!get_program_path(&program_path)) { return false; }
            uint16_t program_path_len = (uint16_t)strlen(program_path);

            header.program_path_len = program_path_len;

            size_t full_header_size = sizeof(SpallHeader) + (size_t)program_path_len;
            uint8_t *full_header = (uint8_t *)malloc(full_header_size);
            memcpy(full_header, &header, sizeof(SpallHeader));
            memcpy(full_header + sizeof(SpallHeader), program_path, program_path_len);

            size_t write_ret = fwrite(full_header, 1, full_header_size, spall_ctx.file);
            if (write_ret < full_header_size) { return false; }

            free(full_header);

            #if SPALL_IS_MSVC
            if (spall_auto__tls_index == 0xFFFFFFFF) {
                spall_auto__tls_index = TlsAlloc();
            }
            #endif

            return true;
        }

        void spall_auto_quit(void) {
            #if SPALL_IS_MSVC
            if (spall_auto__tls_index != 0xFFFFFFFF) {
                TlsFree(spall_auto__tls_index);
                spall_auto__tls_index = 0xFFFFFFFF;
            }
            #endif
        }

        SPALL_NOINSTRUMENT void __cyg_profile_func_enter(void *fn, void *caller) {
            if (!spall_thread_running) {
                return;
            }
            #if SPALL_IS_MSVC
            fn = ((char*)fn - 5);
            #endif

            spall_thread_running = false;
            spall_buffer_micro_begin((uint64_t)fn, (uint64_t)caller);
            spall_thread_running = true;
        }

        SPALL_NOINSTRUMENT void __cyg_profile_func_exit(void *fn, void *caller) {
            if (!spall_thread_running) {
                return;
            }

            spall_thread_running = false;
            spall_buffer_micro_end();
            spall_thread_running = true;
        }

        #if SPALL_IS_MSVC
        #define BE(_0,_1,_2,_3,_4,_5,_6,_7,NOTHING) \
        ((uin ## NOTHING ## t64_t)(_7) << 56 | \
            (uin ## NOTHING ## t64_t)(_6) << 48 | \
            (uin ## NOTHING ## t64_t)(_5) << 40 | \
            (uin ## NOTHING ## t64_t)(_4) << 32 | \
            (uin ## NOTHING ## t64_t)(_3) << 24 | \
            (uin ## NOTHING ## t64_t)(_2) << 16 | \
            (uin ## NOTHING ## t64_t)(_1) <<  8 | \
            (uin ## NOTHING ## t64_t)(_0) <<  0)

        #define PHOOK_CAT__(a, b) a##b
        #define PHOOK_CAT_(a, b) PHOOK_CAT__(a, b)
        #define PHOOK_CAT(a, b) PHOOK_CAT_(a, b)
        #define PHOOK_UNWRAP(...) __VA_ARGS__
        #define PHOOK_IF_1(a, b) a
        #define PHOOK_IF_0(a, b) b
        #define PHOOK_IF(cond, a, b) PHOOK_CAT(PHOOK_IF_, cond)(PHOOK_UNWRAP a, PHOOK_UNWRAP b)
        #define PHOOK(name, ENT, dest) \
        __declspec(allocate(".text")) __declspec(dllexport) extern const uint64_t name[] = { \
            BE( \
                0x0F,0x1F,0x00,                                         /* nop */ \
                0x9C,                                                   /* pushf */ \
                0x50,                                                   /* push rax */ \
                0x51,                                                   /* push rcx */ \
                0x48,0xB8,                                              /* mov rax, spall_auto__tls_index */ \
            ),(uint64_t)&spall_auto__tls_index,BE(                      /* abs64 relocation */ \
                0x83,0x38,0xFF,                                         /* cmp qword ptr [rax], 0xFFFFFFFF */ \
                0x75,0x04,                                              /* jne IS_PROFILING */ \
                /* REENTRANT: */ \
                0x59,                                                   /* pop rcx */ \
                0x58,                                                   /* pop rax */ \
                0x9D,                                                   /* popf */ \
            ),BE( \
                0xC3,                                                   /* ret */ \
                /* IS_PROFILING: */ \
                0x8B,0x08,                                              /* mov ecx, dword ptr [rax] */ \
                /* check if we're already inside of penter, if so then don't call again */ \
                0x65,0x48,0x8B,0x04,0x25,),BE(0x30,0x00,0x00,0x00,      /* mov rax, qword ptr gs:[0x30] */ \
                0x48,0x8D,0x8C,0xC8,),BE(0x80,0x14,0x00,0x00,           /* lea rcx, [rax+rcx*8+0x1480] */ \
                0x83,0x39,0x01,                                         /* cmp dword ptr [rcx], 1 */ \
                0x75,),BE(0xE4,                                         /* jne REENTRANT */ \
                0x49,0x50,                                              /* push r8 */ \
                /* clone rsp into rdx and round down to 16 byte boundary to satisfy ABI */ \
                0x52,                                                   /* push rdx */ \
                0x48,0x89,0xE2,                                         /* mov rdx, rsp */ \
                0x48,),BE(0x83,0xE4,0xF0,                               /* and rsp, 0xfffffffffffffff0 */ \
                0xC6,0x01,0x00,                                         /* mov byte ptr [rcx], 0 */ \
                0x48,0xB8,                                              /* mov rax, spall_auto_trace */ \
            ),(uint64_t)dest,BE(                                        /* abs64 relocation */ \
                0x52,                                                   /* push rdx */ \
                0x49,0x51,                                              /* push r9 */ \
                0x49,0x52,                                              /* push r10 */ \
                0x49,0x53,                                              /* push r11 */ \
                0x51,                                                   /* push rcx */ \
            ),BE( \
                0x48,0x81,0xEC,0x88,0x00,0x00,0x00,                     /* sub rsp, 0x88 */ \
                0x0F,),BE(0x29,0x44,0x24,0x70,                          /* movaps xmmword ptr [rsp+0x70], xmm0 */ \
                0x0F,0x29,0x4C,0x24,),BE(0x60,                          /* movaps xmmword ptr [rsp+0x60], xmm1 */ \
                0x0F,0x29,0x54,0x24,0x50,                               /* movaps xmmword ptr [rsp+0x50], xmm2 */ \
                0x0F,0x29,),BE(0x5C,0x24,0x40,                          /* movaps xmmword ptr [rsp+0x40], xmm3 */ \
                0x0F,0x29,0x64,0x24,0x30,                               /* movaps xmmword ptr [rsp+0x30], xmm4 */ \
            ), \
            PHOOK_IF(ENT, (                                             /* if ENT */ \
                    BE( \
                        0x0F,0x29,0x6C,0x24,0x20,                               /* movaps xmmword ptr [rsp+0x20], xmm5 */ \
                        0x48,0x8B,0x4A,),BE(0x28,                               /* mov rcx, [rdx+0x28] */ \
                        0xFF,0xD0,                                              /* call rax */ \
                        0x0F,0x28,0x6C,0x24,0x20,                               /* movaps xmm3, xmmword ptr [rsp+0x20] */ \
                    ) \
                ),(                                                         /* else */ \
                    BE( \
                        0x0F,0x29,0x6C,0x24,0x20,                               /* movaps xmmword ptr [rsp+0x20], xmm5 */ \
                        0x0F,0x1F,0x40,),BE(0x00,                               /* nop */ \
                        0xFF,0xD0,                                              /* call rax */ \
                        0x0F,0x28,0x6C,0x24,0x20,                               /* movaps xmm3, xmmword ptr [rsp+0x20] */ \
                    ) \
                )),                                                         /* endif */ \
            BE( \
                0x0F,0x28,0x64,0x24,0x30,                               /* movaps xmm3, xmmword ptr [rsp+0x30] */ \
                0x0F,0x28,0x5C,),BE(0x24,0x40,                          /* movaps xmm3, xmmword ptr [rsp+0x40] */ \
                0x0F,0x28,0x54,0x24,0x50,                               /* movaps xmm2, xmmword ptr [rsp+0x50] */ \
                0x0F,),BE(0x28,0x4C,0x24,0x60,                          /* movaps xmm1, xmmword ptr [rsp+0x60] */ \
                0x0F,0x28,0x44,0x24,),BE(0x70,                          /* movaps xmm0, xmmword ptr [rsp+0x70] */ \
                0x48,0x81,0xC4,0x88,0x00,0x00,0x00,                     /* add rsp, 0x88 */ \
            ),BE( \
                0x59,                                                   /* pop rcx */ \
                0xC6,0x01,0x01,                                         /* mov byte ptr [rcx], 1 */ \
                0x49,0x5B,                                              /* pop r11 */ \
                0x49,0x5A,                                              /* pop r10 */ \
            ),BE( \
                0x49,0x59,                                              /* pop r9 */ \
                0x5C,                                                   /* pop rsp */ \
                0x5A,                                                   /* pop rdx */ \
                0x49,0x58,                                              /* pop r8 */ \
                0x59,                                                   /* pop rcx */ \
                0x58,                                                   /* pop rax */ \
            ),BE( \
                0x9D,                                                   /* popf */ \
                0xC3,                                                   /* ret */ \
                0xCC,                                                   /* int3 */ \
                0xCC,                                                   /* int3 */ \
                0xCC,                                                   /* int3 */ \
                0xCC,                                                   /* int3 */ \
                0xCC,                                                   /* int3 */ \
                0xCC,                                                   /* int3 */ \
            ), \
        };

        #pragma code_seg(".text")
        PHOOK(_penter, 1, __cyg_profile_func_enter);
        PHOOK(_pexit, 0, __cyg_profile_func_exit);
        #endif

        #ifdef __cplusplus
    }
    #endif

    #endif
    #endif