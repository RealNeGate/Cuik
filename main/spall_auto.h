#ifndef SPALL_AUTO_H
#define SPALL_AUTO_H

// THIS IS EXPERIMENTAL, BUT VERY HANDY
// *should* work on clang/msvc on Windows, Mac, and Linux

#ifdef __cplusplus
extern "C" {
#endif

#define _GNU_SOURCE
#include <stdint.h>
#include <stddef.h>

typedef struct {
    char *str;
    int len;
} Name;

typedef struct {
    void *addr;
    Name name;
} SymEntry;

typedef struct {
    SymEntry *arr;
    uint64_t len;
    uint64_t cap;
} SymArr;

typedef struct {
    int64_t *arr;
    uint64_t len;
} HashArr;

typedef struct {
    SymArr  entries;
    HashArr hashes;
} AddrHash;

void spall_auto_init(char *filename);
void spall_auto_quit(void);
void spall_auto_thread_init(uint32_t _tid, size_t buffer_size, int64_t symbol_cache_size);
void spall_auto_thread_quit(void);
#if _MSC_VER && !__clang__
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
#define spall_auto_thread_init(tid, buffer_size, symbol_cache_size) (spall_auto_thread_init(tid, buffer_size, symbol_cache_size), spall__thread_on())
#define spall_auto_thread_quit() (spall__thread_off(), spall_auto_thread_quit())


#define SPALL_DEFAULT_BUFFER_SIZE (64 * 1024 * 1024)
#define SPALL_DEFAULT_SYMBOL_CACHE_SIZE (100000)

#ifdef __cplusplus
}
#endif
#endif

#ifdef SPALL_AUTO_IMPLEMENTATION

#ifndef SPALL_AUTO_IMPLEMENTED
#define SPALL_AUTO_IMPLEMENTED

#ifdef __cplusplus
extern "C" {
#endif

#include "spall.h"

static SpallProfile spall_ctx;
static AddrHash global_addr_map;
static _Thread_local SpallBuffer spall_buffer;
static _Thread_local AddrHash addr_map;
static _Thread_local uint32_t tid;
static _Thread_local bool spall_thread_running = false;

#include <stdlib.h>
#include <stdint.h>
#if !_WIN32
#include <dlfcn.h>
#include <time.h>
#include <pthread.h>
#include <unistd.h>
#else
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline unsigned long __builtin_clzl(uint64_t x) { unsigned long result; _BitScanReverse64(&result, x); return result ^ 63; }
static HANDLE process;
#if _MSC_VER && !__clang__
static DWORD spall_auto__tls_index = 0xFFFFFFFF;
#endif
#endif


// we're not checking overflow here...Don't do stupid things with input sizes
SPALL_FN uint64_t next_pow2(uint64_t x) {
    return 1ull << (64ull - __builtin_clzl(x - 1));
}

// This is not thread-safe... Use one per thread!
SPALL_FN void ah_init(AddrHash *ah, int64_t size) {
    ah->entries.cap = size;
    ah->entries.arr = (SymEntry *)calloc(sizeof(SymEntry), size);
    ah->entries.len = 0;

    ah->hashes.len = next_pow2(size);
    ah->hashes.arr = (int64_t *)malloc(sizeof(int64_t) * ah->hashes.len);

    for (uint64_t i = 0; i < ah->hashes.len; i++) {
        ah->hashes.arr[i] = -1;
    }
}

SPALL_FN void ah_free(AddrHash *ah) {
    free(ah->entries.arr);
    free(ah->hashes.arr);
    memset(ah, 0, sizeof(AddrHash));
}

// fibhash addresses
SPALL_FN int ah_hash(void *addr) {
    return (int)(((uint32_t)(uintptr_t)addr) * 2654435769);
}

#if !_WIN32
// Replace me with your platform's addr->name resolver if needed
SPALL_FN bool get_addr_name(void *addr, Name *name_ret) {
    Dl_info info;
    if (!dladdr(addr, &info)) {
        return false;
    }

    if (info.dli_sname != NULL) {
        char *str = (char *)info.dli_sname;
        size_t len = strlen(str);
        Name name;
        name.str = str;
        name.len = len;
        *name_ret = name;
        return true;
    }

    return false;
}
#else

#define TokenStream FuckYouWindows
#include <Dbghelp.h>
#undef TokenStream

#pragma comment(lib, "User32")
#pragma comment(lib, "Dbghelp")
#pragma comment(lib, "Synchronization")

SPALL_FN bool get_addr_name(void *addr, Name *name_ret) {
    bool result = false;
    struct {
        SYMBOL_INFO si;
        char name[256];
    } symbol = {{sizeof(symbol.si)}};
    symbol.si.MaxNameLen = sizeof(symbol.name);
    static LONG sym_lock = 0;
    LONG locked = 1;
    while (InterlockedCompareExchange(&sym_lock, locked, 0) != 0) {
        WaitOnAddress(&sym_lock, &locked, sizeof(locked), INFINITE);
    }
    {
        spall_buffer_flush(&spall_ctx, &spall_buffer);
        spall_buffer_begin_args(&spall_ctx, &spall_buffer, "Symbol Resolve", sizeof("Symbol Resolve") - 1, symbol.si.Name, symbol.si.NameLen, (double)__rdtsc(), tid, 0);
        if (SymFromAddr(process, (DWORD64)addr, NULL, &symbol.si)) {
            char *str = symbol.si.Name;
            size_t len = symbol.si.NameLen;
            Name name;
            name.str = (char *)memcpy(calloc(len + 1, 1), (void *)str, len);
            name.len = (int)len;
            *name_ret = name;
            result = true;
        }
        spall_buffer_end_ex(&spall_ctx, &spall_buffer, (double)__rdtsc(), tid, 0);
    }
    InterlockedExchange(&sym_lock, 0);
    WakeByAddressSingle(&sym_lock);
    return result;
}
#endif

SPALL_FN bool ah_insert(AddrHash *ah, void *addr, Name name) {
    int addr_hash = ah_hash(addr);
    uint64_t hv = ((uint64_t)addr_hash) & (ah->hashes.len - 1);
    for (uint64_t i = 0; i < ah->hashes.len; i++) {
        uint64_t idx = (hv + i) & (ah->hashes.len - 1);

        int64_t e_idx = ah->hashes.arr[idx];
        if (e_idx == -1) {
            SymEntry entry = {.addr = addr, .name = name};
            ah->hashes.arr[idx] = ah->entries.len;
            ah->entries.arr[ah->entries.len] = entry;
            ah->entries.len += 1;
            return true;
        }

        if ((uint64_t)ah->entries.arr[e_idx].addr == (uint64_t)addr) {
            return true;
        }
    }

    // The symbol map is full, make the symbol map bigger!
    return false;
}

SPALL_FN bool ah_get(AddrHash *ah, void *addr, Name *name_ret) {
    int addr_hash = ah_hash(addr);
    uint64_t hv = ((uint64_t)addr_hash) & (ah->hashes.len - 1);
    for (uint64_t i = 0; i < ah->hashes.len; i++) {
        uint64_t idx = (hv + i) & (ah->hashes.len - 1);

        int64_t e_idx = ah->hashes.arr[idx];
        if (e_idx == -1) {

            Name name;
            if (!get_addr_name(addr, &name)) {
                // Failed to get a name for the address!
                return false;
            }

            SymEntry entry = {.addr = addr, .name = name};
            ah->hashes.arr[idx] = ah->entries.len;
            ah->entries.arr[ah->entries.len] = entry;
            ah->entries.len += 1;

            *name_ret = name;
            return true;
        }

        if ((uint64_t)ah->entries.arr[e_idx].addr == (uint64_t)addr) {
            *name_ret = ah->entries.arr[e_idx].name;
            return true;
        }
    }

    // The symbol map is full, make the symbol map bigger!
    return false;
}

#ifdef __linux__
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <linux/perf_event.h>
#include <asm/unistd.h>
#include <sys/mman.h>

SPALL_FN uint64_t mul_u64_u32_shr(uint64_t cyc, uint32_t mult, uint32_t shift) {
    __uint128_t x = cyc;
    x *= mult;
    x >>= shift;
    return x;
}

SPALL_FN long perf_event_open(struct perf_event_attr *hw_event, pid_t pid,
    int cpu, int group_fd, unsigned long flags) {
    return syscall(__NR_perf_event_open, hw_event, pid, cpu, group_fd, flags);
}

SPALL_FN double get_rdtsc_multiplier() {
    struct perf_event_attr pe = {
        .type = PERF_TYPE_HARDWARE,
        .size = sizeof(struct perf_event_attr),
        .config = PERF_COUNT_HW_INSTRUCTIONS,
        .disabled = 1,
        .exclude_kernel = 1,
        .exclude_hv = 1
    };

    int fd = perf_event_open(&pe, 0, -1, -1, 0);
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
    double nanos = (double)mul_u64_u32_shr(1000000, pc->time_mult, pc->time_shift);
    return nanos / 1000000000;
}


#pragma pack(1)
typedef struct {
    uint8_t  ident[16];
    uint16_t type;
    uint16_t machine;
    uint32_t version;
    uint64_t entrypoint;
    uint64_t program_hdr_offset;
    uint64_t section_hdr_offset;
    uint32_t flags;
    uint16_t eh_size;
    uint16_t program_hdr_entry_size;
    uint16_t program_hdr_num;
    uint16_t section_hdr_entry_size;
    uint16_t section_hdr_num;
    uint16_t section_hdr_str_idx;
} ELF64_Header;

typedef struct {
    uint32_t name;
    uint32_t type;
    uint64_t flags;
    uint64_t addr;
    uint64_t offset;
    uint64_t size;
    uint32_t link;
    uint32_t info;
    uint64_t addr_align;
    uint64_t entry_size;
} ELF64_Section_Header;

typedef struct {
    uint32_t name;
    uint8_t  info;
    uint8_t  other;
    uint16_t section_hdr_idx;
    uint64_t value;
    uint64_t size;
} ELF64_Sym;
#pragma pack()

int load_self(AddrHash *ah) {
    int fd = open("/proc/self/exe", O_RDONLY);
    uint64_t length = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    #define round_size(addr, size) (((addr) + (size)) - ((addr) % (size)))
    uint64_t aligned_length = round_size(length, 0x1000);
    uint8_t *self = (uint8_t *)mmap(NULL, aligned_length, PROT_READ, MAP_FILE | MAP_SHARED, fd, 0);
    close(fd);

    ELF64_Header *elf_hdr = (ELF64_Header *)self;
    ELF64_Section_Header *section_hdr_table = (ELF64_Section_Header *)(self + elf_hdr->section_hdr_offset);
    ELF64_Section_Header *section_strtable_hdr = &section_hdr_table[elf_hdr->section_hdr_str_idx];
    char *strtable_ptr = (char *)self + section_strtable_hdr->offset;

    size_t symtab_idx = 0;
    size_t symstrtab_idx = 0;

    int i = 0;
    for (; i < elf_hdr->section_hdr_num; i += 1) {
        ELF64_Section_Header *s_hdr = &section_hdr_table[i];

        char *name = strtable_ptr + s_hdr->name;
        if (strncmp(name, ".symtab", sizeof(".symtab")) == 0) {
            symtab_idx = i;
            symstrtab_idx = s_hdr->link;
            break;
        }
    }
    if (i == elf_hdr->section_hdr_num) return 0;

    ELF64_Section_Header *symtab_section = (ELF64_Section_Header *)(
        self + elf_hdr->section_hdr_offset + (symtab_idx * elf_hdr->section_hdr_entry_size)
    );
    ELF64_Section_Header *symtab_str_section = (ELF64_Section_Header *)(
        self + elf_hdr->section_hdr_offset + (symstrtab_idx * elf_hdr->section_hdr_entry_size)
    );

    #define ELF64_ST_TYPE(info) ((info)&0xf)
    #define STT_FUNC 2
    for (size_t i = 0; i < symtab_section->size; i += symtab_section->entry_size) {
        ELF64_Sym *sym = (ELF64_Sym *)(self + symtab_section->offset + i);

        uint8_t type = ELF64_ST_TYPE(sym->info);
        if (type != STT_FUNC) {
            continue;
        }

        char *name_str = (char *)&self[symtab_str_section->offset + sym->name];

        // load global symbol cache!
        Name name;
        name.str = name_str;
        name.len = strlen(name_str);

        // if we're dealing with C++ BS, demangle symbols poorly
        if (name.len > 3 && name.str[0] == '_' && name.str[1] == 'Z' && name.str[2] == 'L') {
            uint64_t name_len = 0;
            int j = 3;
            for (; j < name.len; j++) {
                char ch = name.str[j];
                if (ch == '\0') {
                    break;
                }

                if (ch < '0' || ch > '9' || name_len > ((uint32_t)-1)) {
                    break;
                }

                name_len = (name_len * 10) + (uint32_t)(ch & 0xf);
            }
            name.str = name.str + j;
            name.len = SPALL_MIN(name_len, 255);
        }
        ah_insert(ah, (void *)sym->value, name);
    }

    return 1;
}
#elif __APPLE__
#include <sys/types.h>
#include <sys/sysctl.h>

int load_self(AddrHash *ah) {
    return 1;
}

SPALL_FN double get_rdtsc_multiplier() {
    uint64_t freq;
    size_t size = sizeof(freq);

    sysctlbyname("machdep.tsc.frequency", &freq, &size, NULL, 0);

    return 1000000.0 / (double)freq;
}
#elif _WIN32

SPALL_FN double get_rdtsc_multiplier(void) {

    // Cache the answer so that multiple calls never take the slow path more than once
    static double multiplier = 0;
    if (multiplier) {
        return multiplier;
    }

    uint64_t tsc_freq = 0;

    // Fast path: Load kernel-mapped memory page
    HMODULE ntdll = LoadLibraryA("ntdll.dll");
    if (ntdll) {

        int (*NtQuerySystemInformation)(int, void *, unsigned int, unsigned int *) =
        (int (*)(int, void *, unsigned int, unsigned int *))GetProcAddress(ntdll, "NtQuerySystemInformation");
        if (NtQuerySystemInformation) {

            volatile uint64_t *hypervisor_shared_page = NULL;
            unsigned int size = 0;

            // SystemHypervisorSharedPageInformation == 0xc5
            int result = (NtQuerySystemInformation)(0xc5, (void *)&hypervisor_shared_page, sizeof(hypervisor_shared_page), &size);

            // success
            if (size == sizeof(hypervisor_shared_page) && result >= 0) {
                // docs say ReferenceTime = ((VirtualTsc * TscScale) >> 64)
                //      set ReferenceTime = 10000000 = 1 second @ 10MHz, solve for VirtualTsc
                //       =>    VirtualTsc = 10000000 / (TscScale >> 64)
                tsc_freq = (10000000ull << 32) / (hypervisor_shared_page[1] >> 32);
                // If your build configuration supports 128 bit arithmetic, do this:
                // tsc_freq = ((unsigned __int128)10000000ull << (unsigned __int128)64ull) / hypervisor_shared_page[1];
            }
        }
        FreeLibrary(ntdll);
    }

    // Slow path
    if (!tsc_freq) {

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
    }

    // Failure case
    if (!tsc_freq) {
        tsc_freq = 1000000000;
    }

    multiplier = 1000000.0 / (double)tsc_freq;

    return multiplier;
}

void load_self(AddrHash *map) {
    (void)map;
}

#endif

SPALL_NOINSTRUMENT SPALL_FORCEINLINE void (spall_auto_thread_init)(uint32_t _tid, size_t buffer_size, int64_t symbol_cache_size) {
    uint8_t *buffer = (uint8_t *)malloc(buffer_size);
    spall_buffer = (SpallBuffer){ 0 };
    spall_buffer.data = buffer;
    spall_buffer.length = buffer_size;

    // removing initial page-fault bubbles to make the data a little more accurate, at the cost of thread spin-up time
    memset(buffer, 1, buffer_size);

    spall_buffer_init(&spall_ctx, &spall_buffer);

    tid = _tid;
    ah_init(&addr_map, symbol_cache_size);
    spall_thread_running = true;
}

void (spall_auto_thread_quit)(void) {
#if _MSC_VER && !__clang__
    TlsSetValue(spall_auto__tls_index, (void *)0);
#endif
    spall_thread_running = false;
    ah_free(&addr_map);
    spall_buffer_quit(&spall_ctx, &spall_buffer);
    free(spall_buffer.data);
}

void spall_auto_init(char *filename) {
    spall_ctx = spall_init_file_ex(filename, get_rdtsc_multiplier(), false);
    ah_init(&global_addr_map, 10000);
    load_self(&global_addr_map);
#if _WIN32
    static bool sym_initted = false;
    if (!sym_initted) {
        process = GetCurrentProcess();
        char temp_data[512];
        SpallBuffer temp = { temp_data, sizeof(temp_data) };
        spall_buffer_init(&spall_ctx, &temp);
        spall_buffer_begin(&spall_ctx, &temp, "SymInitialize", sizeof("SymInitialize") - 1, (double)__rdtsc());
        SymSetOptions(SYMOPT_DEFERRED_LOADS | SYMOPT_LOAD_LINES | SYMOPT_UNDNAME | SYMOPT_FAIL_CRITICAL_ERRORS | SYMOPT_DEFERRED_LOADS);
        SymInitialize(process, NULL, TRUE);
        spall_buffer_end(&spall_ctx, &temp, (double)__rdtsc());
        spall_buffer_quit(&spall_ctx, &temp);
    }
#if _MSC_VER && !__clang__
    if (spall_auto__tls_index == 0xFFFFFFFF) {
        spall_auto__tls_index = TlsAlloc();
    }
#endif
#endif
}

void spall_auto_quit(void) {
#if _WIN32
#if _MSC_VER && !__clang__
    if (spall_auto__tls_index != 0xFFFFFFFF) {
        TlsFree(spall_auto__tls_index);
        spall_auto__tls_index = 0xFFFFFFFF;
    }
#endif
#endif
    spall_quit(&spall_ctx);
}

#define not_found "(unknown name)" // only a macro to avoid bogged codegen
SPALL_NOINSTRUMENT void __cyg_profile_func_enter(void *fn, void *caller) {
    if (!spall_thread_running) {
        return;
    }
    spall_thread_running = false;

#if _MSC_VER && !__clang__
    fn = ((char*)fn - 5);
#endif

    Name name;
    if (
#if !_WIN32
        !ah_get(&global_addr_map, fn, &name) &&
#endif
        !ah_get(&addr_map, fn, &name)) {
      name = (Name){0};
      name.str = (char *)not_found;
      name.len = sizeof(not_found) - 1;
    }

    // printf("Begin: \"%s\"\n", name.str);
    spall_buffer_begin_ex(&spall_ctx, &spall_buffer, name.str, name.len, (double)__rdtsc(), tid, 0);
    // spall_buffer_flush(&spall_ctx, &spall_buffer);
    // spall_flush(&spall_ctx);
    spall_thread_running = true;
}

SPALL_NOINSTRUMENT void __cyg_profile_func_exit(void *fn, void *caller) {
    if (!spall_thread_running) {
        return;
    }
    spall_thread_running = false;

    // printf("End\n");
    spall_buffer_end_ex(&spall_ctx, &spall_buffer, (double)__rdtsc(), tid, 0);
    // spall_buffer_flush(&spall_ctx, &spall_buffer);
    // spall_flush(&spall_ctx);
    spall_thread_running = true;
}

#if _MSC_VER && !__clang__

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
