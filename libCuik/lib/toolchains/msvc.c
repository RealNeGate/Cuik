// Author:   Jonathan Blow
// Version:  1
// Date:     31 August, 2018
//
// This code is released under the MIT license, which you can find at
//
//          https://opensource.org/licenses/MIT
//
#include <cuik.h>
#include <common.h>
#include <log.h>

// OS Char specific goods
#if _WIN32
#define env_get(k) _wgetenv(L ## k)
#define str_copy(dst, src, n) wcsncpy(dst, src, n)
#define str_printf(buf, n, fmt, ...) swprintf(buf, n, L ## fmt, __VA_ARGS__)
#define str_rfind(a, b) wcsrchr(a, b)

#define SLASH "\\"
#define STR_FMT "%S"
typedef wchar_t OSChar;
#else
#define str_copy(dst, src, n) strncpy(dst, src, n)
#define env_get(k) getenv(k)
#define str_printf(buf, n, fmt, ...) snprintf(buf, n, fmt, __VA_ARGS__)
#define str_rfind(a, b) strrchr(a, b)

#define SLASH "/"
#define STR_FMT "%s"
typedef char OSChar;
#endif

typedef struct {
    int windows_sdk_version; // Zero if no Windows SDK found.

    OSChar windows_sdk_include[FILENAME_MAX];
    OSChar windows_sdk_root[FILENAME_MAX];

    OSChar vs_exe_path[FILENAME_MAX];
    OSChar vc_tools_install[FILENAME_MAX];
    OSChar vs_library_path[FILENAME_MAX];
    OSChar vs_include_path[FILENAME_MAX];
} Cuik_WindowsToolchain;

#ifdef _WIN32
#include <windows.h>
#include <io.h>         // For _get_osfhandle

#pragma comment(lib, "Advapi32.lib")
#pragma comment(lib, "Ole32.lib")
#pragma comment(lib, "OleAut32.lib")

// COM objects for the ridiculous Microsoft craziness.
#undef  INTERFACE
#define INTERFACE ISetupInstance
DECLARE_INTERFACE_ (ISetupInstance, IUnknown)
{
    BEGIN_INTERFACE;

    // IUnknown methods
    STDMETHOD (QueryInterface)   (THIS_  REFIID, void **) PURE;
    STDMETHOD_(ULONG, AddRef)    (THIS) PURE;
    STDMETHOD_(ULONG, Release)   (THIS) PURE;

    // ISetupInstance methods
    STDMETHOD(GetInstanceId)(THIS_ _Out_ BSTR* pbstrInstanceId) PURE;
    STDMETHOD(GetInstallDate)(THIS_ _Out_ LPFILETIME pInstallDate) PURE;
    STDMETHOD(GetInstallationName)(THIS_ _Out_ BSTR* pbstrInstallationName) PURE;
    STDMETHOD(GetInstallationPath)(THIS_ _Out_ BSTR* pbstrInstallationPath) PURE;
    STDMETHOD(GetInstallationVersion)(THIS_ _Out_ BSTR* pbstrInstallationVersion) PURE;
    STDMETHOD(GetDisplayName)(THIS_ _In_ LCID lcid, _Out_ BSTR* pbstrDisplayName) PURE;
    STDMETHOD(GetDescription)(THIS_ _In_ LCID lcid, _Out_ BSTR* pbstrDescription) PURE;
    STDMETHOD(ResolvePath)(THIS_ _In_opt_z_ LPCOLESTR pwszRelativePath, _Out_ BSTR* pbstrAbsolutePath) PURE;

    END_INTERFACE
};

#undef  INTERFACE
#define INTERFACE IEnumSetupInstances
DECLARE_INTERFACE_ (IEnumSetupInstances, IUnknown)
{
    BEGIN_INTERFACE;

    // IUnknown methods
    STDMETHOD (QueryInterface)   (THIS_  REFIID, void **) PURE;
    STDMETHOD_(ULONG, AddRef)    (THIS) PURE;
    STDMETHOD_(ULONG, Release)   (THIS) PURE;

    // IEnumSetupInstances methods
    STDMETHOD(Next)(THIS_ _In_ ULONG celt, _Out_writes_to_(celt, *pceltFetched) ISetupInstance** rgelt, _Out_opt_ _Deref_out_range_(0, celt) ULONG* pceltFetched) PURE;
    STDMETHOD(Skip)(THIS_ _In_ ULONG celt) PURE;
    STDMETHOD(Reset)(THIS) PURE;
    STDMETHOD(Clone)(THIS_ _Deref_out_opt_ IEnumSetupInstances** ppenum) PURE;

    END_INTERFACE
};

#undef  INTERFACE
#define INTERFACE ISetupConfiguration
DECLARE_INTERFACE_ (ISetupConfiguration, IUnknown)
{
    BEGIN_INTERFACE

        // IUnknown methods
        STDMETHOD (QueryInterface)   (THIS_  REFIID, void **) PURE;
    STDMETHOD_(ULONG, AddRef)    (THIS) PURE;
    STDMETHOD_(ULONG, Release)   (THIS) PURE;

    // ISetupConfiguration methods
    STDMETHOD(EnumInstances)(THIS_ _Out_ IEnumSetupInstances** ppEnumInstances) PURE;
    STDMETHOD(GetInstanceForCurrentProcess)(THIS_ _Out_ ISetupInstance** ppInstance) PURE;
    STDMETHOD(GetInstanceForPath)(THIS_ _In_z_ LPCWSTR wzPath, _Out_ ISetupInstance** ppInstance) PURE;

    END_INTERFACE
};

#ifdef __cplusplus
#define CALL_STDMETHOD(object, method, ...) object->method(__VA_ARGS__)
#define CALL_STDMETHOD_(object, method) object->method()
#else
#define CALL_STDMETHOD(object, method, ...) object->lpVtbl->method(object, __VA_ARGS__)
#define CALL_STDMETHOD_(object, method) object->lpVtbl->method(object)
#endif


// The beginning of the actual code that does things.

typedef struct {
    int32_t best_version[4];  // For Windows 8 versions, only two of these numbers are used.
    wchar_t* best_name; // [FILENAME_MAX]
} Version_Data;

static bool os_file_exists(wchar_t *name) {
    // @Robustness: What flags do we really want to check here?

    int attrib = GetFileAttributesW(name);
    if (attrib == INVALID_FILE_ATTRIBUTES) return false;
    if (attrib & FILE_ATTRIBUTE_DIRECTORY) return false;

    return true;
}

#define concat2(a, b) concat(a, b, NULL, NULL)
#define concat3(a, b, c) concat(a, b, c, NULL)
#define concat4(a, b, c, d) concat(a, b, c, d)
static wchar_t *concat(wchar_t *a, wchar_t *b, wchar_t *c, wchar_t *d) {
    // Concatenate up to 4 wide strings together. Allocated with malloc.
    // If you don't like that, use a programming language that actually
    // helps you with using custom allocators. Or just edit the code.

    int len_a = wcslen(a);
    int len_b = wcslen(b);

    int len_c = 0;
    if (c) len_c = wcslen(c);

    int len_d = 0;
    if (d) len_d = wcslen(d);

    wchar_t *result = (wchar_t *)cuik_malloc((len_a + len_b + len_c + len_d + 1) * 2);
    memcpy(result, a, len_a*2);
    memcpy(result + len_a, b, len_b*2);

    if (c) memcpy(result + len_a + len_b, c, len_c * 2);
    if (d) memcpy(result + len_a + len_b + len_c, d, len_d * 2);

    result[len_a + len_b + len_c + len_d] = 0;

    return result;
}

typedef bool (*Visit_Proc_W)(wchar_t *short_name, Version_Data *data);
static bool visit_files_w(wchar_t *dir_name, Version_Data *data, Visit_Proc_W proc) {
    data->best_name[0] = 0;

    // Visit everything in one folder (non-recursively). If it's a directory
    // that doesn't start with ".", call the visit proc on it. The visit proc
    // will see if the filename conforms to the expected versioning pattern.
    WIN32_FIND_DATAW find_data;

    wchar_t *wildcard_name = concat2(dir_name, L"\\*");
    HANDLE handle = FindFirstFileW(wildcard_name, &find_data);
    cuik_free(wildcard_name);

    if (handle == INVALID_HANDLE_VALUE) return false;

    while (true) {
        if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && (find_data.cFileName[0] != '.')) {
            if (proc(find_data.cFileName, data)) {
                swprintf(data->best_name, FILENAME_MAX, L"%s\\%s", dir_name, find_data.cFileName);
            }
        }

        BOOL success = FindNextFileW(handle, &find_data);
        if (!success) break;
    }

    FindClose(handle);
    return true;
}

static wchar_t *find_windows_kit_root_with_key(HKEY key, wchar_t *version) {
    // Given a key to an already opened registry entry,
    // get the value stored under the 'version' subkey.
    // If that's not the right terminology, hey, I never do registry stuff.

    DWORD required_length;
    int rc = RegQueryValueExW(key, version, NULL, NULL, NULL, &required_length);
    if (rc != 0)  return NULL;

    DWORD length = required_length + 2;  // The +2 is for the maybe optional zero later on. Probably we are over-allocating.
    wchar_t *value = (wchar_t *)cuik_malloc(length * sizeof(wchar_t));
    if (!value) return NULL;

    rc = RegQueryValueExW(key, version, NULL, NULL, (LPBYTE)value, &length);  // We know that version is zero-terminated...
    if (rc != 0)  return NULL;

    // The documentation says that if the string for some reason was not stored
    // with zero-termination, we need to manually terminate it. Sigh!!

    if (value[length]) {
        value[length+1] = 0;
    }

    return value;
}

static bool win10_best(wchar_t *short_name, Version_Data *data) {
    // Find the Windows 10 subdirectory with the highest version number.
    int i0, i1, i2, i3;
    int success = swscanf_s(short_name, L"%d.%d.%d.%d", &i0, &i1, &i2, &i3);
    if (success < 4) return false;

    if (i0 < data->best_version[0]) return false;
    else if (i0 == data->best_version[0]) {
        if (i1 < data->best_version[1]) return false;
        else if (i1 == data->best_version[1]) {
            if (i2 < data->best_version[2]) return false;
            else if (i2 == data->best_version[2]) {
                if (i3 < data->best_version[3]) return false;
            }
        }
    }

    if (!data->best_name[0]) {
        data->best_version[0] = i0;
        data->best_version[1] = i1;
        data->best_version[2] = i2;
        data->best_version[3] = i3;
    }
    return true;
}

static bool win8_best(wchar_t *short_name, Version_Data *data) {
    // Find the Windows 8 subdirectory with the highest version number.
    int i0, i1;
    int success = swscanf_s(short_name, L"winv%d.%d", &i0, &i1);
    if (success < 2) return false;

    if (i0 < data->best_version[0]) return false;
    else if (i0 == data->best_version[0]) {
        if (i1 < data->best_version[1]) return false;
    }

    if (!data->best_name[0]) {
        data->best_version[0] = i0;
        data->best_version[1] = i1;
    }
    return true;
}

static bool find_windows_kit_root(Cuik_WindowsToolchain* result) {
    // Information about the Windows 10 and Windows 8 development kits
    // is stored in the same place in the registry. We open a key
    // to that place, first checking preferntially for a Windows 10 kit,
    // then, if that's not found, a Windows 8 kit.
    HKEY main_key;
    LSTATUS rc = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots",
        0, KEY_QUERY_VALUE | KEY_ENUMERATE_SUB_KEYS, &main_key);
    if (rc != S_OK) return false;

    // Look for a Windows 10 entry.
    wchar_t *windows10_root = find_windows_kit_root_with_key(main_key, L"KitsRoot10");
    if (windows10_root) {
        wchar_t *windows10_lib = concat2(windows10_root, L"Lib");

        Version_Data data = {.best_name = result->windows_sdk_root};
        visit_files_w(windows10_lib, &data, win10_best);
        cuik_free(windows10_lib);

        if (data.best_name[0]) {
            result->windows_sdk_version = 10;

            swprintf_s(result->windows_sdk_include, MAX_PATH,
                L"%sInclude\\%d.%d.%d.%d",
                windows10_root,
                data.best_version[0], data.best_version[1],
                data.best_version[2], data.best_version[3]);

            cuik_free(windows10_root);
            RegCloseKey(main_key);
            return true;
        }

        cuik_free(windows10_lib);
    }

    // Look for a Windows 8 entry.
    wchar_t *windows8_root = find_windows_kit_root_with_key(main_key, L"KitsRoot81");
    if (windows8_root) {
        wchar_t *windows8_lib = concat2(windows8_root, L"Lib");

        Version_Data data = {.best_name = result->windows_sdk_root};
        visit_files_w(windows8_lib, &data, win8_best);
        cuik_free(windows8_lib);

        if (data.best_name[0]) {
            result->windows_sdk_version = 8;

            swprintf_s(result->windows_sdk_include, MAX_PATH,
                L"%sInclude\\%d.%d.%d.%d",
                windows10_root,
                data.best_version[0], data.best_version[1],
                data.best_version[2], data.best_version[3]);

            cuik_free(windows10_root);
            cuik_free(windows8_root);
            RegCloseKey(main_key);
            return true;
        }
    }

    // If we get here, we failed to find anything.
    cuik_free(windows10_root);
    cuik_free(windows8_root);
    RegCloseKey(main_key);
    return false;
}

static bool find_visual_studio_2017_by_fighting_through_microsoft_craziness(Cuik_WindowsToolchain *result) {
    /* HRESULT rc = */ CoInitialize(NULL);
    // "Subsequent valid calls return false." So ignore false.
    // if rc != S_OK  return false;

    GUID my_uid                   = {0x42843719, 0xDB4C, 0x46C2, {0x8E, 0x7C, 0x64, 0xF1, 0x81, 0x6E, 0xFD, 0x5B}};
    GUID CLSID_SetupConfiguration = {0x177F0C4A, 0x1CD3, 0x4DE7, {0xA3, 0x2C, 0x71, 0xDB, 0xBB, 0x9F, 0xA3, 0x6D}};

    ISetupConfiguration *config = NULL;

    // NOTE(Kalinovcic): This is so stupid... These functions take references, so the code is different for C and C++......
    #ifdef __cplusplus
    HRESULT hr = CoCreateInstance(CLSID_SetupConfiguration, NULL, CLSCTX_INPROC_SERVER, my_uid, (void **)&config);
    #else
    HRESULT hr = CoCreateInstance(&CLSID_SetupConfiguration, NULL, CLSCTX_INPROC_SERVER, &my_uid, (void **)&config);
    #endif

    if (hr != 0)  return false;

    IEnumSetupInstances *instances = NULL;
    hr = CALL_STDMETHOD(config, EnumInstances, &instances);
    CALL_STDMETHOD_(config, Release);
    if (hr != 0)     return false;
    if (!instances)  return false;

    bool found_visual_studio_2017 = false;
    while (1) {
        ULONG found = 0;
        ISetupInstance *instance = NULL;
        HRESULT hr = CALL_STDMETHOD(instances, Next, 1, &instance, &found);
        if (hr != S_OK) break;

        BSTR bstr_inst_path;
        hr = CALL_STDMETHOD(instance, GetInstallationPath, &bstr_inst_path);
        CALL_STDMETHOD_(instance, Release);
        if (hr != S_OK)  continue;

        wchar_t *tools_filename = concat2(bstr_inst_path, L"\\VC\\Auxiliary\\Build\\Microsoft.VCToolsVersion.default.txt");
        SysFreeString(bstr_inst_path);

        FILE *f;
        errno_t open_result = _wfopen_s(&f, tools_filename, L"rt");
        cuik_free(tools_filename);
        if (open_result != 0) continue;
        if (!f) continue;

        LARGE_INTEGER tools_file_size;
        HANDLE file_handle = (HANDLE)_get_osfhandle(_fileno(f));
        BOOL success = GetFileSizeEx(file_handle, &tools_file_size);
        if (!success) {
            fclose(f);
            continue;
        }

        // Warning: This multiplication by 2 presumes there is no variable-length encoding in the wchars (wacky characters in the file could betray this expectation).
        uint64_t version_bytes = (tools_file_size.QuadPart + 1) * 2;
        wchar_t *version = (wchar_t *)cuik_malloc(version_bytes);

        wchar_t *read_result = fgetws(version, version_bytes, f);
        fclose(f);
        if (!read_result) continue;

        wchar_t *version_tail = wcschr(version, '\n');
        if (version_tail) *version_tail = 0;  // Stomp the data, because nobody cares about it.

        wchar_t *include_path = concat4(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\include");
        wchar_t *library_path = concat4(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\lib\\x64");
        wchar_t *library_file = concat2(library_path, L"\\vcruntime.lib");  // @Speed: Could have library_path point to this string, with a smaller count, to save on memory flailing!

        if (os_file_exists(library_file)) {
            swprintf(result->vc_tools_install, MAX_PATH, L"%s\\VC\\Tools\\MSVC\\%s\\", bstr_inst_path, version);
            cuik_free(version);

            found_visual_studio_2017 = true;
            break;
        }

        cuik_free(version);

        /*
           Ryan Saunderson said:
           "Clang uses the 'SetupInstance->GetInstallationVersion' / ISetupHelper->ParseVersion to find the newest version
           and then reads the tools file to define the tools path - which is definitely better than what i did."
           So... @Incomplete: Should probably pick the newest version...
        */
    }

    CALL_STDMETHOD_(instances, Release);
    return found_visual_studio_2017;
}

static bool find_visual_studio_by_fighting_through_microsoft_craziness(Cuik_WindowsToolchain* result) {
    // The name of this procedure is kind of cryptic. Its purpose is
    // to fight through Microsoft craziness. The things that the fine
    // Visual Studio team want you to do, JUST TO FIND A SINGLE FOLDER
    // THAT EVERYONE NEEDS TO FIND, are ridiculous garbage.

    // For earlier versions of Visual Studio, you'd find this information in the registry,
    // similarly to the Windows Kits above. But no, now it's the future, so to ask the
    // question "Where is the Visual Studio folder?" you have to do a bunch of COM object
    // instantiation, enumeration, and querying. (For extra bonus points, try doing this in
    // a new, underdeveloped programming language where you don't have COM routines up
    // and running yet. So fun.)
    //
    // If all this COM object instantiation, enumeration, and querying doesn't give us
    // a useful result, we drop back to the registry-checking method.

    bool found_visual_studio_2017 = find_visual_studio_2017_by_fighting_through_microsoft_craziness(result);
    if (found_visual_studio_2017) return true;


    // If we get here, we didn't find Visual Studio 2017. Try earlier versions.

    HKEY vs7_key;
    HRESULT rc = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\VisualStudio\\SxS\\VS7", 0, KEY_QUERY_VALUE, &vs7_key);
    if (rc != S_OK) return false;

    // Hardcoded search for 4 prior Visual Studio versions. Is there something better to do here?
    wchar_t *versions[] = { L"14.0", L"12.0", L"11.0", L"10.0" };
    const int NUM_VERSIONS = sizeof(versions) / sizeof(versions[0]);

    for (int i = 0; i < NUM_VERSIONS; i++) {
        wchar_t *v = versions[i];

        DWORD dw_type, cb_data;
        LSTATUS rc = RegQueryValueExW(vs7_key, v, NULL, &dw_type, NULL, &cb_data);
        if ((rc == ERROR_FILE_NOT_FOUND) || (dw_type != REG_SZ)) {
            continue;
        }

        wchar_t *buffer = (wchar_t *)cuik_malloc(cb_data);
        if (!buffer) return false;

        rc = RegQueryValueExW(vs7_key, v, NULL, NULL, (LPBYTE)buffer, &cb_data);
        if (rc != 0) continue;

        // @Robustness: Do the zero-termination thing suggested in the RegQueryValue docs?
        swprintf(result->vs_library_path, MAX_PATH, L"%sVC\\Lib\\amd64\\", buffer);
        swprintf(result->vs_include_path, MAX_PATH, L"%sVC\\Include\\", buffer);

        // Check to see whether a vcruntime.lib actually exists here.
        wchar_t *vcruntime_filename = concat2(result->vs_library_path, L"\\vcruntime.lib");
        bool vcruntime_exists = os_file_exists(vcruntime_filename);
        cuik_free(vcruntime_filename);

        if (vcruntime_exists) {
            swprintf(result->vs_exe_path, MAX_PATH, L"%SVC\\bin\\amd64\\", buffer);

            cuik_free(buffer);
            RegCloseKey(vs7_key);
            return true;
        }

        cuik_free(buffer);
    }

    RegCloseKey(vs7_key);

    // If we get here, we failed to find anything.
    return false;
}
#else
// during non-windows builds of Cuik we can't actually find the MSVC
// stuff the normal way
static bool find_windows_kit_root(Cuik_WindowsToolchain* result) {
    return false;
}

static bool find_visual_studio_by_fighting_through_microsoft_craziness(Cuik_WindowsToolchain* result) {
    return false;
}
#endif /* _WIN32 */

static void add_libraries(void* ctx, bool nocrt, Cuik_Linker* l) {
    Cuik_WindowsToolchain* t = ctx;
    if (t != NULL) {
        cuiklink_add_libpathf(l, STR_FMT, t->vs_library_path);
        cuiklink_add_libpathf(l, STR_FMT SLASH"um"SLASH"x64", t->windows_sdk_root);
        cuiklink_add_libpathf(l, STR_FMT SLASH"ucrt"SLASH"x64", t->windows_sdk_root);
    }

    if (!nocrt) {
        #if 1
        cuiklink_add_input_file(l, "kernel32.lib");
        cuiklink_add_input_file(l, "libcmt.lib");
        cuiklink_add_input_file(l, "oldnames.lib");
        #else
        cuiklink_add_input_file(l, "kernel32.lib");
        cuiklink_add_input_file(l, "ucrt.lib");
        cuiklink_add_input_file(l, "msvcrt.lib");
        cuiklink_add_input_file(l, "vcruntime.lib");
        #endif
    }
}

static void print_verbose(void* ctx, bool nocrt) {
    Cuik_WindowsToolchain* t = ctx;

    printf("Includes:\n");
    printf("  " STR_FMT SLASH"um\n",     t->windows_sdk_include);
    printf("  " STR_FMT SLASH"shared\n", t->windows_sdk_include);
    printf("  " STR_FMT "\n",            t->vs_include_path);
    if (!nocrt) {
        printf("  " STR_FMT SLASH"ucrt\n", t->windows_sdk_include);
    }
    printf("\n");
}

static void set_preprocessor(void* ctx, bool nocrt, Cuik_CPP* cpp) {
    Cuik_WindowsToolchain* t = ctx;
    if (t != NULL) {
        cuikpp_add_include_directoryf(cpp, true, STR_FMT SLASH"um",     t->windows_sdk_include);
        cuikpp_add_include_directoryf(cpp, true, STR_FMT SLASH"shared", t->windows_sdk_include);
        cuikpp_add_include_directoryf(cpp, true, STR_FMT,               t->vs_include_path);
        if (!nocrt) {
            cuikpp_add_include_directoryf(cpp, true, STR_FMT SLASH"ucrt", t->windows_sdk_include);
        }
    }

    cuikpp_define_empty_cstr(cpp, "_MT");
    if (true) {
        cuikpp_define_empty_cstr(cpp, "_DLL");
    }

    // we support MSVC extensions
    cuikpp_define_cstr(cpp, "_MSC_EXTENSIONS", "1");
    cuikpp_define_cstr(cpp, "_INTEGRAL_MAX_BITS", "64");

    cuikpp_define_cstr(cpp, "_USE_ATTRIBUTES_FOR_SAL", "0");

    // pretend to be MSVC
    {
        cuikpp_define_cstr(cpp, "_MSC_BUILD", "1");
        cuikpp_define_cstr(cpp, "_MSC_FULL_VER", "192930137");
        cuikpp_define_cstr(cpp, "_MSC_VER", "1929");
    }

    // wrappers over MSVC based keywords and features
    cuikpp_define_cstr(cpp, "__int8", "char");
    cuikpp_define_cstr(cpp, "__int16", "short");
    cuikpp_define_cstr(cpp, "__int32", "int");
    cuikpp_define_cstr(cpp, "__int64", "long long");
    cuikpp_define_cstr(cpp, "__pragma(x)", "_Pragma(#x)");
    cuikpp_define_cstr(cpp, "__inline", "inline");
    cuikpp_define_cstr(cpp, "__forceinline", "inline");
    cuikpp_define_cstr(cpp, "__signed__", "signed");
    cuikpp_define_cstr(cpp, "__restrict__", "restrict");
    cuikpp_define_cstr(cpp, "__alignof", "_Alignof");
    cuikpp_define_cstr(cpp, "__CRTDECL", "__cdecl");
    cuikpp_define_cstr(cpp, "_CRT_DECLARE_NONSTDC_NAMES", "1");

    // things we don't handle yet so we just remove them
    cuikpp_define_empty_cstr(cpp, "_Frees_ptr_");
    cuikpp_define_empty_cstr(cpp, "_Check_return_");
    cuikpp_define_empty_cstr(cpp, "__unaligned");
    cuikpp_define_empty_cstr(cpp, "__analysis_noreturn");
    cuikpp_define_empty_cstr(cpp, "__ptr32");
    cuikpp_define_empty_cstr(cpp, "__ptr64");
}

static bool invoke_link(void* ctx, const Cuik_DriverArgs* args, Cuik_Linker* linker, const char* output, const char* filename) {
    enum { CMD_LINE_MAX = 4096 };
    Cuik_WindowsToolchain* t = ctx;

    static const char* subsystem_option[] = {
        [TB_WIN_SUBSYSTEM_UNKNOWN] = "",
        [TB_WIN_SUBSYSTEM_WINDOWS] = "/subsystem:windows ",
        [TB_WIN_SUBSYSTEM_CONSOLE] = "/subsystem:console ",
        [TB_WIN_SUBSYSTEM_EFI_APP] = "/subsystem:efi_application",
    };

    char cmd_line[CMD_LINE_MAX];
    int cmd_line_len = snprintf(cmd_line, CMD_LINE_MAX,
        "cmd /c \"\"" STR_FMT "bin\\Hostx64\\x64\\link.exe\" /nologo /machine:amd64 %s"
        "/debug:%s /pdb:%s.pdb /out:%s /incremental:no ",
        t->vc_tools_install, subsystem_option[args->subsystem],
        args->debug_info ? "full" : "none", output, output
    );

    dyn_array_for(i, linker->libpaths) {
        cmd_line_len += snprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, "/libpath:\"%s\" ", linker->libpaths[i]);
    }

    dyn_array_for(i, linker->inputs) {
        cmd_line_len += snprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, "%s ", linker->inputs[i]);
    }

    cmd_line[cmd_line_len++] = '"';
    cmd_line[cmd_line_len++] = 0;

    if (args->verbose) {
        printf("Linker command:\n%s\n\n", cmd_line);
    }

    int exit_code = system(cmd_line);
    if (exit_code != 0) {
        fprintf(stderr, "Linker exited with code %d\n", exit_code);
        return false;
    }

    return true;
}

// tries to walk about `steps` slashes in the filepath and return the pointer to said
// slash, if it can't reach then it'll return NULL
static const OSChar* step_out_dir(const OSChar* path, int steps) {
    int slashes_hit = 0;
    const OSChar* end = path;
    while (*end) end++;

    while (slashes_hit != steps && end-- != path) {
        if (*end == '/') slashes_hit++;
        else if (*end == '\\') slashes_hit++;
    }

    return (slashes_hit == steps) ? end : NULL;
}

static void* init(void) {
    Cuik_WindowsToolchain* result = cuik_malloc(sizeof(Cuik_WindowsToolchain));
    result->vc_tools_install[0] = 0;

    // when installing from the mmozeiko's portable MSVC script, the registry doesn't
    // get setup so we search for the windows kit via environment variables.
    const OSChar* sdk_dir = env_get("WindowsSDKDir");
    const OSChar* version = env_get("WindowsSDKVersion");

    if (sdk_dir != NULL && version != NULL) {
        result->windows_sdk_version = 10;

        str_printf(result->windows_sdk_include, FILENAME_MAX, "%s"SLASH"Include"SLASH"%s", sdk_dir, version);
        str_printf(result->windows_sdk_root,    FILENAME_MAX, "%s"SLASH"Lib"SLASH"%s", sdk_dir, version);
    } else {
        if (!find_windows_kit_root(result)) {
            fprintf(stderr,
                "warning: could not locate windows SDK!\n"
                "  you can provide WindowsSDKDir, WindowsSDKVersion and\n"
                "  VCToolsInstallDir via the environment.\n"
            );
            return NULL;
        }
    }

    const OSChar* vc_tools_install = env_get("VCToolsInstallDir");
    if (vc_tools_install != NULL) {
        str_copy(result->vc_tools_install, vc_tools_install, FILENAME_MAX);
        str_printf(result->vs_include_path, FILENAME_MAX, "%s"SLASH"include"SLASH, vc_tools_install);
        str_printf(result->vs_library_path, FILENAME_MAX, "%s"SLASH"lib"SLASH"x64"SLASH, vc_tools_install);
        str_printf(result->vs_exe_path, FILENAME_MAX, "%sVC"SLASH"bin"SLASH"amd64"SLASH, vc_tools_install);
    } else {
        if (!find_visual_studio_by_fighting_through_microsoft_craziness(result)) {
            fprintf(stderr,
                "warning: could not locate VC tools!\n"
                "  you can provide SDK_INCLUDE, SDK_LIBS and\n"
                "  VCToolsInstallDir via the environment.\n"
            );
            return NULL;
        }
    }

    return result;
}

Cuik_Toolchain cuik_toolchain_msvc(void) {
    return (Cuik_Toolchain){
        .init = init,
        .case_insensitive = true,
        .set_preprocessor = set_preprocessor,
        .add_libraries = add_libraries,
        .invoke_link = invoke_link,
        .print_verbose = print_verbose,
    };
}
