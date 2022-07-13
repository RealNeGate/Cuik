//
// Author:   Jonathan Blow
// Version:  1
// Date:     31 August, 2018
//
// This code is released under the MIT license, which you can find at
//
//          https://opensource.org/licenses/MIT
//
//
//
// See the comments for how to use this library just below the includes.
//

//
// This file was about 400 lines before we started adding these comments.
// You might think that's way too much code to do something as simple
// as finding a few library and executable paths. I agree. However,
// Microsoft's own solution to this problem, called "vswhere", is a
// mere EIGHT THOUSAND LINE PROGRAM, spread across 70 files,
// that they posted to github *unironically*.
//
// I am not making this up: https://github.com/Microsoft/vswhere
//
// Several people have therefore found the need to solve this problem
// themselves. We referred to some of these other solutions when
// figuring out what to do, most prominently ziglang's version,
// by Ryan Saunderson.
//
// I hate this kind of code. The fact that we have to do this at all
// is stupid, and the actual maneuvers we need to go through
// are just painful. If programming were like this all the time,
// I would quit.
//
// Because this is such an absurd waste of time, I felt it would be
// useful to package the code in an easily-reusable way, in the
// style of the stb libraries. We haven't gone as all-out as some
// of the stb libraries do (which compile in C with no includes, often).
// For this version you need C++ and the headers at the top of the file.
//
// We return the strings as Windows wide character strings. Aesthetically
// I don't like that (I think most sane programs are UTF-8 internally),
// but apparently, not all valid Windows file paths can even be converted
// correctly to UTF-8. So have fun with that. It felt safest and simplest
// to stay with wchar_t since all of this code is fully ensconced in
// Windows crazy-land.
//
// One other shortcut I took is that this is hardcoded to return the
// folders for x64 libraries. If you want x86 or arm, you can make
// slight edits to the code below, or, if enough people want this,
// I can work it in here.
//
#include "microsoft_craziness.h"

#include <windows.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <io.h>         // For _get_osfhandle


void free_resources(MicrosoftCraziness_Find_Result *result) {
    free(result->windows_sdk_root);
    free(result->windows_sdk_um_library_path);
    free(result->windows_sdk_ucrt_library_path);
    free(result->vs_exe_path);
    free(result->vs_library_path);
}


// COM objects for the ridiculous Microsoft craziness.

#undef  INTERFACE
#define INTERFACE ISetupInstance
DECLARE_INTERFACE_ (ISetupInstance, IUnknown)
{
    BEGIN_INTERFACE

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
    BEGIN_INTERFACE

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
    wchar_t *best_name;
} Version_Data;

bool os_file_exists(wchar_t *name) {
    // @Robustness: What flags do we really want to check here?

    int attrib = GetFileAttributesW(name);
    if (attrib == INVALID_FILE_ATTRIBUTES) return false;
    if (attrib & FILE_ATTRIBUTE_DIRECTORY) return false;

    return true;
}

#define concat2(a, b) concat(a, b, NULL, NULL)
#define concat3(a, b, c) concat(a, b, c, NULL)
#define concat4(a, b, c, d) concat(a, b, c, d)
wchar_t *concat(wchar_t *a, wchar_t *b, wchar_t *c, wchar_t *d) {
    // Concatenate up to 4 wide strings together. Allocated with malloc.
    // If you don't like that, use a programming language that actually
    // helps you with using custom allocators. Or just edit the code.

    int len_a = wcslen(a);
    int len_b = wcslen(b);

    int len_c = 0;
    if (c) len_c = wcslen(c);

    int len_d = 0;
    if (d) len_d = wcslen(d);

    wchar_t *result = (wchar_t *)malloc((len_a + len_b + len_c + len_d + 1) * 2);
    memcpy(result, a, len_a*2);
    memcpy(result + len_a, b, len_b*2);

    if (c) memcpy(result + len_a + len_b, c, len_c * 2);
    if (d) memcpy(result + len_a + len_b + len_c, d, len_d * 2);

    result[len_a + len_b + len_c + len_d] = 0;

    return result;
}

typedef void (*Visit_Proc_W)(wchar_t *short_name, wchar_t *full_name, Version_Data *data);
bool visit_files_w(wchar_t *dir_name, Version_Data *data, Visit_Proc_W proc) {

    // Visit everything in one folder (non-recursively). If it's a directory
    // that doesn't start with ".", call the visit proc on it. The visit proc
    // will see if the filename conforms to the expected versioning pattern.

    WIN32_FIND_DATAW find_data;

    wchar_t *wildcard_name = concat2(dir_name, L"\\*");
    HANDLE handle = FindFirstFileW(wildcard_name, &find_data);
    free(wildcard_name);

    if (handle == INVALID_HANDLE_VALUE) return false;

    while (true) {
        if ((find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) && (find_data.cFileName[0] != '.')) {
            wchar_t *full_name = concat3(dir_name, L"\\", find_data.cFileName);
            proc(find_data.cFileName, full_name, data);
            free(full_name);
        }

        BOOL success = FindNextFileW(handle, &find_data);
        if (!success) break;
    }

    FindClose(handle);

    return true;
}


wchar_t *find_windows_kit_root_with_key(HKEY key, wchar_t *version) {
    // Given a key to an already opened registry entry,
    // get the value stored under the 'version' subkey.
    // If that's not the right terminology, hey, I never do registry stuff.

    DWORD required_length;
    int rc = RegQueryValueExW(key, version, NULL, NULL, NULL, &required_length);
    if (rc != 0)  return NULL;

    DWORD length = required_length + 2;  // The +2 is for the maybe optional zero later on. Probably we are over-allocating.
    wchar_t *value = (wchar_t *)malloc(length);
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

void win10_best(wchar_t *short_name, wchar_t *full_name, Version_Data *data) {
    // Find the Windows 10 subdirectory with the highest version number.

    int i0, i1, i2, i3;
    int success = swscanf_s(short_name, L"%d.%d.%d.%d", &i0, &i1, &i2, &i3);
    if (success < 4) return;

    if (i0 < data->best_version[0]) return;
    else if (i0 == data->best_version[0]) {
        if (i1 < data->best_version[1]) return;
        else if (i1 == data->best_version[1]) {
            if (i2 < data->best_version[2]) return;
            else if (i2 == data->best_version[2]) {
                if (i3 < data->best_version[3]) return;
            }
        }
    }

    // we have to copy_string and free here because visit_files free's the full_name string
    // after we execute this function, so Win*_Data would contain an invalid pointer.
    if (data->best_name) free(data->best_name);
    data->best_name = _wcsdup(full_name);

    if (data->best_name) {
        data->best_version[0] = i0;
        data->best_version[1] = i1;
        data->best_version[2] = i2;
        data->best_version[3] = i3;
    }
}

void win8_best(wchar_t *short_name, wchar_t *full_name, Version_Data *data) {
    // Find the Windows 8 subdirectory with the highest version number.

    int i0, i1;
    int success = swscanf_s(short_name, L"winv%d.%d", &i0, &i1);
    if (success < 2) return;

    if (i0 < data->best_version[0]) return;
    else if (i0 == data->best_version[0]) {
        if (i1 < data->best_version[1]) return;
    }

    // we have to copy_string and free here because visit_files free's the full_name string
    // after we execute this function, so Win*_Data would contain an invalid pointer.
    if (data->best_name) free(data->best_name);
    data->best_name = _wcsdup(full_name);

    if (data->best_name) {
        data->best_version[0] = i0;
        data->best_version[1] = i1;
    }
}

void find_windows_kit_root(MicrosoftCraziness_Find_Result *result) {
    // Information about the Windows 10 and Windows 8 development kits
    // is stored in the same place in the registry. We open a key
    // to that place, first checking preferntially for a Windows 10 kit,
    // then, if that's not found, a Windows 8 kit.

    HKEY main_key;

    LSTATUS rc = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots",
        0, KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &main_key);
    if (rc != S_OK) return;

    // Look for a Windows 10 entry.
    wchar_t *windows10_root = find_windows_kit_root_with_key(main_key, L"KitsRoot10");

    if (windows10_root) {
        wchar_t *windows10_lib = concat2(windows10_root, L"Lib");
        free(windows10_root);

        Version_Data data = {0};
        visit_files_w(windows10_lib, &data, win10_best);
        free(windows10_lib);

        if (data.best_name) {
            result->windows_sdk_version = 10;
            result->windows_sdk_root = data.best_name;
            wchar_t* include_path = (wchar_t*)malloc(MAX_PATH);

            int success = swprintf_s(include_path, MAX_PATH,
                L"%sInclude\\%d.%d.%d.%d",
                windows10_root,
                data.best_version[0], data.best_version[1],
                data.best_version[2], data.best_version[3]);

            if (success < MAX_PATH) {
                result->windows_sdk_include = include_path;
            }

            RegCloseKey(main_key);
            return;
        }
    }

    // Look for a Windows 8 entry.
    wchar_t *windows8_root = find_windows_kit_root_with_key(main_key, L"KitsRoot81");

    if (windows8_root) {
        wchar_t *windows8_lib = concat2(windows8_root, L"Lib");
        free(windows8_root);

        Version_Data data = {0};
        visit_files_w(windows8_lib, &data, win8_best);
        free(windows8_lib);

        if (data.best_name) {
            result->windows_sdk_version = 8;
            result->windows_sdk_root = data.best_name;
            wchar_t* include_path = malloc(MAX_PATH);

            int success = swprintf_s(include_path, MAX_PATH,
                L"%sInclude\\%d.%d.%d.%d",
                windows10_root,
                data.best_version[0], data.best_version[1],
                data.best_version[2], data.best_version[3]);

            if (success < MAX_PATH) {
                result->windows_sdk_include = include_path;
            }

            RegCloseKey(main_key);
            return;
        }
    }

    // If we get here, we failed to find anything.
    RegCloseKey(main_key);
}

bool find_visual_studio_2017_by_fighting_through_microsoft_craziness(MicrosoftCraziness_Find_Result *result) {
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
        free(tools_filename);
        if (open_result != 0) continue;
        if (!f) continue;

        LARGE_INTEGER tools_file_size;
        HANDLE file_handle = (HANDLE)_get_osfhandle(_fileno(f));
        BOOL success = GetFileSizeEx(file_handle, &tools_file_size);
        if (!success) {
            fclose(f);
            continue;
        }

        uint64_t version_bytes = (tools_file_size.QuadPart + 1) * 2;  // Warning: This multiplication by 2 presumes there is no variable-length encoding in the wchars (wacky characters in the file could betray this expectation).
        wchar_t *version = (wchar_t *)malloc(version_bytes);

        wchar_t *read_result = fgetws(version, version_bytes, f);
        fclose(f);
        if (!read_result) continue;

        wchar_t *version_tail = wcschr(version, '\n');
        if (version_tail)  *version_tail = 0;  // Stomp the data, because nobody cares about it.

        wchar_t *include_path = concat4(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\include");
        wchar_t *library_path = concat4(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\lib\\x64");
        wchar_t *library_file = concat2(library_path, L"\\vcruntime.lib");  // @Speed: Could have library_path point to this string, with a smaller count, to save on memory flailing!

        if (os_file_exists(library_file)) {
            wchar_t *link_exe_path = concat4(bstr_inst_path, L"\\VC\\Tools\\MSVC\\", version, L"\\bin\\Hostx64\\x64");
            free(version);

            result->vs_exe_path     = link_exe_path;
            result->vs_include_path = include_path;
            result->vs_library_path = library_path;
            found_visual_studio_2017 = true;
            break;
        }

        free(version);

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

void find_visual_studio_by_fighting_through_microsoft_craziness(MicrosoftCraziness_Find_Result *result) {
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
    if (found_visual_studio_2017)  return;


    // If we get here, we didn't find Visual Studio 2017. Try earlier versions.

    HKEY vs7_key;
    HRESULT rc = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\VisualStudio\\SxS\\VS7", 0, KEY_QUERY_VALUE | KEY_WOW64_32KEY, &vs7_key);
    if (rc != S_OK)  return;

    // Hardcoded search for 4 prior Visual Studio versions. Is there something better to do here?
    wchar_t *versions[] = { L"14.0", L"12.0", L"11.0", L"10.0" };
    const int NUM_VERSIONS = sizeof(versions) / sizeof(versions[0]);

    for (int i = 0; i < NUM_VERSIONS; i++) {
        wchar_t *v = versions[i];

        DWORD dw_type;
        DWORD cb_data;

        LSTATUS rc = RegQueryValueExW(vs7_key, v, NULL, &dw_type, NULL, &cb_data);
        if ((rc == ERROR_FILE_NOT_FOUND) || (dw_type != REG_SZ)) {
            continue;
        }

        wchar_t *buffer = (wchar_t *)malloc(cb_data);
        if (!buffer)  return;

        rc = RegQueryValueExW(vs7_key, v, NULL, NULL, (LPBYTE)buffer, &cb_data);
        if (rc != 0)  continue;

        // @Robustness: Do the zero-termination thing suggested in the RegQueryValue docs?

        wchar_t *lib_path = concat2(buffer, L"VC\\Lib\\amd64");
        wchar_t *include_path = concat2(buffer, L"VC\\Include");

        // Check to see whether a vcruntime.lib actually exists here.
        wchar_t *vcruntime_filename = concat2(lib_path, L"\\vcruntime.lib");
        bool vcruntime_exists = os_file_exists(vcruntime_filename);
        free(vcruntime_filename);

        if (vcruntime_exists) {
            result->vs_exe_path     = concat2(buffer, L"VC\\bin\\amd64");
            result->vs_include_path = include_path;
            result->vs_library_path = lib_path;

            free(buffer);
            RegCloseKey(vs7_key);
            return;
        }

        free(lib_path);
        free(buffer);
    }

    RegCloseKey(vs7_key);

    // If we get here, we failed to find anything.
}


MicrosoftCraziness_Find_Result cuik__find_visual_studio_and_windows_sdk() {
    MicrosoftCraziness_Find_Result result = { 0 };
    find_windows_kit_root(&result);

    if (result.windows_sdk_root) {
        result.windows_sdk_um_library_path   = concat2(result.windows_sdk_root, L"\\um\\x64");
        result.windows_sdk_ucrt_library_path = concat2(result.windows_sdk_root, L"\\ucrt\\x64");
    }

    find_visual_studio_by_fighting_through_microsoft_craziness(&result);

    return result;
}
