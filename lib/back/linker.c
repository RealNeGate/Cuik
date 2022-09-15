#include <cuik.h>
#include "../common.h"
#include "../timer.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "microsoft_craziness.h"
#define SLASH "\\"

extern MicrosoftCraziness_Find_Result cuik__vswhere;
#else
#define SLASH "/"
#endif

#define CMD_LINE_MAX 4096
#define LINKER_STRING_BUFFER_CAP 8192

bool cuiklink_init(Cuik_Linker* l) {
    // NOTE(NeGate): Windows has a max command line length of 32768 iirc,
    // so this seems reasonable
    *l = (Cuik_Linker){
        .input_file_buffer = malloc(LINKER_STRING_BUFFER_CAP),
        .libpaths_buffer = malloc(LINKER_STRING_BUFFER_CAP),
    };

    return true;
}

void cuiklink_deinit(Cuik_Linker* l) {
    free(l->input_file_buffer);
    free(l->libpaths_buffer);
}

void cuiklink_add_default_libpaths(Cuik_Linker* l) {
    #ifdef _WIN32
    if (cuik__vswhere.vs_exe_path == NULL) {
        printf("internal compiler error: Could not locate VS and Windows SDK to link with. You'll need Visual Studio to link with cuik.\n");
        abort();
    }

    cuiklink_add_libpath_wide(l, cuik__vswhere.vs_library_path);
    //cuiklink_add_libpath_wide(l, libs->vswhere.windows_sdk_ucrt_library_path);
    cuiklink_add_libpath_wide(l, cuik__vswhere.windows_sdk_um_library_path);
    #endif
}

#ifdef _WIN32
void cuiklink_add_libpath_wide(Cuik_Linker* l, const wchar_t* filepath) {
    assert(filepath);

    size_t filepath_len = wcslen(filepath) + 1;
    if (l->libpaths_top + filepath_len >= LINKER_STRING_BUFFER_CAP) abort();

    memcpy(&l->libpaths_buffer[l->libpaths_top], filepath, filepath_len * sizeof(wchar_t));
    l->libpaths_top += filepath_len;
    l->libpaths_count++;
}
#endif

void cuiklink_add_libpath(Cuik_Linker* l, const char filepath[]) {
    #ifdef _WIN32
    size_t remaining = LINKER_STRING_BUFFER_CAP - l->libpaths_top;
    OS_String output = &l->libpaths_buffer[l->libpaths_top];

    int number_of_wide_chars = MultiByteToWideChar(65001 /* UTF8 */, 0, filepath, -1, output, remaining);
    l->libpaths_top += number_of_wide_chars;
    l->libpaths_count++;
    #else
    size_t filepath_len = strlen(filepath) + 1;
    if (l->libpaths_top + filepath_len >= LINKER_STRING_BUFFER_CAP) abort();

    memcpy(&l->libpaths_buffer[l->libpaths_top], filepath, filepath_len);
    l->libpaths_top += filepath_len;
    l->libpaths_count++;
    #endif
}

void cuiklink_add_input_file(Cuik_Linker* l, const char filepath[]) {
    #ifdef _WIN32
    size_t remaining = LINKER_STRING_BUFFER_CAP - l->input_file_top;
    OS_String output = &l->input_file_buffer[l->input_file_top];

    int number_of_wide_chars = MultiByteToWideChar(65001 /* UTF8 */, 0, filepath, -1, output, remaining);
    l->input_file_top += number_of_wide_chars;
    l->input_file_count++;
    #else
    size_t filepath_len = strlen(filepath) + 1;
    if (l->input_file_top + filepath_len >= LINKER_STRING_BUFFER_CAP) abort();

    memcpy(&l->input_file_buffer[l->input_file_top], filepath, filepath_len);
    l->input_file_top += filepath_len;
    l->input_file_count++;
    #endif
}

void cuiklink_subsystem_windows(Cuik_Linker* l) {
    l->subsystem_windows = true;
}

bool cuiklink_invoke(Cuik_Linker* l, const char* filename, const char* crt_name) {
    #if defined(_WIN32)
    wchar_t cmd_line[CMD_LINE_MAX];
    int cmd_line_len = swprintf(cmd_line, CMD_LINE_MAX,
        L"%s\\link.exe /nologo /machine:amd64 /subsystem:%s "
        "/debug:full /pdb:%S.pdb /out:%S.exe /incremental:no ",
        cuik__vswhere.vs_exe_path, l->subsystem_windows ? L"windows" : L"console", filename, filename);

    // Add all the libpaths
    OS_String str = l->libpaths_buffer;
    for (size_t i = l->libpaths_count; i--;) {
        cmd_line_len += swprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, L"/libpath:\"%s\" ", str);
        str += wcslen(str) + 1;
    }

    // Add all the input files
    str = l->input_file_buffer;
    for (size_t i = l->input_file_count; i--;) {
        cmd_line_len += swprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, L"%s ", str);
        str += wcslen(str) + 1;
    }

    STARTUPINFOW si = {
        .cb = sizeof(STARTUPINFOW),
        .dwFlags = STARTF_USESTDHANDLES,
        .hStdInput = GetStdHandle(STD_INPUT_HANDLE),
        .hStdError = GetStdHandle(STD_ERROR_HANDLE),
        .hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE),
    };
    PROCESS_INFORMATION pi = { 0 };

    // printf("Linker command:\n%S\n", cmd_line);
    if (!CreateProcessW(NULL, cmd_line, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
        WaitForSingleObject(pi.hProcess, INFINITE);
        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);

        fprintf(stderr, "Linker command could not be executed.\n");
        return false;
    }

    // Wait until child process exits.
    WaitForSingleObject(pi.hProcess, INFINITE);

    DWORD exit_code = 0;
    if (!GetExitCodeProcess(pi.hProcess, &exit_code)) {
        fprintf(stderr, "Failed to retrieve linker exit code.\n");
        goto error;
    }

    if (exit_code != 0) {
        fprintf(stderr, "Linker exited with code %lu\n", exit_code);
        goto error;
    }

    // Close process and thread handles.
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
    return true;

    error:
    fprintf(stderr, "Linker command:\n%S\n", cmd_line);

    if (pi.hProcess && pi.hProcess != INVALID_HANDLE_VALUE) CloseHandle(pi.hProcess);
    if (pi.hThread && pi.hThread != INVALID_HANDLE_VALUE) CloseHandle(pi.hThread);
    return false;
    #elif defined(__unix__) || defined(__APPLE__)
    char cmd_line[CMD_LINE_MAX];
    int cmd_line_len = snprintf(cmd_line, CMD_LINE_MAX, "gcc ");

    // Add all the libpaths
    OS_String str = l->libpaths_buffer;
    for (size_t i = l->libpaths_count; i--;) {
        cmd_line_len += snprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, "-L%s ", str);
        str += strlen(str) + 1;
    }

    // Add all the input files
    str = l->input_file_buffer;
    for (size_t i = l->input_file_count; i--;) {
        cmd_line_len += snprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, "%s ", str);
        str += strlen(str) + 1;
    }

    cmd_line_len += snprintf(&cmd_line[cmd_line_len], CMD_LINE_MAX - cmd_line_len, "-o %s ", filename);

    int exit_code = system(cmd_line);
    if (exit_code != 0) {
        fprintf(stderr, "Linker exited with code %d\n", exit_code);
        fprintf(stderr, "Linker command: %s\n", cmd_line);
        return false;
    }

    return true;
    #else
    #error "Implement system linker on other platforms"
    #endif
}
