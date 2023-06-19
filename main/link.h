
static DynArray(const char*) libpaths = NULL;

// pretends to act like link.exe
int run_link(int argc, const char** argv) {
    const char* output_name = "a.exe";
    DynArray(const char*) input_files = NULL;

    for (int i = 0; i < argc; i++) {
        const char* arg = argv[i];

        if (arg[0] != '-') {
            dyn_array_put(input_files, arg);
        } else if (strncmp(arg, "/libpath:", 9) == 0) {
            i += 1;
            dyn_array_put(libpaths, argv[i]);
        } else {
            fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved warning: %s\n", arg);
        }
    }

    TB_Linker* l = tb_linker_create(TB_EXECUTABLE_PE, TB_ARCH_X86_64);

    int errors = 0;
    dyn_array_for(i, input_files) {
        size_t path_len = strlen(input_files[i]);
        const char* path = input_files[i];

        FileMap fm = { 0 };
        TB_Slice path_slice;
        if (path_len >= 4 && strcmp(path + path_len - 4, ".lib") == 0) {
            // try for libpaths
            char test_path[FILENAME_MAX];
            dyn_array_for(j, libpaths) {
                snprintf(test_path, FILENAME_MAX, "%s/%s", libpaths[j], path);
                fm = open_file_map(test_path);
                if (fm.data != NULL) {
                    path_slice = (TB_Slice){ strlen(test_path), (const uint8_t*) cuik_strdup(test_path) };
                    break;
                }
            }

            if (fm.data == NULL) {
                fprintf(stderr, "could not find library: %s\n", path);
                errors++;
                continue;
            }

            tb_linker_append_library(l, path_slice, (TB_Slice){ fm.size, fm.data });
        } else {
            // normal object files
            path_slice = (TB_Slice){ strlen(path), (const uint8_t*) path };
            fm = open_file_map(path);

            if (fm.data == NULL) {
                fprintf(stderr, "could not find object file: %s\n", path);
                errors++;
                continue;
            }

            TB_Slice data = { fm.size, fm.data };
            tb_linker_append_object(l, path_slice, data);
        }
    }

    if (errors) {
        fprintf(stderr, "library search paths:\n");
        dyn_array_for(i, libpaths) {
            fprintf(stderr, "  %s\n", libpaths[i]);
        }
        return EXIT_FAILURE;
    }

    TB_Exports exports = tb_linker_export(l);
    if (exports.count == 0) {
        fprintf(stderr, "\x1b[31merror\x1b[0m: could not link executable\n");
        return false;
    }

    FILE* file = fopen(output_name, "wb");
    if (file == NULL) {
        fprintf(stderr, "could not open file for writing! %s", output_name);
        return EXIT_FAILURE;
    }

    CUIK_TIMED_BLOCK("fwrite") {
        fwrite(exports.files[0].data, 1, exports.files[0].length, file);
    }

    fclose(file);
    tb_exporter_free(exports);
    tb_linker_destroy(l);
    return EXIT_SUCCESS;
}
