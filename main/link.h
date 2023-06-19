
static DynArray(const char*) libpaths = NULL;
static const char* output_name = "a.exe";
static DynArray(const char*) input_files = NULL;
static int errors = 0;

static void add_linker_input(TB_Linker* l, const char* path) {
    size_t path_len = strlen(path);

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
            return;
        }

        tb_linker_append_library(l, path_slice, (TB_Slice){ fm.size, fm.data });
    } else {
        // normal object files
        path_slice = (TB_Slice){ strlen(path), (const uint8_t*) path };
        fm = open_file_map(path);

        if (fm.data == NULL) {
            fprintf(stderr, "could not find object file: %s\n", path);
            errors++;
            return;
        }

        TB_Slice data = { fm.size, fm.data };
        tb_linker_append_object(l, path_slice, data);
    }
}

// pretends to act like link.exe
int run_link(int argc, const char** argv) {
    dyn_array_put(libpaths, "./");

    // find toolchain details from Cuik
    {
        Cuik_Linker l = { .toolchain = cuik_toolchain_host() };
        l.toolchain.ctx = l.toolchain.init();

        cuiklink_apply_toolchain_libs(&l, false);

        dyn_array_for(i, l.libpaths) {
            dyn_array_put(libpaths, l.libpaths[i]);
        }
    }

    for (int i = 0; i < argc; i++) {
        const char* arg = argv[i];

        if (arg[0] == '-' || arg[0] == '/') {
            arg += 1;

            if (strncmp(arg, "libpath:", 8) == 0) {
                dyn_array_put(libpaths, arg + 8);
            } else {
                fprintf(stderr, "\x1b[31merror\x1b[0m: unresolved option: -%s\n", arg);
            }
        } else {
            dyn_array_put(input_files, arg);
        }
    }

    TB_Linker* l = tb_linker_create(TB_EXECUTABLE_PE, TB_ARCH_X86_64);

    dyn_array_for(i, input_files) {
        add_linker_input(l, input_files[i]);
    }

    TB_LinkerMsg m;
    while (tb_linker_get_msg(l, &m)) {
        if (m.tag == TB_LINKER_MSG_IMPORT) {
            char path[FILENAME_MAX];
            assert(m.import_path.length < FILENAME_MAX);
            memcpy(path, m.import_path.data, m.import_path.length);
            path[m.import_path.length] = 0;

            add_linker_input(l, path);
        }
    }

    if (errors) {
        fprintf(stderr, "library search paths:\n");
        for (size_t j = 0; j < dyn_array_length(libpaths); j++) {
            fprintf(stderr, "  %s\n", libpaths[j]);
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
