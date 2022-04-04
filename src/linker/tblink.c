#include <back/linker.h>
#include <back/tb.h>

#include <stdio.h>
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat  _fstat
#define stat   _stat
#endif

static TB_Slice read_file_into_slice(FILE* file) {
	if (!file) {
		printf("Internal error: could not read file\n");
		abort();
	}

	int descriptor = fileno(file);
	struct stat file_stats;
	if (fstat(descriptor, &file_stats) == -1) {
		fclose(file);
		abort();
	}

	fseek(file, 0, SEEK_SET);
	uint8_t* buffer = malloc(file_stats.st_size);
	size_t length_read = fread(buffer, 1, file_stats.st_size, file);
	
	fclose(file);
	return (TB_Slice){ length_read, buffer };
}

static FILE* locate_file(Linker* l, OS_String path) {
#ifdef _WIN32
	FILE* file = _wfopen(path, L"rb");
	if (file) return file;

	wchar_t temp_str[MAX_PATH];
	size_t i = l->libpaths_count;
	OS_String str = l->libpaths_buffer;
	while (i--) {
		swprintf(temp_str, MAX_PATH, L"%s\\%s", str, path);

		file = _wfopen(temp_str, L"rb");
		if (file) {
			return file;
		}
		str += wcslen(str) + 1;
	}

	return NULL;
#else
#error "Implement locate_file for non-windows platforms"
#endif
}

bool linker_invoke_tb(Linker* l, const char* filename) {
	OS_String str = l->input_file_buffer;
	for (size_t i = 0; i < l->input_file_count; i++) {
		TB_Slice buffer = read_file_into_slice(locate_file(l, str)); 
		TB_ObjectFile* obj = tb_object_parse_coff(buffer);
	
		printf("\nSUMMARY FOR %S\n", str);
		for (size_t j = 0; j < obj->section_count; j++) {
			printf("%-20.*s    %zu\n", 
					(int) obj->sections[j].name.length,
					(char*) obj->sections[j].name.data,
					obj->sections[j].raw_data.length
					);
		}

#ifdef _WIN32
		str += wcslen(str) + 1;
#else
		str += strlen(str) + 1;
#endif
	}
	
	return false;
}

