#define _CRT_SECURE_NO_WARNINGS
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>

#ifdef _WIN32
#define fileno _fileno
#define fstat _fstat
#define stat _stat
#endif

static char* read_entire_file(const char* filepath, size_t* out_length) {
    FILE* file = fopen(filepath, "rb");
    if (file == NULL) {
        return NULL;
    }
    int descriptor = fileno(file);

    struct stat file_stats;
    if (fstat(descriptor, &file_stats) == -1) {
        return NULL;
    }

    int length = file_stats.st_size;
    char* data = malloc(length + 1);

    fseek(file, 0, SEEK_SET);
    size_t length_read = fread(data, 1, length, file);
    data[length_read] = '\0';
    fclose(file);

    *out_length = length_read;
    return data;
}

uint32_t murmur3_32(const void* key, size_t len) {
    uint32_t h = 0;

    // main body, work on 32-bit blocks at a time
    for (size_t i=0;i<len/4;i++) {
        uint32_t k = ((uint32_t*) key)[i];
        // memcpy(&k, &key[i * 4], sizeof(k));

        k *= 0xcc9e2d51;
        k = ((k << 15) | (k >> 17))*0x1b873593;
        h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    }

    // load/mix up to 3 remaining tail bytes into a tail block
    uint32_t t = 0;
    const uint8_t *tail = ((const uint8_t*) key) + 4*(len/4);
    switch(len & 3) {
        case 3: t ^= tail[2] << 16;
        case 2: t ^= tail[1] <<  8;
        case 1: {
            t ^= tail[0] <<  0;
            h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >> 17))*0x1b873593;
        }
    }

    // finalization mix, including key length
    h = ((h^len) ^ ((h^len) >> 16))*0x85ebca6b;
    h = (h ^ (h >> 13))*0xc2b2ae35;
    return (h ^ (h >> 16));
}

int main(int argc, char** argv) {
    size_t len;
    void* data = read_entire_file(argv[1], &len);

    printf("Wack! %#x\n", murmur3_32(data, len));
    return 0;
}
