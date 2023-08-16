#define STBI_NO_SIMD
#define STB_IMAGE_IMPLEMENTATION
#include "win32_dx/stb_image.h"
// #include <stdio.h>

static void handle_stuff() {

}

int main() {
    printf("Hello, World! %d, %d, %d, %d, %d, %d", 16, 32, 64, 128, 256, 512);
    putchar('\n');

    const char* path = "./test.png";
    printf("Trying to load image! %s", path);
    putchar('\n');

    int x,y;
    unsigned int* pixels = (unsigned int*) stbi_load(path, &x, &y, NULL, 4);
    printf(stbi_failure_reason());
    putchar('\n');

    printf("Results: %d, %d", x, y);
    putchar('\n');

    int count = x * y;
    for (int i = 0; i < count; i++) {
        printf("%x%c", pixels[i], i % 16 == 0 ? '\n' : ' ');
    }
    return 0;
}
