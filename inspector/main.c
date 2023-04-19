#pragma comment(lib, "user32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "Winmm.lib")
#include <raylib.h>

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define NOGDI
#define NOUSER
#include <file_map.h>

#include <tb_elf.h>

static struct {
    size_t length;
    uint8_t* data;

    enum {
        ELF_LOAD_HEADER,
    } stage;
} loader;

static struct {
    Vector2 size;

    float top_left_x, top_left_y;
    float hex_stride, char_stride;
} metrics;

static struct {
    Vector2 mouse;
    const char* text;
} tooltip;

static float scroll;
static float scroll_target;

static int loader_next(void) {
    switch (loader.stage) {
        case ELF_LOAD_HEADER: {
            TB_Elf64_Ehdr* elf_header = (TB_Elf64_Ehdr*) &loader.data[0];

            return ELF_LOAD_HEADER;
        }

        default: break;
    }
}

// static const char aaa[] = "00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ";
static const char aaa[] = "000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 000 ";

static Font fontTtf;

inline static uint32_t fnv1a(size_t len, const void *key) {
    const uint8_t* data = key;
    uint32_t h = 0x811C9DC5;
    for (size_t i = 0; i < len; i++) {
        h = (data[i] ^ h) * 0x01000193;
    }

    return h;
}

static bool draw_highlight(float yy, uint32_t pos, int len, const char* tooltip_text) {
    uint32_t raw_color = (pos * 11400714819323198485ull) >> 40u;
    raw_color ^= (((uintptr_t) tooltip_text) * 11400714819323198485ull) >> 32u;

    Color color = GetColor(raw_color);
    color.a = 127;

    Rectangle r = {
        metrics.top_left_x + ((pos % 16) * metrics.hex_stride) - metrics.char_stride*0.5f,
        metrics.size.y + ((pos / 16) * metrics.size.y) - 2.0,
        len * metrics.hex_stride, metrics.size.y,
    };

    r.y += yy;
    DrawRectangle(r.x, r.y, r.width, r.height, color);

    if (tooltip_text && CheckCollisionPointRec(tooltip.mouse, r)) {
        tooltip.text = tooltip_text;

        if (IsMouseButtonPressed(0)) {
            return true;
        }
    }

    return false;
}

static float draw_bytes(float yy, const void* bytes, size_t size, const char* label, uint32_t pos) {
    uint32_t raw_color = ((pos + 1234567) * 11400714819323198485ull) >> 40u;
    Color color = GetColor(raw_color);
    color.a = 255;

    char pos_txt[12];
    snprintf(pos_txt, sizeof(pos_txt), "0x%08x", pos);

    Vector2 pos_size = MeasureTextEx(fontTtf, pos_txt, fontTtf.baseSize, 2.0f);
    pos_size.x += metrics.char_stride;

    DrawTextEx(fontTtf, label, (Vector2){ metrics.top_left_x, yy }, fontTtf.baseSize, 2, color);
    DrawTextEx(fontTtf, pos_txt, (Vector2){ metrics.top_left_x + metrics.size.x - pos_size.x, yy }, fontTtf.baseSize, 2, BLACK);
    yy += metrics.size.y;

    // number of characters for the digit + space
    enum { BYTE_STRIDE = 4 };

    const uint8_t* data = bytes;
    for (size_t i = 0; i < size; i += 16) {
        char buffer[sizeof(aaa) + 1];

        // convert to hex
        size_t len = i + 16;
        if (len > size) len = size;
        len = len - i;

        for (size_t j = 0; j < len; j++) {
            snprintf(buffer + (j * BYTE_STRIDE), BYTE_STRIDE+1, "%03o ", data[i+j]);
        }
        buffer[sizeof(aaa) - 1] = 0;

        DrawTextEx(fontTtf, buffer, (Vector2){ metrics.top_left_x, yy }, fontTtf.baseSize, 2, BLACK);
        yy += metrics.size.y;
    }

    return yy + metrics.size.y;
}

static void mark_region(float start_y, float end_y, const char* label) {
    Vector2 size = MeasureTextEx(fontTtf, label, fontTtf.baseSize, 2.0f);

    float text_x = metrics.top_left_x - (size.x + metrics.char_stride*3.0);
    float text_y = ((start_y + end_y) * 0.5f) - (size.y * 1.0f);
    DrawTextEx(fontTtf, label, (Vector2){ text_x, text_y }, fontTtf.baseSize, 2, BLACK);

    // draw some line bounds
    DrawLine(text_x + size.x + metrics.char_stride, text_y + size.y * 0.5f, metrics.top_left_x - metrics.char_stride*0.5f, end_y - size.y, BLACK);
    DrawLine(text_x + size.x + metrics.char_stride, text_y + size.y * 0.5f, metrics.top_left_x - metrics.char_stride*0.5f, start_y, BLACK);
}

static void draw_tooltip(const char* text, float xx, float yy) {
    Color color = (Color){ 0, 0, 0, 200 };
    Vector2 size = MeasureTextEx(fontTtf, text, fontTtf.baseSize, 2.0f);

    Rectangle r = { xx, yy - (size.y + 20.0), size.x + 20.0, size.y };

    DrawRectangle(r.x, r.y, r.width, r.height, color);
    DrawTriangle((Vector2){ r.x, yy }, (Vector2){ r.x + 20.0, yy - 20.0 }, (Vector2){ r.x, yy - 20.0 }, color);

    Vector2 p = { r.x + (r.width*0.5f) - (size.x*0.5f), r.y + 2.0 };
    DrawTextEx(fontTtf, text, p, (float)fontTtf.baseSize, 2, WHITE);
}

static float remap(float a, float b, float t, float t_max) {
    return a + ((b - a) * (t / t_max));
}

int main() {
    const int screenWidth = 1280;
    const int screenHeight = 720;
    InitWindow(screenWidth, screenHeight, "ELF viewer");
    SetTargetFPS(165);

    // NOTE: raylib supports UTF-8 encoding, following list is actually codified as UTF8 internally
    const char msg[256] = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ";
    fontTtf = LoadFontEx("inspector/consola.ttf", 18, 0, 250);

    const char* path = "tests/1th.bin";
    // const char* path = "W:/Workspace/SockOS/bin/kernel.so";
    FileMap fm = open_file_map(path);
    if (fm.data == NULL) {
        fprintf(stderr, "\x1b[31merror\x1b[0m: '%s' not found!\n", path);
        return EXIT_FAILURE;
    }
    const uint8_t* data = fm.data;

    ////////////////////////////////
    // find program bounds
    ////////////////////////////////
    uintptr_t image_base = UINTPTR_MAX;
    uintptr_t image_end = 0;

    TB_Elf64_Ehdr* header = (TB_Elf64_Ehdr*) data;
    const uint8_t* segments = data + header->phoff;
    size_t segment_size = header->phentsize;
    for (size_t i = 0; i < header->phnum; i++) {
        TB_Elf64_Phdr* segment = (TB_Elf64_Phdr*) (segments + i*segment_size);
        if (segment->type != PT_LOAD) continue;

        uintptr_t segment_top = segment->vaddr + segment->memsz;
        if (segment_top > image_end) {
            image_end = segment_top;
        }

        if (segment->vaddr < image_base) {
            image_base = segment->vaddr;
        }
    }

    uintptr_t loaded_base = image_base < 0xFFFF ? 0xA0000000 : image_base;

    while (!WindowShouldClose()) {
        int w = GetScreenWidth(), h = GetScreenHeight();
        float dt = GetFrameTime();

        BeginDrawing();
        ClearBackground(RAYWHITE);

        metrics.size = MeasureTextEx(fontTtf, aaa, fontTtf.baseSize, 2.0f);
        metrics.hex_stride = metrics.size.x / 16.0;
        metrics.char_stride = metrics.size.x / (sizeof(aaa) - 1);

        metrics.top_left_x = (w * 0.5f) - (metrics.size.x*0.5f);
        metrics.top_left_y = h * 0.15f;

        tooltip.text = NULL;
        tooltip.mouse = GetMousePosition();

        scroll_target -= GetMouseWheelMoveV().y * 100.0;
        if (scroll_target < 0.0) {
            scroll_target = 0.0;
        }

        scroll += (scroll_target - scroll) * (1.0f - expf(-10.0 * dt));

        float yy = metrics.top_left_y - scroll;
        static float program_headers_y;

        float header_start_y = yy;
        {
            // draw helpful overlaps
            draw_highlight(yy, 0x0,  4, "magic");
            draw_highlight(yy, 0x4,  1, "class");
            draw_highlight(yy, 0x5,  1, "data");
            draw_highlight(yy, 0x6,  1, "version");
            draw_highlight(yy, 0x7,  1, "os abi");
            draw_highlight(yy, 0x8,  1, "abi ver");
            draw_highlight(yy, 0x9,  7, "pad");
            draw_highlight(yy, 0x10, 2, "type");
            draw_highlight(yy, 0x12, 2, "machine");
            draw_highlight(yy, 0x14, 4, "version");
            draw_highlight(yy, 0x18, 8, "entry");
            if (draw_highlight(yy, 0x20, 8, "program headers")) {
                scroll_target = program_headers_y;
            }

            draw_highlight(yy, 0x28, 8, "section headers");
            draw_highlight(yy, 0x30, 4, "flags");
            draw_highlight(yy, 0x34, 2, "header size");
            draw_highlight(yy, 0x36, 2, "program header size");
            draw_highlight(yy, 0x38, 2, "program header count");
            draw_highlight(yy, 0x3A, 2, "section header size");
            draw_highlight(yy, 0x3C, 2, "section header count");
            draw_highlight(yy, 0x3E, 2, "string table index");

            yy = draw_bytes(yy, header, header->ehsize, "ELF header", 0);
        }
        mark_region(header_start_y, yy, "file header");

        program_headers_y = yy;
        for (int i = 0; i < header->phnum; i++) {
            size_t offset = header->phoff + i*header->phentsize;
            TB_Elf64_Phdr* phdr = (TB_Elf64_Phdr*) &data[offset];

            yy = draw_bytes(yy, phdr, header->phentsize, "program header", offset);
        }
        mark_region(program_headers_y, yy, "program headers");

        for (int i = 0; i < header->phnum; i++) {
            TB_Elf64_Phdr* phdr = (TB_Elf64_Phdr*) &data[header->phoff + i*header->phentsize];
            yy = draw_bytes(yy, data + phdr->offset, phdr->filesz, "segment data", phdr->offset);
        }

        if (tooltip.text != NULL) {
            draw_tooltip(tooltip.text, tooltip.mouse.x, tooltip.mouse.y);
        }

        // address space view
        {
            Rectangle r = { 0, h * 0.6f, w, h * 0.4f };
            DrawRectangle(r.x, r.y, r.width, r.height, (Color){ 63, 63, 63, 255 });

            DrawTextEx(fontTtf, "Memory", (Vector2){ 8.0f, (h * 0.6f) + metrics.size.y*0.5f }, (float)fontTtf.baseSize, 2, WHITE);

            // timeline is scaled such that 80% of it is in the ELF's used space
            float timeline_y = r.y + r.height*0.25f - 2.0;

            float addr_space_w = w * 0.8f;
            float xx = w * 0.1f;

            DrawRectangle(xx,                     timeline_y, addr_space_w * 0.1f, 4.0, WHITE);
            DrawRectangle(xx + addr_space_w*0.1f, timeline_y, addr_space_w * 0.8f, 4.0, RED);
            DrawRectangle(xx + addr_space_w*0.9f, timeline_y, addr_space_w * 0.1f, 4.0, WHITE);

            // Draw loaded regions
            uintptr_t image_size = image_end - image_base;
            for (int i = 0; i < header->phnum; i++) {
                size_t offset = header->phoff + i*header->phentsize;
                TB_Elf64_Phdr* phdr = (TB_Elf64_Phdr*) &data[offset];

                uint32_t raw_color = ((offset + 1234567) * 11400714819323198485ull) >> 40u;
                Color color = GetColor(raw_color);
                color.a = 255;

                float start_x  = xx + addr_space_w*0.1f;
                float start_x2 = xx + addr_space_w*0.9f;

                float region_x  = remap(start_x, start_x2, phdr->vaddr - image_base, image_size);
                float region_x2 = remap(start_x, start_x2, (phdr->vaddr + phdr->filesz) - image_base, image_size);
                DrawRectangle(region_x, timeline_y, region_x2 - region_x, 4.0, color);
            }

            // hover over timeline
            Rectangle timeline = { xx, timeline_y - 14.0, addr_space_w, 32.0 };
            if (CheckCollisionPointRec(tooltip.mouse, timeline)) {
                Vector2 p = tooltip.mouse;
                p.y = timeline_y + 4.0;

                float factor = (tooltip.mouse.x - (xx + addr_space_w*0.1f)) / (addr_space_w * 0.8);
                uint64_t addr = 0;
                if (factor > 1.0) {
                    addr = 0xFFFFFFFF;
                } else if (factor < 0.0) {
                    addr = 0;
                } else {
                    addr = factor * image_size;
                    addr += loaded_base;
                }

                char addr_str[20];
                snprintf(addr_str, sizeof(addr_str), "0x%016llx", addr);
                // snprintf(addr_str, sizeof(addr_str), "%f", factor);
                draw_tooltip(addr_str, p.x, p.y);
            }
        }

        EndDrawing();
    }

    UnloadFont(fontTtf);
    CloseWindow();
    return 0;
}