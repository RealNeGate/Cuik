#pragma comment(lib, "user32.lib")
#pragma comment(lib, "gdi32.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "Winmm.lib")
#include <raylib.h>

#include <cuik.h>

Cuik_DriverArgs get_cuik_args(void) {
    return (Cuik_DriverArgs){
        .version = CUIK_VERSION_C23,
        .target = cuik_target_host(),
        .toolchain = cuik_toolchain_host(),
        .flavor = TB_FLAVOR_EXECUTABLE,
        .ir = true,
    };
}

static int aaa_len = 0;
static char aaa[10000];

typedef struct {
    TB_Label bb;
    float x, y;
} Node;

static int node_count = 0;
static Node nodes[100];

static void convert_to_str(void* user_data, const char* fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    aaa_len += vsnprintf(aaa + aaa_len, sizeof(aaa) - aaa_len, fmt, ap);
    va_end(ap);
}

static void print_doms(TB_Label* doms, size_t bb_count, TB_Label bb, int depth) {
    for (int i = 0; i < depth; i++) printf("  ");
    printf("L%d\n", bb);

    // elements in this line
    size_t n = 0;
    for (size_t i = 0; i < bb_count; i++) {
        n += (bb != i && doms[i] == bb);
    }

    float width = n * 120.0f;
    float step = width / n;
    float x = 60.0f + (width * -0.5f);

    for (size_t i = 0; i < bb_count; i++) if (bb != i && doms[i] == bb) {
        nodes[node_count++] = (Node){ i, x, depth * 120.0f };
        x += step;

        print_doms(doms, bb_count, i, depth + 1);
    }
}

int main() {
    cuik_init();

    const int screenWidth = 1280;
    const int screenHeight = 720;
    InitWindow(screenWidth, screenHeight, "Cuik inspector");

    // NOTE: raylib supports UTF-8 encoding, following list is actually codified as UTF8 internally
    const char msg[256] = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHI\nJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmn\nopqrstuvwxyz{|}~¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓ\nÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷\nøùúûüýþÿ";
    Font fontTtf = LoadFontEx("inspector/consola.ttf", 16, 0, 250);
    Font fontTtf2 = LoadFontEx("inspector/consola.ttf", 20, 0, 250);

    SetTargetFPS(165);

    Cuik_DriverArgs args = get_cuik_args();
    dyn_array_put(args.sources, "W:/Workspace/Cuik/tests/the_increment/bench/csel.c");

    CompilationUnit* cu = cuik_driver_compile(NULL, &args, true, false);

    TB_Module* mod = cuik_compilation_unit_tb_module(cu);

    TB_Function* f = tb_first_function(mod);
    TB_Predeccesors preds = tb_get_predeccesors(f);

    size_t bb_count = tb_function_get_label_count(f);
    TB_Label* doms = cuik_malloc(bb_count * sizeof(TB_Label));
    tb_get_dominators(f, preds, doms);

    node_count = 1;
    nodes[0] = (Node){ 0, 0.0f, 0.0f };
    print_doms(doms, bb_count, 0, 1);

    tb_function_print(f, convert_to_str, NULL, false);

    while (!WindowShouldClose()) {
        // useTtf = IsKeyDown(KEY_SPACE);
        int w = GetScreenWidth(), h = GetScreenHeight();

        BeginDrawing();
        ClearBackground(RAYWHITE);

        DrawTextEx(fontTtf, aaa, (Vector2){ 8.0f, 8.0f }, (float)fontTtf.baseSize, 2, BLACK);
        DrawRectangle(w / 2, 0, 1, h, BLACK);

        float nv_cx = w * 0.75f;
        float nv_cy = h * 0.25f;

        char txt[5];
        for (size_t i = 0; i < node_count; i++) {
            Node* n = &nodes[i];
            float x = nv_cx + n->x, y = nv_cy + n->y;

            snprintf(txt, 5, "L%d", n->bb);
            DrawCircleLines(x, y, 40.0f, BLACK);

            Vector2 size = MeasureTextEx(fontTtf, txt, fontTtf.baseSize, 0);
            DrawTextEx(fontTtf2, txt, (Vector2){ x - size.x*0.5f, y - size.y*0.5f }, fontTtf2.baseSize, 0, BLACK);
        }

        EndDrawing();
    }

    cuik_destroy_compilation_unit(cu);
    UnloadFont(fontTtf);
    CloseWindow();
    return 0;
}
