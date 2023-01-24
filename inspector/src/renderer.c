#include <SDL2/SDL.h>
#include <SDL2/SDL_syswm.h>
#include <SDL2/SDL_ttf.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "renderer.h"
#include "atlas.inl"

#include <GL/gl.h>
#include "glcorearb.h"

#ifdef _WIN32
#pragma comment(lib, "Shcore.lib")
#include <ShellScalingAPI.h>

#include "wglext.h"
#endif /* _WIN32 */

#define STB_RECT_PACK_IMPLEMENTATION
#include "stb_rect_pack.h"

#define MAX_QUADS 16384
#define MAX_CLIPS 256
#define FONT_TEXTURE_SIZE 1024

typedef struct {
    float l, t, r, b;
} Clip;

typedef struct {
    float x, y, w, h;
    uint16_t u, v, s, t;
    mu_Color color;
    uint32_t extra;
} Rect;

static int width = 1600, height = 900;
static SDL_Window* window;

static GLuint vao, rect_bo[2];
static int texture_ids_used;
static GLuint texture_ids[16];

static Rect* rects;
static int rect_head, rect_tail;
static GLuint program;
static GLuint atlas_id;

static GLuint clip_bo[2];
static Clip* clip_rects;

static int clip_rect_id;
static GLchar tmp_string[1024];

static struct {
    // starts at 32 goes to 128
    GLuint texture;
    int descent;
    stbrp_rect glyphs[96];
} font;

// make sure you use functions that are valid for selected GL version (specified when context is created)
#define GL_FUNCTIONS(X) \
X(PFNGLMEMORYBARRIERPROC,            glMemoryBarrier            ) \
X(PFNGLUSEPROGRAMPROC,               glUseProgram               ) \
X(PFNGLUNIFORM1IPROC,                glUniform1i                ) \
X(PFNGLUNIFORM2FPROC,                glUniform2f                ) \
X(PFNGLCREATEBUFFERSPROC,            glCreateBuffers            ) \
X(PFNGLMAPNAMEDBUFFERRANGEPROC,      glMapNamedBufferRange      ) \
X(PFNGLNAMEDBUFFERSTORAGEPROC,       glNamedBufferStorage       ) \
X(PFNGLCOPYNAMEDBUFFERSUBDATAPROC,   glCopyNamedBufferSubData   ) \
X(PFNGLBINDBUFFERBASEPROC,           glBindBufferBase           ) \
X(PFNGLBINDVERTEXARRAYPROC,          glBindVertexArray          ) \
X(PFNGLCREATEVERTEXARRAYSPROC,       glCreateVertexArrays       ) \
X(PFNGLVERTEXARRAYATTRIBBINDINGPROC, glVertexArrayAttribBinding ) \
X(PFNGLVERTEXARRAYVERTEXBUFFERPROC,  glVertexArrayVertexBuffer  ) \
X(PFNGLVERTEXARRAYATTRIBFORMATPROC,  glVertexArrayAttribFormat  ) \
X(PFNGLENABLEVERTEXARRAYATTRIBPROC,  glEnableVertexArrayAttrib  ) \
X(PFNGLCREATESHADERPROC,             glCreateShader             ) \
X(PFNGLCREATEPROGRAMPROC,            glCreateProgram            ) \
X(PFNGLSHADERSOURCEPROC,             glShaderSource             ) \
X(PFNGLCOMPILESHADERPROC,            glCompileShader            ) \
X(PFNGLATTACHSHADERPROC,             glAttachShader             ) \
X(PFNGLDELETEPROGRAMPROC,            glDeleteProgram            ) \
X(PFNGLLINKPROGRAMPROC,              glLinkProgram              ) \
X(PFNGLGETSHADERIVPROC,              glGetShaderiv              ) \
X(PFNGLGETPROGRAMIVPROC,             glGetProgramiv             ) \
X(PFNGLGETSHADERINFOLOGPROC,         glGetShaderInfoLog         ) \
X(PFNGLGETPROGRAMINFOLOGPROC,        glGetProgramInfoLog        ) \
X(PFNGLBINDTEXTURESPROC,             glBindTextures             ) \
X(PFNGLCREATETEXTURESPROC,           glCreateTextures           ) \
X(PFNGLTEXTUREPARAMETERIPROC,        glTextureParameteri        ) \
X(PFNGLTEXTURESTORAGE2DPROC,         glTextureStorage2D         ) \
X(PFNGLTEXTURESUBIMAGE2DPROC,        glTextureSubImage2D        ) \
X(PFNGLDEBUGMESSAGECALLBACKPROC,     glDebugMessageCallback     )

#define X(type, name) static type name;
GL_FUNCTIONS(X)
#undef X

static GLuint load_shader(GLenum type, const char* src) {
    GLuint id = glCreateShader(type);
    glShaderSource(id, 1, &src, NULL);
    glCompileShader(id);

    GLint result;
    glGetShaderiv(id, GL_COMPILE_STATUS, &result);
    if (result == GL_FALSE) {
        glGetShaderInfoLog(id, 1024, NULL, tmp_string);
        __debugbreak();
        return -1;
    }

    return id;
}

static GLuint load_shader_program(GLuint vs, GLuint fs) {
    GLuint program = glCreateProgram();
    glAttachShader(program, vs);
    glAttachShader(program, fs);

    GLint result;
    glLinkProgram(program);
    glGetProgramiv(program, GL_LINK_STATUS, &result);
    if (!result) {
        glGetProgramInfoLog(program, 1024, NULL, tmp_string);
        glDeleteProgram(program);
        __debugbreak();
        return -1;
    }

    return program;
}

float r_width(void) { return width; }
float r_height(void) { return height; }

void r_init(void) {
    #ifdef _WIN32
    SetProcessDpiAwareness(PROCESS_PER_MONITOR_DPI_AWARE);
    #endif /* _WIN32 */

    SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 4);
    SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 5);

    // init SDL window
    window = SDL_CreateWindow(
        NULL, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
        width, height, SDL_WINDOW_OPENGL | SDL_WINDOW_ALLOW_HIGHDPI
    );

    TTF_Init();
    SDL_GL_CreateContext(window);

    #define X(type, name) name = (type)SDL_GL_GetProcAddress(#name);
    GL_FUNCTIONS(X);
    #undef X

    const char* VERTEX =
        "#version 450 core\n"
        "\n"
        "struct Rect {\n"
        "    vec4 bounds;\n"
        "    uvec4 payload;\n"
        "};\n"
        "\n"
        "layout(binding = 0, std140) readonly buffer Primitives {"
        "    Rect rects[];"
        "};\n"
        "layout(binding = 1, std140) uniform Clips {"
        "    vec4 clips[256];"
        "};\n"
        "\n"
        "layout(location = 0) uniform vec2 uProj;\n"
        "\n"
        "flat out uint Tex;\n"
        "out vec2 UV;\n"
        "out vec4 Color;\n"
        "\n"
        "out gl_PerVertex { vec4 gl_Position; float gl_ClipDistance[]; };\n"
        "\n"
        "const vec2 verts[6] = vec2[](\n"
        "    vec2(0, 0), vec2(1, 0), vec2(1, 1),\n"
        "    vec2(1, 1), vec2(0, 1), vec2(0, 0)\n"
        ");\n"
        "\n"
        "void main() {\n"
        "    // calculate rect ID\n"
        "    uint rid = gl_VertexID / 6;\n"
        "    uint vid = gl_VertexID % 6;\n"
        "\n"
        "    vec4 clip   = clips[rects[rid].payload.w >> 16];\n"
        "    vec4 bounds = rects[rid].bounds;\n"
        "    vec2 uv     = unpackUnorm2x16(rects[rid].payload.x);\n"
        "    vec2 st     = unpackUnorm2x16(rects[rid].payload.y);\n"
        "    vec4 color  = unpackUnorm4x8(rects[rid].payload.z);\n"
        "    uint tex    = rects[rid].payload.w & 0xFFFF;\n"
        "\n"
        "    Color = color;\n"
        "    UV = uv + verts[vid]*st;\n"
        "    Tex = tex;\n"
        "\n"
        "    vec2 pos = bounds.xy + verts[vid]*bounds.zw;\n"
        "    gl_Position = vec4(pos.x*uProj.x - 1.0, pos.y*uProj.y + 1.0, 0.0, 1.0);\n"
        "    gl_ClipDistance[0] = pos.x - clip.x;\n"
        "    gl_ClipDistance[1] = pos.y - clip.y;\n"
        "    gl_ClipDistance[2] = -pos.x + clip.z;\n"
        "    gl_ClipDistance[3] = -pos.y + clip.w;\n"
        "}\n";
    const char* FRAGMENT =
        "#version 450 core\n"
        "\n"
        "out vec4 fColor;\n"
        "\n"
        "flat in uint Tex;\n"
        "in vec2 UV;\n"
        "in vec4 Color;\n"
        "\n"
        "layout(location = 1) uniform sampler2D uTextures[16];\n"
        "\n"
        "void main() {\n"
        "   fColor = Color.bgra;\n"
        "   fColor.a *= texture(uTextures[Tex], UV).r;\n"
        "}\n";

    program = load_shader_program(
        load_shader(GL_VERTEX_SHADER, VERTEX),
        load_shader(GL_FRAGMENT_SHADER, FRAGMENT)
    );

    glUseProgram(program);
    for (int i = 0; i < 16; i++) {
        glUniform1i(1+i, i);
    }
    glUseProgram(0);

    glDisable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);
    glEnable(GL_BLEND);
    glEnable(GL_CLIP_DISTANCE0);
    glEnable(GL_CLIP_DISTANCE1);
    glEnable(GL_CLIP_DISTANCE2);
    glEnable(GL_CLIP_DISTANCE3);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // Create Primitive buffer
    glCreateVertexArrays(1, &vao);
    glCreateBuffers(2, rect_bo);

    int flags = GL_MAP_WRITE_BIT | GL_MAP_PERSISTENT_BIT | GL_MAP_COHERENT_BIT;
    glNamedBufferStorage(rect_bo[0], MAX_QUADS * sizeof(Rect), NULL, flags);
    glNamedBufferStorage(rect_bo[1], MAX_QUADS * sizeof(Rect), NULL, 0);
    rects = glMapNamedBufferRange(rect_bo[0], 0, MAX_QUADS * sizeof(Rect), flags | GL_MAP_INVALIDATE_BUFFER_BIT);

    glCreateBuffers(2, clip_bo);
    glNamedBufferStorage(clip_bo[0], MAX_QUADS * sizeof(Clip), NULL, flags);
    glNamedBufferStorage(clip_bo[1], MAX_QUADS * sizeof(Clip), NULL, 0);
    clip_rects = glMapNamedBufferRange(clip_bo[0], 0, MAX_CLIPS * sizeof(Clip), flags | GL_MAP_INVALIDATE_BUFFER_BIT);

    glCreateTextures(GL_TEXTURE_2D, 1, &atlas_id);
    glTextureParameteri(atlas_id, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTextureParameteri(atlas_id, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTextureParameteri(atlas_id, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTextureParameteri(atlas_id, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTextureStorage2D(atlas_id, 1, GL_R8, ATLAS_WIDTH, ATLAS_HEIGHT);
    glTextureSubImage2D(atlas_id, 0, 0, 0, ATLAS_WIDTH, ATLAS_HEIGHT, GL_RED, GL_UNSIGNED_BYTE, atlas_texture);

    {
        TTF_Font* sdl_font = TTF_OpenFont("notosans.ttf", 16);
        if (sdl_font == NULL) {
            const char* err = TTF_GetError();
            printf("%s\n", err);
            abort();
        }

        for (size_t i = 32; i < 128; i++) {
            int w, h;
            char ch[2] = { i, 0 };
            TTF_SizeUTF8(sdl_font, ch, &w, &h);

            font.glyphs[i - 32].w = w, font.glyphs[i - 32].h = h;
        }

        stbrp_context ctx;
        stbrp_init_target(&ctx, FONT_TEXTURE_SIZE, FONT_TEXTURE_SIZE, malloc(100 * sizeof(stbrp_node)), 100);
        stbrp_pack_rects(&ctx, font.glyphs, 96);

        // upload characters now
        SDL_Surface* s = SDL_CreateRGBSurface(0, FONT_TEXTURE_SIZE, FONT_TEXTURE_SIZE, 32, 0, 0, 0, 0xff);
        SDL_SetColorKey(s, SDL_TRUE, SDL_MapRGBA(s->format, 0, 0, 0, 0));

        for (size_t i = 32; i < 128; i++) {
            stbrp_rect* r = &font.glyphs[i - 32];

            SDL_Surface* glyph = TTF_RenderGlyph32_Blended(sdl_font, i, (SDL_Color){ 255, 255, 255, 255 });
            if (glyph != NULL) {
                SDL_Rect dst = { r->x, r->y, glyph->w, glyph->h };
                SDL_BlitSurface(glyph, NULL, s, &dst);
                SDL_FreeSurface(glyph);
            }
        }

        GLuint id;
        glCreateTextures(GL_TEXTURE_2D, 1, &id);
        glTextureParameteri(id, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTextureParameteri(id, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTextureParameteri(id, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTextureParameteri(id, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
        glTextureStorage2D(id, 1, GL_RGBA8, FONT_TEXTURE_SIZE, FONT_TEXTURE_SIZE);
        font.texture = id;
        font.descent = TTF_FontDescent(sdl_font);

        SDL_LockSurface(s);
        glTextureSubImage2D(id, 0, 0, 0, s->w, s->h, GL_RGBA, GL_UNSIGNED_BYTE, s->pixels);
        SDL_UnlockSurface(s);
        SDL_FreeSurface(s);
    }
}

static void flush(void) {
    if (rect_head == rect_tail) return;

    // copy next
    glCopyNamedBufferSubData(rect_bo[0], rect_bo[1], rect_head * sizeof(Rect), rect_head * sizeof(Rect), (rect_tail - rect_head) * sizeof(Rect));
    glCopyNamedBufferSubData(clip_bo[0], clip_bo[1], 0, 0, clip_rect_id * sizeof(mu_Rect));

    glBindTextures(0, texture_ids_used, texture_ids);
    glDrawArrays(GL_TRIANGLES, rect_head * 6, (rect_tail - rect_head) * 6);
    glBindVertexArray(0);

    texture_ids_used = 2;
    rect_head = rect_tail;
}

static void push_quad(GLint tid, float tw, float th, mu_Rect dst, mu_Rect src, mu_Color color) {
    if (rect_tail == MAX_QUADS) { flush(); }

    float u = src.x / tw;
    float v = src.y / th;
    float s = src.w / tw;
    float t = src.h / th;

    rects[rect_tail++] = (Rect){
        // pos & size
        dst.x, dst.y, dst.w, dst.h,
        // tex coords
        u*65535.0, v*65535.0, s*65535.0, t*65535.0,
        // color
        .color = color, .extra = tid | ((clip_rect_id - 1) << 16)
    };
}

void r_draw_rect(mu_Rect rect, mu_Color color) {
    push_quad(0, ATLAS_WIDTH, ATLAS_HEIGHT, rect, atlas[ATLAS_WHITE], color);
}

void r_draw_text(const char *text, mu_Vec2 pos, mu_Color color) {
    mu_Rect dst = { pos.x, pos.y + font.descent, 0, 0 };
    for (const char *p = text; *p; p++) {
        if ((*p & 0xc0) == 0x80) { continue; }
        int chr = mu_min((unsigned char) *p, 127);

        #if 1
        stbrp_rect* r = &font.glyphs[chr - 32];
        dst.w = r->w;
        dst.h = r->h;

        mu_Rect src = { r->x, r->y, r->w, r->h };
        push_quad(1, FONT_TEXTURE_SIZE, FONT_TEXTURE_SIZE, dst, src, color);
        dst.x += dst.w;
        #else
        mu_Rect src = atlas[ATLAS_FONT + chr];
        dst.w = src.w;
        dst.h = src.h;
        push_quad(0, ATLAS_WIDTH, ATLAS_HEIGHT, dst, src, color);
        dst.x += dst.w;
        #endif
    }
}

void r_draw_icon(int id, mu_Rect rect, mu_Color color) {
    mu_Rect src = atlas[id];
    int x = rect.x + (rect.w - src.w) / 2;
    int y = rect.y + (rect.h - src.h) / 2;
    push_quad(0, ATLAS_WIDTH, ATLAS_HEIGHT, mu_rect(x, y, src.w, src.h), src, color);
}

int r_get_text_width(const char *text, int len) {
    int res = 0;
    for (const char *p = text; *p && len--; p++) {
        if ((*p & 0xc0) == 0x80) { continue; }
        int chr = mu_min((unsigned char) *p, 127);
        res += font.glyphs[chr - 32].w;
        // res += atlas[ATLAS_FONT + chr].w;
    }
    return res;
}

int r_get_text_height(void) {
    return 16;
}

void r_set_clip_rect(mu_Rect rect) {
    assert(clip_rect_id+1 < MAX_CLIPS);
    clip_rects[clip_rect_id++] = (Clip){ rect.x, rect.y, rect.x+rect.w, rect.y+rect.h };
}

void r_clear(mu_Color clr) {
    glClearColor(clr.r / 255., clr.g / 255., clr.b / 255., clr.a / 255.);
    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);

    glUseProgram(program);
    glUniform2f(0, 2.0f / width, -2.0f / height);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, rect_bo[1]);
    glBindBufferBase(GL_UNIFORM_BUFFER, 1, clip_bo[1]);
    glBindVertexArray(vao);

    rect_head = rect_tail = 0;

    clip_rect_id = 1;
    clip_rects[0] = (Clip){ 0.0f, 0.0f, width, height };

    texture_ids_used = 2;
    texture_ids[0] = atlas_id;
    texture_ids[1] = font.texture;
}

void r_present(void) {
    flush();

    glBindVertexArray(0);
    glUseProgram(0);

    SDL_GL_SwapWindow(window);
    glFinish();
}
