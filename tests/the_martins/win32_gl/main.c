// example how to set up OpenGL core context on Windows
// and use basic functionality of OpenGL 4.5 version

// important extension functionality used here:
// (4.3) KHR_debug:                     https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_debug.txt
// (4.5) ARB_direct_state_access:       https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_direct_state_access.txt
// (4.1) ARB_separate_shader_objects:   https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_separate_shader_objects.txt
// (4.2) ARB_shading_language_420pack:  https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shading_language_420pack.txt
// (4.3) ARB_explicit_uniform_location: https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_explicit_uniform_location.txt

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <GL/gl.h>
#include "glcorearb.h"  // download from https://www.khronos.org/registry/OpenGL/api/GL/glcorearb.h
#include "wglext.h"     // download from https://www.khronos.org/registry/OpenGL/api/GL/wglext.h
// also download https://www.khronos.org/registry/EGL/api/KHR/khrplatform.h and put in "KHR" folder

#define _USE_MATH_DEFINES
#include <math.h>
#include <stddef.h>
#include <stdio.h>

// replace this with your favorite Assert() implementation
#include <intrin.h>
#define Assert(cond) do { if (!(cond)) __debugbreak(); } while (0)

#pragma comment (lib, "gdi32.lib")
#pragma comment (lib, "user32.lib")
#pragma comment (lib, "opengl32.lib")

// make sure you use functions that are valid for selected GL version (specified when context is created)
#define GL_FUNCTIONS(X) \
    X(PFNGLCREATEBUFFERSPROC,            glCreateBuffers            ) \
    X(PFNGLNAMEDBUFFERSTORAGEPROC,       glNamedBufferStorage       ) \
    X(PFNGLBINDVERTEXARRAYPROC,          glBindVertexArray          ) \
    X(PFNGLCREATEVERTEXARRAYSPROC,       glCreateVertexArrays       ) \
    X(PFNGLVERTEXARRAYATTRIBBINDINGPROC, glVertexArrayAttribBinding ) \
    X(PFNGLVERTEXARRAYVERTEXBUFFERPROC,  glVertexArrayVertexBuffer  ) \
    X(PFNGLVERTEXARRAYATTRIBFORMATPROC,  glVertexArrayAttribFormat  ) \
    X(PFNGLENABLEVERTEXARRAYATTRIBPROC,  glEnableVertexArrayAttrib  ) \
    X(PFNGLCREATESHADERPROGRAMVPROC,     glCreateShaderProgramv     ) \
    X(PFNGLGETPROGRAMIVPROC,             glGetProgramiv             ) \
    X(PFNGLGETPROGRAMINFOLOGPROC,        glGetProgramInfoLog        ) \
    X(PFNGLGENPROGRAMPIPELINESPROC,      glGenProgramPipelines      ) \
    X(PFNGLUSEPROGRAMSTAGESPROC,         glUseProgramStages         ) \
    X(PFNGLBINDPROGRAMPIPELINEPROC,      glBindProgramPipeline      ) \
    X(PFNGLPROGRAMUNIFORMMATRIX2FVPROC,  glProgramUniformMatrix2fv  ) \
    X(PFNGLBINDTEXTUREUNITPROC,          glBindTextureUnit          ) \
    X(PFNGLCREATETEXTURESPROC,           glCreateTextures           ) \
    X(PFNGLTEXTUREPARAMETERIPROC,        glTextureParameteri        ) \
    X(PFNGLTEXTURESTORAGE2DPROC,         glTextureStorage2D         ) \
    X(PFNGLTEXTURESUBIMAGE2DPROC,        glTextureSubImage2D        ) \
    X(PFNGLDEBUGMESSAGECALLBACKPROC,     glDebugMessageCallback     )

#define X(type, name) static type name;
GL_FUNCTIONS(X)
#undef X

#define STR2(x) #x
#define STR(x) STR2(x)

static void FatalError(const char* message)
{
    MessageBoxA(NULL, message, "Error", MB_ICONEXCLAMATION);
    ExitProcess(0);
}

// hacky
#ifdef __CUIKC__
void* __readgsqword(LONG offset) { return NULL; }
void __stosb(char* ptr, char src, size_t count) {}
void _chvalidator_l() {}
LONG InterlockedExchangeAdd(LONG volatile *Addend, LONG Value) { return 0; }
long long InterlockedExchangeAdd64(long long volatile *Addend, long long Value) { return 0; }
unsigned long long __shiftright128 ( unsigned long long _LowPart , unsigned long long _HighPart , unsigned char _Shift ) { return 0; }
LPUWSTR __stdcall uaw_CharUpperW(LPUWSTR String) { return NULL; }
#endif

#ifndef NDEBUG
static void APIENTRY DebugCallback(
    GLenum source, GLenum type, GLuint id, GLenum severity,
    GLsizei length, const GLchar* message, const void* user)
{
    OutputDebugStringA(message);
    OutputDebugStringA("\n");
    if (severity == GL_DEBUG_SEVERITY_HIGH || severity == GL_DEBUG_SEVERITY_MEDIUM)
    {
        if (IsDebuggerPresent())
        {
            Assert(!"OpenGL error - check the callstack in debugger");
        }
        FatalError("OpenGL API usage error! Use debugger to examine call stack!");
    }
}
#endif

LRESULT CALLBACK WindowProc(HWND wnd, UINT msg, WPARAM wparam, LPARAM lparam)
{
    switch (msg)
    {
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
	
    return DefWindowProcW(wnd, msg, wparam, lparam);
}

// compares src string with dstlen characters from dst, returns 1 if they are equal, 0 if not
static int StringsAreEqual(const char* src, const char* dst, size_t dstlen)
{
    while (*src && dstlen-- && *dst)
    {
        if (*src++ != *dst++)
        {
            return 0;
        }
    }

    return (dstlen && *src == *dst) || (!dstlen && *src == 0);
}

static PFNWGLCHOOSEPIXELFORMATARBPROC wglChoosePixelFormatARB = NULL;
static PFNWGLCREATECONTEXTATTRIBSARBPROC wglCreateContextAttribsARB = NULL;
static PFNWGLSWAPINTERVALEXTPROC wglSwapIntervalEXT = NULL;

static void GetWglFunctions(void)
{
    // to get WGL functions we need valid GL context, so create dummy window for dummy GL contetx
    HWND dummy = CreateWindowExW(
        0, L"STATIC", L"DummyWindow", WS_OVERLAPPED,
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
        NULL, NULL, NULL, NULL);
    Assert(dummy && "Failed to create dummy window");

    HDC dc = GetDC(dummy);
    Assert(dc && "Failed to get device context for dummy window");

    PIXELFORMATDESCRIPTOR desc =
    {
        .nSize = sizeof(desc),
        .nVersion = 1,
        .dwFlags = PFD_DRAW_TO_WINDOW | PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER,
        .iPixelType = PFD_TYPE_RGBA,
        .cColorBits = 24,
    };

    int format = ChoosePixelFormat(dc, &desc);
    if (!format)
    {
        FatalError("Cannot choose OpenGL pixel format for dummy window!");
    }

    int ok = DescribePixelFormat(dc, format, sizeof(desc), &desc);
    Assert(ok && "Failed to describe OpenGL pixel format");

    // reason to create dummy window is that SetPixelFormat can be called only once for the window
    if (!SetPixelFormat(dc, format, &desc))
    {
        FatalError("Cannot set OpenGL pixel format for dummy window!");
    }

    HGLRC rc = wglCreateContext(dc);
    Assert(rc && "Failed to create OpenGL context for dummy window");

    ok = wglMakeCurrent(dc, rc);
    Assert(ok && "Failed to make current OpenGL context for dummy window");

    // https://www.khronos.org/registry/OpenGL/extensions/ARB/WGL_ARB_extensions_string.txt
    PFNWGLGETEXTENSIONSSTRINGARBPROC wglGetExtensionsStringARB =
        (void*)wglGetProcAddress("wglGetExtensionsStringARB");
    if (!wglGetExtensionsStringARB)
    {
        FatalError("OpenGL does not support WGL_ARB_extensions_string extension!");
    }

    const char* ext = wglGetExtensionsStringARB(dc);
    Assert(ext && "Failed to get OpenGL WGL extension string");

    const char* start = ext;
    for (;;)
    {
        for (;;)
        {
			if (*ext == 0) break;
			if (*ext == ' ') break;
            ext++;
        }

        size_t length = ext - start;
        if (StringsAreEqual("WGL_ARB_pixel_format", start, length))
        {
            // https://www.khronos.org/registry/OpenGL/extensions/ARB/WGL_ARB_pixel_format.txt
            wglChoosePixelFormatARB = (void*)wglGetProcAddress("wglChoosePixelFormatARB");
        }
        else if (StringsAreEqual("WGL_ARB_create_context", start, length))
        {
            // https://www.khronos.org/registry/OpenGL/extensions/ARB/WGL_ARB_create_context.txt
            wglCreateContextAttribsARB = (void*)wglGetProcAddress("wglCreateContextAttribsARB");
        }
        else if (StringsAreEqual("WGL_EXT_swap_control", start, length))
        {
            // https://www.khronos.org/registry/OpenGL/extensions/EXT/WGL_EXT_swap_control.txt
            wglSwapIntervalEXT = (void*)wglGetProcAddress("wglSwapIntervalEXT");
        }

        if (*ext == 0)
        {
            break;
        }

        ext++;
        start = ext;
    }
	
    if (!wglChoosePixelFormatARB || !wglCreateContextAttribsARB || !wglSwapIntervalEXT)
    {
        FatalError("OpenGL does not support required WGL extensions for modern context!");
    }

    wglMakeCurrent(NULL, NULL);
    wglDeleteContext(rc);
    ReleaseDC(dummy, dc);
    DestroyWindow(dummy);
}

int main()
{
    HINSTANCE instance = GetModuleHandle(NULL);
	
    // get WGL functions to be able to create modern GL context
    GetWglFunctions();

    // register window class to have custom WindowProc callback
    WNDCLASSEXW wc =
    {
        .cbSize = sizeof(wc),
        .lpfnWndProc = WindowProc,
        .hInstance = instance,
        //.hIcon = LoadIcon(NULL, IDI_APPLICATION),
        //.hCursor = LoadCursor(NULL, IDC_ARROW),
        .lpszClassName = L"opengl_window_class",
    };
    ATOM atom = RegisterClassExW(&wc);
    Assert(atom && "Failed to register window class");

    // window properties - width, height and style
    int width = CW_USEDEFAULT;
    int height = CW_USEDEFAULT;
    DWORD exstyle = WS_EX_APPWINDOW;
    DWORD style = WS_OVERLAPPEDWINDOW;

    // uncomment in case you want fixed size window
    //style &= ~WS_THICKFRAME & ~WS_MAXIMIZEBOX;
    //RECT rect = { 0, 0, 1280, 720 };
    //AdjustWindowRectEx(&rect, style, FALSE, exstyle);
    //width = rect.right - rect.left;
    //height = rect.bottom - rect.top;

    // create window
    HWND window = CreateWindowExW(
        exstyle, wc.lpszClassName, L"OpenGL Window", style,
        CW_USEDEFAULT, CW_USEDEFAULT, width, height,
        NULL, NULL, wc.hInstance, NULL);
    Assert(window && "Failed to create window");

    HDC dc = GetDC(window);
    Assert(dc && "Failed to window device context");

    // set pixel format for OpenGL context
    {
        int attrib[] =
        {
            WGL_DRAW_TO_WINDOW_ARB, GL_TRUE,
            WGL_SUPPORT_OPENGL_ARB, GL_TRUE,
            WGL_DOUBLE_BUFFER_ARB,  GL_TRUE,
            WGL_PIXEL_TYPE_ARB,     WGL_TYPE_RGBA_ARB,
            WGL_COLOR_BITS_ARB,     24,
            WGL_DEPTH_BITS_ARB,     24,
            WGL_STENCIL_BITS_ARB,   8,

            // uncomment for sRGB framebuffer, from WGL_ARB_framebuffer_sRGB extension
            // https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_framebuffer_sRGB.txt
            //WGL_FRAMEBUFFER_SRGB_CAPABLE_ARB, GL_TRUE,

            // uncomment for multisampeld framebuffer, from WGL_ARB_multisample extension
            // https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_multisample.txt
            //WGL_SAMPLE_BUFFERS_ARB, 1,
            //WGL_SAMPLES_ARB,        4, // 4x MSAA

            0,
        };

        int format;
        UINT formats;
        if (!wglChoosePixelFormatARB(dc, attrib, NULL, 1, &format, &formats) || formats == 0)
        {
            FatalError("OpenGL does not support required pixel format!");
        }

        PIXELFORMATDESCRIPTOR desc = { .nSize = sizeof(desc) };
        int ok = DescribePixelFormat(dc, format, sizeof(desc), &desc);
        Assert(ok && "Failed to describe OpenGL pixel format");

        if (!SetPixelFormat(dc, format, &desc))
        {
            FatalError("Cannot set OpenGL selected pixel format!");
        }
    }

    // create modern OpenGL context
    {
        int attrib[] =
        {
            WGL_CONTEXT_MAJOR_VERSION_ARB, 4,
            WGL_CONTEXT_MINOR_VERSION_ARB, 5,
            WGL_CONTEXT_PROFILE_MASK_ARB,  WGL_CONTEXT_CORE_PROFILE_BIT_ARB,
#ifndef NDEBUG
            // ask for debug context for non "Release" builds
            // this is so we can enable debug callback
            WGL_CONTEXT_FLAGS_ARB, WGL_CONTEXT_DEBUG_BIT_ARB,
#endif
            0,
        };

        HGLRC rc = wglCreateContextAttribsARB(dc, NULL, attrib);
        if (!rc)
        {
            FatalError("Cannot create modern OpenGL context! OpenGL version 4.5 not supported?");
        }

        BOOL ok = wglMakeCurrent(dc, rc);
        Assert(ok && "Failed to make current OpenGL context");

        // load OpenGL functions
#define X(type, name) name = (type)wglGetProcAddress(#name); Assert(name);
        GL_FUNCTIONS(X)
#undef X

#ifndef NDEBUG
        // enable debug callback
        glDebugMessageCallback(&DebugCallback, NULL);
        glEnable(GL_DEBUG_OUTPUT_SYNCHRONOUS);
#endif
    }

    struct Vertex
    {
        float position[2];
        float uv[2];
        float color[3];
    };

    // vertex buffer containing triangle vertices
    GLuint vbo;
    {
        struct Vertex data[] =
        {
            { { -0.00f, +0.75f }, { 25.0f, 50.0f }, { 1, 0, 0 } },
            { { +0.75f, -0.50f }, {  0.0f,  0.0f }, { 0, 1, 0 } },
            { { -0.75f, -0.50f }, { 50.0f,  0.0f }, { 0, 0, 1 } },
        };
        glCreateBuffers(1, &vbo);
        glNamedBufferStorage(vbo, sizeof(data), data, 0);
    }

    // vertex input
    GLuint vao;
    {
        glCreateVertexArrays(1, &vao);

        GLint vbuf_index = 0;
        glVertexArrayVertexBuffer(vao, vbuf_index, vbo, 0, sizeof(struct Vertex));

        GLint a_pos = 0;
        glVertexArrayAttribFormat(vao, a_pos, 2, GL_FLOAT, GL_FALSE, offsetof(struct Vertex, position));
        glVertexArrayAttribBinding(vao, a_pos, vbuf_index);
        glEnableVertexArrayAttrib(vao, a_pos);

        GLint a_uv = 1;
        glVertexArrayAttribFormat(vao, a_uv, 2, GL_FLOAT, GL_FALSE, offsetof(struct Vertex, uv));
        glVertexArrayAttribBinding(vao, a_uv, vbuf_index);
        glEnableVertexArrayAttrib(vao, a_uv);

        GLint a_color = 2;
        glVertexArrayAttribFormat(vao, a_color, 3, GL_FLOAT, GL_FALSE, offsetof(struct Vertex, color));
        glVertexArrayAttribBinding(vao, a_color, vbuf_index);
        glEnableVertexArrayAttrib(vao, a_color);
    }

    // checkerboard texture, with 50% transparency on black colors
    GLuint texture;
    {
        unsigned int pixels[] =
        {
            0x80000000, 0xffffffff,
            0xffffffff, 0x80000000,
        };

        glCreateTextures(GL_TEXTURE_2D, 1, &texture);
        glTextureParameteri(texture, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTextureParameteri(texture, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTextureParameteri(texture, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTextureParameteri(texture, GL_TEXTURE_WRAP_T, GL_REPEAT);

        GLsizei width = 2;
        GLsizei height = 2;
        glTextureStorage2D(texture, 1, GL_RGBA8, width, height);
        glTextureSubImage2D(texture, 0, 0, 0, width, height, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    }

    // fragment & vertex shaders for drawing triangle
    GLuint pipeline, vshader, fshader;
    {
        const char* glsl_vshader =
            "#version 450 core                             \n"
            // "#line " STR(__LINE__) "                     \n\n" // actual line number in this file for nicer error messages
            "                                              \n"
            "layout (location=0) in vec2 a_pos;            \n" // position attribute index 0
            "layout (location=1) in vec2 a_uv;             \n" // uv attribute index 1
            "layout (location=2) in vec3 a_color;          \n" // color attribute index 2
            "                                              \n"
            "layout (location=0)                           \n" // (from ARB_explicit_uniform_location)
            "uniform mat2 u_matrix;                        \n" // matrix uniform location 0
            "                                              \n"
            "out gl_PerVertex { vec4 gl_Position; };       \n" // required because of ARB_separate_shader_objects
            "out vec2 uv;                                  \n"
            "out vec4 color;                               \n"
            "                                              \n"
            "void main()                                   \n"
            "{                                             \n"
            "    vec2 pos = u_matrix * a_pos;              \n"
            "    gl_Position = vec4(pos, 0, 1);            \n"
            "    uv = a_uv;                                \n"
            "    color = vec4(a_color, 1);                 \n"
            "}                                             \n"
        ;

        const char* glsl_fshader =
            "#version 450 core                             \n"
            // "#line " STR(__LINE__) "                     \n\n" // actual line number in this file for nicer error messages
            "                                              \n"
            "in vec2 uv;                                   \n"
            "in vec4 color;                                \n"
            "                                              \n"
            "layout (binding=0)                            \n" // (from ARB_shading_language_420pack)
            "uniform sampler2D s_texture;                  \n" // texture unit binding 0
            "                                              \n"
            "layout (location=0)                           \n"
            "out vec4 o_color;                             \n" // output fragment data location 0
            "                                              \n"
            "void main()                                   \n"
            "{                                             \n"
            "    o_color = color * texture(s_texture, uv); \n"
            "}                                             \n"
        ;

        vshader = glCreateShaderProgramv(GL_VERTEX_SHADER, 1, &glsl_vshader);
        fshader = glCreateShaderProgramv(GL_FRAGMENT_SHADER, 1, &glsl_fshader);

        GLint linked;
        glGetProgramiv(vshader, GL_LINK_STATUS, &linked);
        if (!linked)
        {
            char message[1024];
            glGetProgramInfoLog(vshader, sizeof(message), NULL, message);
            OutputDebugStringA(message);
            Assert(!"Failed to create vertex shader!");
        }

        glGetProgramiv(fshader, GL_LINK_STATUS, &linked);
        if (!linked)
        {
            char message[1024];
            glGetProgramInfoLog(fshader, sizeof(message), NULL, message);
            OutputDebugStringA(message);
            Assert(!"Failed to create fragment shader!");
        }

        glGenProgramPipelines(1, &pipeline);
        glUseProgramStages(pipeline, GL_VERTEX_SHADER_BIT, vshader);
        glUseProgramStages(pipeline, GL_FRAGMENT_SHADER_BIT, fshader);
    }

    // setup global GL state
    {
        // enable alpha blending
        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        // disble depth testing
        glDisable(GL_DEPTH_TEST);

        // disable culling
        glDisable(GL_CULL_FACE);
    }

    // set to FALSE to disable vsync
    BOOL vsync = FALSE;
    wglSwapIntervalEXT(vsync ? 1 : 0);

    // show the window
    ShowWindow(window, SW_SHOWDEFAULT);

    LARGE_INTEGER freq, c1;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&c1);

    float angle = 0.0f;

    for (;;)
    {
        // process all incoming Windows messages
        MSG msg;
        if (PeekMessageW(&msg, NULL, 0, 0, PM_REMOVE))
        {
            if (msg.message == WM_QUIT)
            {
                break;
            }
            TranslateMessage(&msg);
            DispatchMessageW(&msg);
            continue;
        }

        // get current window client area size
        RECT rect;
        GetClientRect(window, &rect);
        width = rect.right - rect.left;
        height = rect.bottom - rect.top;

        LARGE_INTEGER c2;
        QueryPerformanceCounter(&c2);
        float delta = (float)((double)(c2.QuadPart - c1.QuadPart) / freq.QuadPart);
        c1 = c2;

        // render only if window size is non-zero
        if (width != 0 && height != 0)
        {
            // setup output size covering all client area of window
            glViewport(0, 0, width, height);

            // clear screen
            glClearColor(0.392f, 0.584f, 0.929f, 1.f);
            glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

            // setup rotation matrix in uniform
            {
                angle += delta * 2.0f * (float)M_PI / 20.0f; // full rotation in 20 seconds
                angle = fmodf(angle, 2.0f * (float)M_PI);

                float aspect = (float)height / width;
                float matrix[] =
                {
                    cosf(angle) * aspect, -sinf(angle),
                    sinf(angle) * aspect,  cosf(angle),
                };


                GLint u_matrix = 0;
                glProgramUniformMatrix2fv(vshader, u_matrix, 1, GL_FALSE, matrix);
            }

            // activate shaders for next draw call
            glBindProgramPipeline(pipeline);

            // provide vertex input
            glBindVertexArray(vao);

            // bind texture to texture unit
            GLint s_texture = 0; // texture unit that sampler2D will use in GLSL code
            glBindTextureUnit(s_texture, texture);

            // draw 3 vertices as triangle
            glDrawArrays(GL_TRIANGLES, 0, 3);

            // swap the buffers to show output
            if (!SwapBuffers(dc))
            {
                FatalError("Failed to swap OpenGL buffers!");
            }
        }
        else
        {
            // window is minimized, cannot vsync - instead sleep a bit
            if (vsync)
            {
                Sleep(10);
            }
        }
    }
}
