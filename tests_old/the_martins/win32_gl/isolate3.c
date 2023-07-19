// Cuik: cuik main.c --lib=user32.lib,opengl32.lib,gdi32.lib
//
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
#include "GL/glcorearb.h"  // download from https://www.khronos.org/registry/OpenGL/api/GL/glcorearb.h
#include "GL/wglext.h"     // download from https://www.khronos.org/registry/OpenGL/api/GL/wglext.h
// also download https://www.khronos.org/registry/EGL/api/KHR/khrplatform.h and put in "KHR" folder

#define _USE_MATH_DEFINES
#include <math.h>
#include <stddef.h>

// replace this with your favorite Assert() implementation
#include <intrin.h>
#define Assert(cond) do { if (!(cond)) __debugbreak(); } while (0)

#pragma comment (lib, "gdi32.lib")
#pragma comment (lib, "user32.lib")
#pragma comment (lib, "opengl32.lib")

static PFNWGLCHOOSEPIXELFORMATARBPROC wglChoosePixelFormatARB = NULL;

int WINAPI WinMain(HINSTANCE instance, HINSTANCE previnstance, LPSTR cmdline, int cmdshow)
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
        wglChoosePixelFormatARB(NULL, attrib, NULL, 1, &format, &formats);
}