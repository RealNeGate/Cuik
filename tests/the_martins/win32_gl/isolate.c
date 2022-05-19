
typedef unsigned long long size_t;
int printf(const char* format, ...);
int strncmp(const char * str1, const char * str2, size_t num);

/*_Bool separated(const char* src, const char* dst, size_t dstlen)
{
    return (*src && dstlen-- && *dst);
}*/

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

	//printf("%d %d\n", (_Bool)dstlen, !dstlen);
    return (dstlen && *src == *dst) || (!dstlen && *src == 0);
	//return strncmp(src, dst, dstlen) == 0;
}

void test(const char* ext) {
    const char* start = ext;
    for (;;)
    {
        while (*ext != 0 && *ext != ' ') 
		{
            ext++;
        }
		
        size_t length = ext - start;
		printf("Length: %zu, %.*s\n", length, (int)length, start);
		
		if (StringsAreEqual("WGL_ARB_pixel_format", start, length))
        {
            printf("A\n\n");
        }
        else if (StringsAreEqual("WGL_ARB_create_context", start, length))
        {
            printf("B\n\n");
        }
        else if (StringsAreEqual("WGL_EXT_swap_control", start, length))
        {
            printf("C\n\n");
        }

        if (*ext == 0)
        {
            break;
        }

        ext++;
        start = ext;
	}
}

int main() {
	test("WGL_ARB_buffer_region WGL_ARB_create_context WGL_ARB_create_context_no_error WGL_ARB_create_context_profile WGL_ARB_create_context_robustness WGL_ARB_context_flush_control WGL_ARB_extensions_string WGL_ARB_make_current_read WGL_ARB_multisample WGL_ARB_pbuffer WGL_ARB_pixel_format WGL_ARB_pixel_format_float WGL_ARB_render_texture WGL_ATI_pixel_format_float WGL_EXT_colorspace WGL_EXT_create_context_es_profile WGL_EXT_create_context_es2_profile WGL_EXT_extensions_string WGL_EXT_framebuffer_sRGB WGL_EXT_pixel_format_packed_float WGL_EXT_swap_control WGL_EXT_swap_control_tear WGL_NVX_DX_interop WGL_NV_DX_interop WGL_NV_DX_interop2 WGL_NV_copy_image WGL_NV_delay_before_swap WGL_NV_float_buffer WGL_NV_multisample_coverage WGL_NV_multigpu_context WGL_NV_render_depth_texture WGL_NV_render_texture_rectangle");
	return 0;
}
