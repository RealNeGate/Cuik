typedef unsigned char stbi_uc;

typedef struct {
    int x, y, z;
} stbi__context;

static stbi_uc stbi__get8(stbi__context *s) {
    return s->x++;
}

int stbi__check_png_header(stbi__context *s)
{
    static const stbi_uc png_sig[8] = { 137,80,78,71,13,10,26,10 };
    int i;
    for (i=0; i < 8; ++i)
        if (stbi__get8(s) != png_sig[i]) return 0;
    return 1;
}
