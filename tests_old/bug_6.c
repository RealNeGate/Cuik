#include <stdio.h>
#include <stdlib.h>

typedef unsigned char stbi_uc;
typedef unsigned int   stbi__uint32;

typedef struct
{
    int      (*read)  (void *user,char *data,int size);   // fill 'data' with 'size' bytes.  return number of bytes actually read
    void     (*skip)  (void *user,int n);                 // skip the next 'n' bytes, or 'unget' the last -n bytes if negative
    int      (*eof)   (void *user);                       // returns nonzero if we are at end of file/data
} stbi_io_callbacks;

typedef struct
{
    stbi__uint32 img_x, img_y;
    int img_n, img_out_n;

    stbi_io_callbacks io;
    void *io_user_data;

    int read_from_callbacks;
    int buflen;
    stbi_uc buffer_start[128];
    int callback_already_read;

    stbi_uc *img_buffer, *img_buffer_end;
    stbi_uc *img_buffer_original, *img_buffer_original_end;
} stbi__context;

int main(void) {
    stbi__context* s = &(stbi__context){ 0 };

    snprintf(s->buffer_start, 128, "Woah!!!");

    s->img_buffer = s->buffer_start;
    s->img_buffer_end = s->buffer_start + n;

    printf("%s\n", s->img_buffer);
}
