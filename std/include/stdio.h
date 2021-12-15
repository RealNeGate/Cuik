#pragma once
#include <stddef.h>

#define _IOREAD         1
#define _IOWRT          2
#define _IORW           0x0080 /* opened as "r+w" */

#define STDIN_FILENO    0
#define STDOUT_FILENO   1
#define STDERR_FILENO   2

#define EOF	         (-1)

#define FILENAME_MAX	(260)
#define FOPEN_MAX	   (20)
#define TMP_MAX     	32767

#define _P_tmpdir       "\\"
#define _wP_tmpdir      L"\\"

#define L_tmpnam        (16)

#define _IOFBF		  0x0000
#define _IOLBF          0x0040
#define _IONBF          0x0004

#define BUFSIZ          512

#ifndef SEEK_SET
#define SEEK_SET        (0)
#endif

#ifndef SEEK_CUR
#define SEEK_CUR        (1)
#endif

#ifndef SEEK_END
#define SEEK_END        (2)
#endif

// This isn't actually what's inside but it's good 
// enough to trick the mortals
typedef struct FILE {
	int unused;
} FILE;

typedef long long fpos_t;

int fgetpos(FILE*, fpos_t*);
int fsetpos(FILE*, const fpos_t*);

// File Operations
FILE* fopen(const char*, const char*);
FILE* freopen(const char*, const char*, FILE*);
int fflush(FILE*);
int fclose(FILE*);

// MS puts remove & rename (but not wide versions) in io.h
int remove(const char*);
int rename(const char*, const char*);
FILE* tmpfile(void);
char* tmpnam(char*);
char* tempnam(const char*, const char*);

int setvbuf(FILE*, char*, int, size_t);
void setbuf(FILE*, char*);

// Formatted Output
int fprintf(FILE*, const char*, ...);
int printf(const char*, ...);
int sprintf(char*, const char*, ...);
int _snprintf(char*, size_t, const char*, ...);
int vfprintf(FILE*, const char*, va_list);
int vprintf(const char*, va_list);
int vsprintf(char*, const char*, va_list);
int _vsnprintf(char*, size_t, const char*, va_list);

// Formatted Input
int fscanf(FILE*, const char*, ...);
int scanf(const char*, ...);
int sscanf(const char*, const char*, ...);

// Character Input and Output Functions
int fgetc(FILE*);
char* fgets(char*, int, FILE*);
int fputc(int, FILE*);
int fputs(const char*, FILE*);
int getc(FILE*);
int getchar(void);
char* gets(char*);
int putc(int, FILE*);
int putchar(int);
int puts(const char*);
int ungetc(int, FILE*);

// Direct Input and Output Functions
size_t fread(void*, size_t, size_t, FILE*);
size_t fwrite(const void*, size_t, size_t, FILE*);

// File Positioning Functions
int fseek(FILE*, long, int);
long ftell(FILE*);
void rewind(FILE*);

// Errors
void clearerr (FILE*);
int feof (FILE*);
int ferror (FILE*);
void perror (const char*);