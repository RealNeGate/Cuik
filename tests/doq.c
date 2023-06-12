#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>


enum {
    PRE    = 1 << 0,
    CODE   = 1 << 1,
    EM     = 1 << 2,
    STRONG = 1 << 3,
    STRIKE = 1 << 4,
    H1     = 1 << 5,
    H2     = 1 << 6,
    H3     = 1 << 7,
    LIST   = 1 << 8,
    QUOTE  = 1 << 9,
};


static void panic(const char *msg) {
    fprintf(stderr, "error: %s\n", msg);
    exit(1);
}


static char* copy_until(char *dst, char *src, char *chars) {
    while (*src && !strchr(chars, *src)) {
        if (*src == '\\') { *dst++ = *src++; }
        *dst++ = *src++;
    }
    *dst = '\0';
    return src;
}


static bool consume(char **p, char *expect) {
    char *q = *p;
    while (*expect) {
        if (*q++ != *expect++) { return false; }
    }
    *p = q;
    return true;
}


static void write_fp(FILE *out, FILE *in) {
    int chr;
    while ((chr = fgetc(in)) != EOF) { fputc(chr, out); }
}


static void write_b64_fp(FILE *out, FILE *in) {
    static char t[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    int n;
    do {
        unsigned char b[3] = {0};
        n = fread(b, 1, 3, in);
        if (n == 0) { break; }
        unsigned x = (b[0] << 16) | (b[1] << 8) | b[2];
        fputc(        t[(x >> 18) & 0x3f],       out);
        fputc(        t[(x >> 12) & 0x3f],       out);
        fputc(n > 1 ? t[(x >>  6) & 0x3f] : '=', out);
        fputc(n > 2 ? t[(x >>  0) & 0x3f] : '=', out);
    } while (n == 3);
}


static int write_text(FILE *fp, char *text, int flags);

static int write_embedded(FILE *fp, char **p, int flags) {
    char name[512];
    *p = copy_until(name, *p, "\n :]*");
    FILE *in = fopen(name, "rb");
    if (in) {
        if (strstr(name, ".png") || strstr(name, ".jpg") || strstr(name, ".gif")) {
            fprintf(fp, "<img src=\"data:image;base64, ");
            write_b64_fp(fp, in);
            fprintf(fp, "\"/>");
        } else {
            write_fp(fp, in);
        }
        fclose(in);
        return flags;
    }
    fputc('@', fp);
    return write_text(fp, name, flags);
}


static int write_link(FILE *fp, char **p, int flags) {
    char text[512], url[512];
    *p = copy_until(text, *p, "]");
    if (consume(p, "](")) {
        *p = copy_until(url, *p, ")");
        consume(p, ")");
        fprintf(fp, "<a href=\""); write_text(fp, url, PRE); fprintf(fp, "\">");
        write_text(fp, text, flags);
        fprintf(fp, "</a>");
        return flags;
    }
    fputc('[', fp);
    return write_text(fp, text, flags);
}


static int edge(FILE *fp, int flags, int f, char *tag) {
    if (flags & f) {
        fprintf(fp, "</%s>", tag);
        return flags & ~f;
    }
    fprintf(fp, "<%s>", tag);
    return flags | f;
}


static int write_text(FILE *fp, char *text, int flags) {
    for (char *p = text;; p++) {
        top:
        if (~flags & PRE) {
            if (consume(&p,    "`")) { flags = edge(fp, flags, CODE,     "code"); goto top; }
            if (~flags & CODE) {
                if (consume(&p, "~~")) { flags = edge(fp, flags, STRIKE, "strike"); goto top; }
                if (consume(&p,  "*")) { flags = edge(fp, flags, EM,         "em"); goto top; }
                if (consume(&p,  "_")) { flags = edge(fp, flags, STRONG, "strong"); goto top; }
                if (consume(&p,  "@")) { flags = write_embedded(fp, &p, flags);     goto top; }
                if (consume(&p,  "[")) { flags = write_link(fp, &p, flags);         goto top; }
            }
        }

        if (*p == '\\') { p++; }
        switch (*p) {
            case '\0': return flags;
            case '<' : fprintf(fp, "&lt;");   break;
            case '>' : fprintf(fp, "&gt;");   break;
            case '&' : fprintf(fp, "&amp;");  break;
            case '"' : fprintf(fp, "&quot;"); break;
            case '\'': fprintf(fp, "&apos;"); break;
            default  : fputc(*p, fp);         break;
        }
    }
}


static int process_line(FILE *fp, char *line, int flags) {
    /* code block */
    if (consume(&line, "```")) { return edge(fp, flags, PRE, "pre"); }
    if (flags & PRE) { return write_text(fp, line, flags); }

    /* skip whitespace */
    while (isspace(*line)) { line++; }

    /* quote */
    if (consume(&line, ">")) {
        if (~flags & QUOTE) { flags = edge(fp, flags, QUOTE, "blockquote"); }
        while (isspace(*line)) { line++; }
    } else if (flags & QUOTE && !*line) {
        flags = edge(fp, flags, QUOTE, "blockquote");
    }

    /* list */
    if (consume(&line, "* ")) {
        if (~flags & LIST) { flags = edge(fp, flags, LIST, "ul"); }
        fprintf(fp, "<li>");
    } else if (flags & LIST && !*line) {
        flags = edge(fp, flags, LIST, "ul");
    }

    /* new paragraph */
    if (!*line) { fprintf(fp, "<p>"); }

    /* header */
    if (consume(&line,   "# ")) { flags = edge(fp, flags, H1, "h1"); }
    if (consume(&line,  "## ")) { flags = edge(fp, flags, H2, "h2"); }
    if (consume(&line, "### ")) { flags = edge(fp, flags, H3, "h3"); }

    /* write text */
    flags = write_text(fp, line, flags);

    /* finish header */
    if (flags & H1) { flags = edge(fp, flags, H1, "h1"); }
    if (flags & H2) { flags = edge(fp, flags, H2, "h2"); }
    if (flags & H3) { flags = edge(fp, flags, H3, "h3"); }

    return flags;
}


int main(int argc, char **argv) {
    FILE *css = NULL;
    FILE *in = stdin;

    for (int i = 1; i < argc; i++) {
        if (strstr(argv[i], ".css")) {
            css = fopen(argv[i], "rb");
            if (!css) { panic("failed to open .css file"); }
        } else {
            in = fopen(argv[i], "rb");
            if (!in) { panic("failed to open input file"); }
        }
    }

    fprintf(stdout, "<html><head><meta charset=\"utf-8\"><style>");
    if (css) {
        write_fp(stdout, css);
    } else {
        fprintf(stdout,
            "body{margin:60 auto;max-width:750px;line-height:1.6;"
            "font-family:Open Sans,Arial;color:#444;padding:0 10px;}"
            "h1,h2,h3{line-height:1.2;padding-top: 14px;}");
    }
    fprintf(stdout, "</style></head><body>");

    char line[4096];
    int flags = 0;
    while (fgets(line, sizeof(line), in)) {
        flags = process_line(stdout, line, flags);
    }

    fprintf(stdout, "</body></html>\n");
    return 0;
}
