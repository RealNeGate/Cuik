// Somewhat based on the code from:
//   https://github.com/SeaOfNodes/Simple/blob/main/chapter23/src/main/java/com/seaofnodes/simple/print/SimpleWebSocket.java
#include "../sandbird/sha1.c"

static sb_Stream* dbg_client;
static sb_Server* dbg_server;

static void write_bytes(sb_Stream* stream, const void* data, size_t len) {
    const char* bytes = data;
    uint8_t header[10];
    header[0] = 129; // FIN, opcode 1 - whole text message
    if (len <= 125) {
        header[1] = len;
        sb_write(stream, header, 2);
    } else if (len <= 65535) {
        header[1] = 126;
        header[2] = len >> 8;
        header[3] = len & 0xFF;
        sb_write(stream, header, 4);
    } else {
        header[1] = 127;
        header[2] = (len >> 56) & 0xFF;
        header[3] = (len >> 48) & 0xFF;
        header[4] = (len >> 40) & 0xFF;
        header[5] = (len >> 32) & 0xFF;
        header[6] = (len >> 24) & 0xFF;
        header[7] = (len >> 16) & 0xFF;
        header[8] = (len >> 8)  & 0xFF;
        header[9] = (len >> 0)  & 0xFF;
        sb_write(stream, header, 10);
    }
    sb_write(stream, data, len);
}

void dbg_submit_event(TB_Function* f, const char* desc, ...) {
    #if TB_OPTDEBUG_SERVER
    if (dbg_server == NULL) {
        return;
    }

    // if we're acting as a debug server, submit the latest copy of the
    // IR to the list of events. The viewer will organize the timeline
    // on it's end
    int t = f->dbg_server_t++;

    BufferOutStream s = bos_make();
    s.header.quoted = true;
    s_writef(&s.header, "{ \"type\":\"OPT\", \"name\":\"%s\", \"time\":%d, \"desc\":\"", f->super.name, t);

    va_list ap;
    va_start(ap, desc);
    s.header.writef(&s.header, desc, ap);
    va_end(ap);

    s_writef(&s.header, "\", \"content\":\"");
    tb_print_to_stream(f, &s.header);
    s_writef(&s.header, "\" }");

    // printf("%.*s\n", (int) s.cnt, s.data);

    write_bytes(dbg_client, s.data, s.cnt);
    sb_poll_server(dbg_server, 0);
    cuik_free(s.data);
    #endif
}

void dbg_submit_event_sched(TB_CFG* cfg, TB_Function* f, const char* desc, ...) {
    #if TB_OPTDEBUG_SERVER
    if (dbg_server == NULL) {
        return;
    }

    // if we're acting as a debug server, submit the latest copy of the
    // IR to the list of events. The viewer will organize the timeline
    // on it's end
    int t = f->dbg_server_t++;

    BufferOutStream s = bos_make();
    s.header.quoted = true;
    s_writef(&s.header, "{ \"type\":\"OPT\", \"name\":\"%s\", \"time\":%d, \"desc\":\"", f->super.name, t);

    va_list ap;
    va_start(ap, desc);
    s.header.writef(&s.header, desc, ap);
    va_end(ap);

    s_writef(&s.header, "\", \"content\":\"");
    aarray_for(i, cfg->blocks) {
        TB_BasicBlock* bb = &cfg->blocks[i];
        s_writef(&s.header, "BB %zu:\\n", i);
        aarray_for(j, bb->items) {
            s_writef(&s.header, "  ");
            tb_print_dumb_node_raw(NULL, bb->items[j], &s.header);
            s_writef(&s.header, "\\n");
        }
    }
    tb_print_to_stream(f, &s.header);
    s_writef(&s.header, "\" }");

    // printf("%.*s\n", (int) s.cnt, s.data);

    write_bytes(dbg_client, s.data, s.cnt);
    sb_poll_server(dbg_server, 0);
    cuik_free(s.data);
    #endif
}

static const char *level_strings[] = {
    "TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"
};

static const char *level_colors[] = {
    "gray", "lightblue", "green", "yellow", "red", "magenta"
};

static void punt_to_server(log_Event *ev) {
    char buf[1000];
    int len = snprintf(
        buf, sizeof(buf), "{ \"type\":\"LOG\", \"content\":\"Thread-%d %.4f s <span style=\\\"color: %s\\\">%-5s</span>: ",
        ev->tid, ev->time / 1000000.0, level_colors[ev->level], level_strings[ev->level]
    );
    len += vsnprintf(&buf[len], sizeof(buf) - len, ev->fmt, ev->ap);
    len += snprintf(&buf[len], sizeof(buf) - len, "\"}");

    TB_ASSERT(len < sizeof(buf));
    write_bytes(ev->udata, buf, len);
    sb_poll_server(dbg_server, 0);
}

static int event_handler(sb_Event *e) {
    if (e->type == SB_EV_CLOSE) {
        printf("Stream%p: closing...\n", e->stream);
    } else if (e->type == SB_EV_REQUEST) {
        printf("Stream%p: %s - %s %s\n", e->stream, e->address, e->method, e->path);

        if (strcmp(e->path, "/") == 0 && dbg_client == NULL) {
            static const char key_suffix[] = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
            static const char base64_alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

            // WebSocket connection!
            char key[24 + sizeof(key_suffix)];
            sb_get_header(e->stream, "Sec-WebSocket-Key", key, 25);
            memcpy(key + 24, key_suffix, sizeof(key_suffix));
            key[sizeof(key) - 1] = 0;

            char sha1_key[21];
            SHA1(sha1_key, key, sizeof(key) - 1);
            sha1_key[20] = 0;

            // convert 20B digest to Base-64
            char base64_key[29];
            int j = 0;
            FOR_N(i, 0, 7) {
                uint32_t triple = 0;
                triple |= ((uint8_t)sha1_key[i*3+0]) << 16;
                triple |= ((uint8_t)sha1_key[i*3+1]) << 8;
                triple |= ((uint8_t)sha1_key[i*3+2]);

                base64_key[j++] = base64_alphabet[(triple >> 3 * 6) & 0x3F];
                base64_key[j++] = base64_alphabet[(triple >> 2 * 6) & 0x3F];
                base64_key[j++] = base64_alphabet[(triple >> 1 * 6) & 0x3F];
                base64_key[j++] = base64_alphabet[(triple >> 0 * 6) & 0x3F];
            }
            base64_key[27] = '=';
            base64_key[28] = '\0';

            sb_send_status(e->stream, 101, "Switching Protocols");
            sb_send_header(e->stream, "Connection", "Upgrade");
            sb_send_header(e->stream, "Upgrade", "websocket");
            sb_send_header(e->stream, "Sec-WebSocket-Accept", base64_key);
            sb_write(e->stream, NULL, 0);

            log_add_callback(punt_to_server, e->stream, LOG_TRACE);
            dbg_client = e->stream;
        }
    }
    return SB_RES_OK;
}

static void dbg_close_server(void) {
    sb_close_server(dbg_server);
}

void dbg_startup_server(TB_Module* m) {
    sb_Options opt = { .port = "8000", .handler = event_handler };
    dbg_server = sb_new_server(&opt);
    if (!dbg_server) {
        fprintf(stderr, "failed to initialize server\n");
        exit(EXIT_FAILURE);
    }

    atexit(dbg_close_server);
    printf("Server running at http://localhost:%s\n", opt.port);

    // wait until we've connected, after that we only punt messages to the
    // debug viewer
    while (dbg_client == NULL) {
        sb_poll_server(dbg_server, 1000);
    }

    log_debug("Server has connected to client!");
}
