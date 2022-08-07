#pragma once

typedef struct {
    // implicitly converting between types and losing information
    bool data_loss : 1;
    bool unused_decls : 1;
    bool unused_funcs : 1;
} Warnings;

extern Warnings warnings;