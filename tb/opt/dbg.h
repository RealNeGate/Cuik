// Integrated IR debugger
void tb_integrated_dbg(TB_Function* f, TB_Node* initial_n) {
    printf("Entering integrated debugger...\n");

    TB_Node* cursor = initial_n;
    char line[255];
    for (;;) {
        // read
        printf("> ");
        while (fgets(line, 255, stdin) == NULL) {}

        char* nl = strchr(line, '\n');
        if (nl) *nl = 0;

        if (strncmp(line, "in ", sizeof("in ")-1) == 0) {
            // move cursor to input
            int i = atoi(line + sizeof("in ") - 1);
            if (i < 0 || i >= cursor->input_count) {
                printf("error: index OoB (index=%d, limit=%d)\n", i, cursor->input_count);
                continue;
            }

            if (cursor->inputs[i] == NULL) {
                printf("error: no input edge at %%%u[%d]\n", cursor->gvn, i);
                continue;
            }

            printf("moving cursor %%%u to in[%d] -> %%%u\n", cursor->gvn, i, cursor->inputs[i]->gvn);
            cursor = cursor->inputs[i];
        } else if (strncmp(line, "out ", sizeof("out ")-1) == 0) {
            // move cursor to user
            int i = atoi(line + sizeof("out ") - 1);
            if (i < 0 || i >= cursor->user_count) {
                printf("error: index OoB (index=%d, limit=%d)\n", i, cursor->user_count);
                continue;
            }

            TB_User* u = &cursor->users[i];
            printf("moving cursor %%%u to out[%d] -> %%%u\n", cursor->gvn, i, USERN(u)->gvn);
            cursor = USERN(u);
        } else if (strcmp(line, "p") == 0) {
            // print cursor
            tb_print_dumb_node(NULL, cursor);
            printf("\n");
        } else if (strcmp(line, "users") == 0) {
            printf("printing users of %%%u:\n", cursor->gvn);
            int i = 0;
            FOR_USERS(u, cursor) {
                printf("  [%d] used by %-4d ", i, USERI(u));
                tb_print_dumb_node(NULL, USERN(u));
                printf("\n");
                i++;
            }
        } else if (strcmp(line, "debugger") == 0) {
            __builtin_debugtrap();
            break;
        } else if (strcmp(line, "pf") == 0) {
            tb_print_dumb(f);
        } else if (strcmp(line, "exit") == 0) {
            exit(1);
        } else {
            printf("error: unknown command: %s\n", line);
        }
    }
}

