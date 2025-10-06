
typedef struct {
    uint64_t gprs[16];
    uint8_t fxsave[512 + 16];
} CPUState;

typedef struct Sched_G {
    bool is_running;

    CPUState state;
    struct Sched_G* next;
} Sched_G;

// This thread runs goroutines
static void sched_g_main(void* arg) {

}

// This thread maps to an OS-thread, it's main job is just to schedule tasks
static void sched_n_main(void* arg) {
    for (;;) {

    }
}

static void checkpoint_handler(void) {

}
