

int x = 16;
int* y = &x;

int mainCRTStartup() {
    return *y;
}
