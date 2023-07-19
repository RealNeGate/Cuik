//#Bye, world!
#include <windows.h>

int mainCRTStartup(void) {
    char msg[] = "Bye, world!\n";

    DWORD x;
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteFile(stdout, msg, sizeof(msg), &x, NULL);
    return 0;
}
