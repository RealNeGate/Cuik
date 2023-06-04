#include <stdint.h>
#include <stdbool.h>

int isalnum(int ch) {
    switch (ch) {
        case 'a' ... 'z':
        case 'A' ... 'Z':
        case '0' ... '9': 
        return true;
        
        default:
        return false;
    }
}