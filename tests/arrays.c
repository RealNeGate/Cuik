#include <stdio.h>

int main(int argc, char** argv)
{
    char** str = { "22222", "AAAA" }; 
    printf("%c", (**++str));
}
