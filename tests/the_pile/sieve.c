#include <stdio.h>

#define true 1
#define false 0
#define size 8190

char flags[size + 1];

int main() {
    int i,prime,k,count,iter;
    printf("10 iterations\n");
    for (iter = 1; iter <= 10; iter++) { /*do program 10 times*/
        count = 0; /*prime counter*/
        for (i = 0; i <= size; i++) /*set all flags true*/
            flags[i] = true;
        for (i = 0; i <= size; i++) {
            if(flags[i]) { /*found a prime*/
                prime = i + i + 3;
                printf("\n%d", prime);
                for (k = i + prime; k <= size; k += prime)
                    flags[k] = false;
                count++;
            }
        }
    }

    printf("\n%d primes.", count); /* primes found on 10th pass */
}