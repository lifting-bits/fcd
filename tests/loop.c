// gcc -o test.out test.c
#include <stdio.h>

int main(void)
{
    unsigned a[6] = {1, 2, 3, 4, 5, 0};
    
    for (unsigned *b = a; *b != 0; b++) {
        printf("Variable at %px is ", b);
        if (*b % 2 == 0)
            printf("even.\n");
        else
            printf("odd.\n");
    }
    
    return 0;
}