#include <stdio.h>

unsigned a = 0;

int main(void)
{
    unsigned *b = &a;
    
    printf("Global variable 'a' of value %u at address %p is ", a, b);
    if (a % 2 == 0)
        printf("even.\n");
    else
        printf("odd.\n");
    
    return 0;
}