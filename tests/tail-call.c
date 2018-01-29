// gcc -O1 -foptimize-sibling-calls -o tail-call.out tail-call.c
#include <stdlib.h>

unsigned char *a;

int f(char *a) {
    return atoi(a);
}

int main(void) {
    return f(a);   // tail call
}