#include <stdlib.h>

char c = 'A';
int r = 0;

int f(const char* a, const char* b){
  return atoi(a) - atoi(b);
}

int main(void){
  char d = 'B';
  r = f(&d, &c);
  return r;
}