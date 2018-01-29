// gcc -o funcptr.out funcptr.c
#include <stdio.h>
#include <string.h>

double cm_to_inches(double cm) {
	return cm / 2.54;
}

int main(void) {
	double (*func1)(double) = cm_to_inches;
	char * (*func2)(const char *, int) = strchr;
	printf("%f %s", func1(15.0), func2("Wikipedia", 'p'));
	return 0;
}