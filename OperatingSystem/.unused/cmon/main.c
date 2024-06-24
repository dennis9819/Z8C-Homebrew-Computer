#include<stdio.h>

int main() {
	printf("Hello World\n");
	return 0;
}

void initSystem () {
    __asm
    di
    
    __endasm;
}