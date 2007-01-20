#include <time.h>
#include <stdlib.h>
#include <stdio.h>

const int mb = 10;
const int bytes = 1024 * 1024 * 100;

void bytewrite(unsigned char *a);
unsigned char byteread(unsigned char *a);
void wordwrite(unsigned int *a);
unsigned int wordread(unsigned int *a);

int main() {
  unsigned char *a = malloc(bytes);

  bytewrite(a);
  //wordwrite((unsigned int *)a);
  //byteread(a);
  //wordread((unsigned int *)a);

  return 0;
}

void bytewrite(unsigned char *a) {
  unsigned char n = 0;
  int i = 0;
  int iterations = bytes;
  while (i < iterations) {
    a[i++] = n++;
  }
}

unsigned char byteread(unsigned char *a) {
  unsigned char n = 0;
  int i = 0;
  int iterations = bytes;
  while (i < iterations) {
    n += a[i++];
  }
  return n;
}

void wordwrite(unsigned int *a) {
  unsigned int n = 0;
  int i = 0;
  int iterations = bytes / sizeof(unsigned int) ;
  while (i < iterations) {
    a[i++] = n++;
  }
}

unsigned int wordread(unsigned int *a) {
  unsigned int n = 0;
  int i = 0;
  int iterations = bytes / sizeof(unsigned int);
  while (i < iterations) {
    n += a[i++];
  }
  return n;
}
