#include "CBenchmark.h"

void bytewrite(unsigned char *a, int bytes) {
  unsigned char n = 0;
  int i = 0;
  int iterations = bytes;
  while (i < iterations) {
    a[i++] = n++;
  }
}

unsigned char byteread(unsigned char *a, int bytes) {
  unsigned char n = 0;
  int i = 0;
  int iterations = bytes;
  while (i < iterations) {
    n += a[i++];
  }
  return n;
}

void wordwrite(unsigned int *a, int bytes) {
  unsigned int n = 0;
  int i = 0;
  int iterations = bytes / sizeof(unsigned int) ;
  while (i < iterations) {
    a[i++] = n++;
  }
}

unsigned int wordread(unsigned int *a, int bytes) {
  unsigned int n = 0;
  int i = 0;
  int iterations = bytes / sizeof(unsigned int);
  while (i < iterations) {
    n += a[i++];
  }
  return n;
}
