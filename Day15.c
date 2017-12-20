#include "stdio.h"

int f(unsigned long a, unsigned long b) {
  int ret = 0;
  for(int i = 0; i<=40000000; i++) {
    a = (a * 16807) % 0x7fffffff;
    b = (b * 48271) % 0x7fffffff;
    if((a & 0xffff) == (b & 0xffff))
      ret++;
  }
  return ret;
}

int g(unsigned long a, unsigned long b) {
  int ret = 0;
  for(int i = 0; i<=5000000; i++) {
    do {
      a = (a * 16807) % 0x7fffffff;
    } while(a % 4);
    do {
      b = (b * 48271) % 0x7fffffff;
    } while(b % 8);
    if((a & 0xffff) == (b & 0xffff))
      ret++;
  }
  return ret;
}

int main(void) {
  printf("f(%d, %d) = %d\n", 65, 8921, f(65, 8921));
  printf("f(%d, %d) = %d\n", 512, 191, f(512, 191));
  printf("g(%d, %d) = %d\n", 65, 8921, g(65, 8921));
  printf("g(%d, %d) = %d\n", 512, 191, g(512, 191));
  return 0;
}
