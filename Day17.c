#include <stdio.h>

int spinlock(int n, int step) {
  int ret = 0;
  int idx = 0;
  for(int i=1; i<=n; i++) {
    idx = (idx + step) % i;
    if(idx == 0) ret = i;
    idx++;
  }
  return ret;
}

int main(void) {
  printf("spinlock(5, 3) = %d\n", spinlock(5, 3));
  printf("spinlock(50000000, 382) = %d\n", spinlock(50000000,382));
  return 0;
}
