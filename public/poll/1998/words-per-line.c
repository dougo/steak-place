#include <stdio.h>

int main(int argc, char **argv) {
  register char c; register int n = 1;
  while ((c = getchar()) != EOF) {
    if (c == '\n') {
      printf("%d\n", n);
      n = 1;
    } else if (c == ' ') {
      n++;
    }
  }
  return 0;
}
