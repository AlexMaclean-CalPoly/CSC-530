#include <stdio.h>

extern int optimizing(int i) asm("optimizing");


int main(int argc, char **argv) {
   int i, o, r;
   while (1) {
      printf("> ");
      r = scanf("%d", &i);
      if (r != 1)
         break;
      o = optimizing(i);
      printf("< %d\n", o);
   }
   return 0;
}
