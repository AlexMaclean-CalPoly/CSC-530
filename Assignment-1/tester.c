#include <stdio.h>
#include "expected.h"

extern int optimizing(int i) asm("optimizing");

int main(int argc, char **argv) {
   int passed = 1;

//   /* Conduct the basic probabilistic test */
//   int inputs[] = {-1000, 0, 456};
//   int outputs[] = {-1000, 0, 456};
//
//   for (int i = 0; i < 3; i++) {
//      if (optimizing(inputs[i]) != outputs[i]) {
//         passed = 0;
//         break;
//      }
//   }

   /* Conduct the exhaustive probabilistic test if needed */
   for (int i = -1000; i <= 1000; i++) {
      if (optimizing(i) != run_expected(i)) {
         passed = 0;
         break;
      }
   }

   printf("%d\n", passed);
   return 0;
}
