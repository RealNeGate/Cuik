#include <stdio.h>

int main(void) {
  #if defined (__STDC__)
    #if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 201112L)  /* C11 */
      #if defined(__STDC_ANALYZABLE__)
        #if (__STDC_ANALYZABLE__ == 1)
          printf("Compiler conforms to Annex L (Analyzability).\n");
        #else
          printf("Compiler does not support Annex L (Analyzability).\n");
        #endif
      #else
        printf("__STDC_ANALYZABLE__ is not defined.\n");
      #endif
    #else
      printf("Compiler not C11.\n");
    #endif
  #else
    printf("Compiler not Standard C.\n");
  #endif
  
  return 0;
}
