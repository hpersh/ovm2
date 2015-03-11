#include <stdio.h>

#include "ovm.h"

enum {
  OVM_INST_PAGE_SIZE = 4096
};

unsigned char glob[4096], stack[4096];

ovm_var ovm;


void
iadd(void)
{
  void *old;

  old = ovm_falloc(ovm, 0);

  ovm_push(ovm, R2);

  ovm_fload(ovm, R1, 0);
  ovm_fload(ovm, R2, 1);
  OVM_METHOD_CALL(ovm, R1, R1, OVM_METHOD_CALL_SEL_ADD, R2);

  ovm_pop(ovm, R2);

  ovm_ffree(ovm, old);
}

void
inst_print(ovm_t ovm, unsigned src)
{
  ovm_cval_var cv;

  ovm_push(ovm, src);

  OVM_NEW(ovm, src, ovm_cl_string, src);
  
  ovm_cval_get(ovm, cv, src);
  printf("%s\n", cv->strval->data);

  ovm_pop(ovm, src);
}

int
main(void)
{
  ovm_init(ovm, OVM_INST_PAGE_SIZE, glob, sizeof(glob), stack, sizeof(stack));

#if 0

  ovm_integer_newc(ovm, R1, 42);
  ovm_integer_newc(ovm, R2, 13);

  OVM_METHOD_CALL(ovm, R3, R1, OVM_METHOD_CALL_SEL_ADD, R2);

  OVM_NEW(ovm, R4, ovm_cl_integer, R3);

  ovm_boolean_newc(ovm, R5, 1);

  OVM_NEW(ovm, R6, ovm_cl_integer, R5);

  OVM_NEW(ovm, R7, ovm_cl_string, R3);

#endif

#if 0
  unsigned n;

  ovm_integer_newc(ovm, R1, 0);
  ovm_integer_newc(ovm, R2, 1);

  for (n = 1000000000; n; --n) {
    OVM_METHOD_CALL(ovm, R1, R1, OVM_METHOD_CALL_SEL_ADD, R2);
  }

#endif  

#if 0

  ovm_string_newc(ovm, R1, "The rain in Spain");
  ovm_string_newc(ovm, R1, "stays mainly in the plain");

#endif

#if 0

  ovm_integer_newc(ovm, R3, 42);
  ovm_push(ovm, R3);
  ovm_integer_newc(ovm, R3, 13);
  ovm_push(ovm, R3);

  iadd();

  ovm_dropm(ovm, 2);

  OVM_NEW(ovm, R1, ovm_cl_string, R1);
  
  inst_print(ovm, R1);

#endif

#if 1

  ovm_integer_newc(ovm, R1, 42);
  ovm_string_newc(ovm, R2, "The rain in Spain");
  
  OVM_NEW(ovm, R3, ovm_cl_pair, R1, R2);

  inst_print(ovm, R3);

  OVM_NEW(ovm, R3, ovm_cl_list, R2, R0);
  OVM_NEW(ovm, R3, ovm_cl_list, R1, R3);

  inst_print(ovm, R3);

#endif

  ovm_stats_print(ovm);

  return (0);
}
