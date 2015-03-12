#include <stdio.h>

#include "ovm.h"

enum {
  OVM_INST_PAGE_SIZE = 4096
};

unsigned char globals[4096], stack[4096];

enum {
  GLOB_A, GLOB_B, GLOB_C
};

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
  ovm_init(ovm, OVM_INST_PAGE_SIZE, globals, sizeof(globals), stack, sizeof(stack));

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

#if 0

  ovm_integer_newc(ovm, R1, 42);
  ovm_string_newc(ovm, R2, "The rain in Spain");
  
  OVM_NEW(ovm, R3, ovm_cl_pair, R1, R2);

  inst_print(ovm, R3);

  OVM_NEW(ovm, R3, ovm_cl_list, R2, R0);
  OVM_NEW(ovm, R3, ovm_cl_list, R1, R3);

  inst_print(ovm, R3);

#endif

#if 0

  ovm_bmval_unit_t d[] = { 0x12345678, 0x9abcdef0, 0x11111111 };

  ovm_bitmap_newc(ovm, R1, 47, d);

  inst_print(ovm, R1);

  OVM_NEW(ovm, R2, ovm_cl_xml, R1);

  inst_print(ovm, R2);

#endif

#if 0

  ovm_integer_newc(ovm, R1, 10);

  OVM_NEW(ovm, R2, ovm_cl_array, R1);

  ovm_integer_newc(ovm, R3, 3);
  ovm_string_newc(ovm, R4, "The rain in Spain");

  OVM_METHOD_CALL(ovm, R0, R2, OVM_METHOD_CALL_SEL_AT_PUT, R3, R4);
  
  inst_print(ovm, R2);

#endif

#if 0

  OVM_NEW(ovm, R2, ovm_cl_set);

  ovm_integer_newc(ovm, R3, 42);
  ovm_string_newc(ovm, R4, "The rain in Spain");

  OVM_METHOD_CALL(ovm, R0, R2, OVM_METHOD_CALL_SEL_PUT, R3);
  OVM_METHOD_CALL(ovm, R0, R2, OVM_METHOD_CALL_SEL_PUT, R4);
  OVM_METHOD_CALL(ovm, R0, R2, OVM_METHOD_CALL_SEL_PUT, R3);
  
  inst_print(ovm, R2);

  OVM_METHOD_CALL(ovm, R0, R2, OVM_METHOD_CALL_SEL_DEL, R3);

  inst_print(ovm, R2);

#endif

#if 1

  enum {
    DICT, KEY, VAL
  };

  ovm_integer_newc(ovm, R1, 42);
  ovm_gstore(ovm, KEY, R1);

  ovm_string_newc(ovm, R1, "The rain in &Spain");
  ovm_gstore(ovm, VAL, R1);

  OVM_NEW(ovm, R2, ovm_cl_xml, R1);

  inst_print(ovm, R2);

  OVM_NEW(ovm, R1, ovm_cl_dictionary);
  ovm_gstore(ovm, DICT, R1);

  ovm_gload(ovm, R1, DICT);
  ovm_gload(ovm, R2, KEY);
  ovm_gload(ovm, R3, VAL);

  OVM_METHOD_CALL(ovm, R0, R1, OVM_METHOD_CALL_SEL_AT_PUT, R2, R3);
  
  inst_print(ovm, R1);

  OVM_METHOD_CALL(ovm, R0, R1, OVM_METHOD_CALL_SEL_DEL, R2);

  inst_print(ovm, R1);

#endif

  ovm_stats_print(ovm);

  return (0);
}
