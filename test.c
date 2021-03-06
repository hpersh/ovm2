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

  ovm_push(ovm, OVM_R2);

  ovm_fload(ovm, OVM_R1, 0);
  ovm_fload(ovm, OVM_R2, 1);
  OVM_METHOD_CALL(ovm, OVM_R1, OVM_R1, OVM_METHOD_CALL_SEL_ADD, OVM_R2);

  ovm_pop(ovm, OVM_R2);

  ovm_ffree(ovm, old);
}

void
inst_print(ovm_t ovm, unsigned src)
{
  ovm_cval_var cv;

  ovm_push(ovm, src);

  OVM_NEW(ovm, src, OVM_CL_STRING, src);
  
  ovm_cval_get(ovm, cv, src);
  printf("%s\n", cv->strval->data);

  ovm_pop(ovm, src);
}


int
main(void)
{
  ovm_init(ovm, OVM_INST_PAGE_SIZE, globals, sizeof(globals), stack, sizeof(stack));

#if 0

  ovm_integer_newc(ovm, OVM_R1, 42);
  ovm_integer_newc(ovm, OVM_R2, 13);

  OVM_METHOD_CALL(ovm, OVM_R3, OVM_R1, OVM_METHOD_CALL_SEL_ADD, OVM_R2);

  OVM_NEW(ovm, OVM_R4, ovm_cl_integer, OVM_R3);

  ovm_boolean_newc(ovm, OVM_R5, 1);

  OVM_NEW(ovm, OVM_R6, ovm_cl_integer, OVM_R5);

  OVM_NEW(ovm, OVM_R7, ovm_cl_string, OVM_R3);

#endif

#if 0
  unsigned n;

  ovm_integer_newc(ovm, OVM_R1, 0);
  ovm_integer_newc(ovm, OVM_R2, 1);

  for (n = 1000000000; n; --n) {
    OVM_METHOD_CALL(ovm, OVM_R1, OVM_R1, OVM_METHOD_CALL_SEL_ADD, OVM_R2);
  }

#endif  

#if 0

  ovm_string_newc(ovm, OVM_R1, "The rain in Spain");
  ovm_string_newc(ovm, OVM_R1, "stays mainly in the plain");

#endif

#if 0

  ovm_integer_newc(ovm, OVM_R3, 42);
  ovm_push(ovm, OVM_R3);
  ovm_integer_newc(ovm, OVM_R3, 13);
  ovm_push(ovm, OVM_R3);

  iadd();

  ovm_dropm(ovm, 2);

  OVM_NEW(ovm, OVM_R1, ovm_cl_string, OVM_R1);
  
  inst_print(ovm, OVM_R1);

#endif

#if 0

  ovm_integer_newc(ovm, OVM_R1, 42);
  ovm_string_newc(ovm, OVM_R2, "The rain in Spain");
  
  OVM_NEW(ovm, OVM_R3, ovm_cl_pair, OVM_R1, OVM_R2);

  inst_print(ovm, OVM_R3);

  OVM_NEW(ovm, OVM_R3, ovm_cl_list, OVM_R2, OVM_R0);
  OVM_NEW(ovm, OVM_R3, ovm_cl_list, OVM_R1, OVM_R3);

  inst_print(ovm, OVM_R3);

#endif

#if 0

  ovm_bmval_unit_t d[] = { 0x12345678, 0x9abcdef0, 0x11111111 };

  ovm_bitmap_newc(ovm, OVM_R1, 47, d);

  inst_print(ovm, OVM_R1);

  OVM_NEW(ovm, OVM_R2, ovm_cl_xml, OVM_R1);

  inst_print(ovm, OVM_R2);

#endif

#if 0

  ovm_integer_newc(ovm, OVM_R1, 10);

  OVM_NEW(ovm, OVM_R2, ovm_cl_array, OVM_R1);

  ovm_integer_newc(ovm, OVM_R3, 3);
  ovm_string_newc(ovm, OVM_R4, "The rain in Spain");

  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R2, OVM_METHOD_CALL_SEL_AT_PUT, OVM_R3, OVM_R4);
  
  inst_print(ovm, OVM_R2);

#endif

#if 0

  OVM_NEW(ovm, OVM_R2, ovm_cl_set);

  ovm_integer_newc(ovm, OVM_R3, 42);
  ovm_string_newc(ovm, OVM_R4, "The rain in Spain");

  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R2, OVM_METHOD_CALL_SEL_PUT, OVM_R3);
  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R2, OVM_METHOD_CALL_SEL_PUT, OVM_R4);
  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R2, OVM_METHOD_CALL_SEL_PUT, OVM_R3);
  
  inst_print(ovm, OVM_R2);

  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R2, OVM_METHOD_CALL_SEL_DEL, OVM_R3);

  inst_print(ovm, OVM_R2);

#endif

#if 0

  enum {
    DICT, KEY, VAL
  };

  ovm_integer_newc(ovm, OVM_R1, 42);
  ovm_gstore(ovm, KEY, OVM_R1);

  ovm_string_newc(ovm, OVM_R1, "The rain in &Spain");
  ovm_gstore(ovm, VAL, OVM_R1);

  OVM_NEW(ovm, OVM_R2, ovm_cl_xml, OVM_R1);

  inst_print(ovm, OVM_R2);

  OVM_NEW(ovm, OVM_R1, ovm_cl_dictionary);
  ovm_gstore(ovm, DICT, OVM_R1);

  ovm_gload(ovm, OVM_R1, DICT);
  ovm_gload(ovm, OVM_R2, KEY);
  ovm_gload(ovm, OVM_R3, VAL);

  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R1, OVM_METHOD_CALL_SEL_AT_PUT, OVM_R2, OVM_R3);
  
  inst_print(ovm, OVM_R1);

  OVM_NEW(ovm, OVM_R4, ovm_cl_xml, OVM_R1);
  inst_print(ovm, OVM_R4);

  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R1, OVM_METHOD_CALL_SEL_DEL, OVM_R2);

  inst_print(ovm, OVM_R1);

#endif

#if 0

  ovm_xml_newc(ovm, OVM_R1, "  <Boolean>   1 </Boolean>     ");
  OVM_NEW(ovm, OVM_R2, ovm_cl_boolean, OVM_R1);
  inst_print(ovm, OVM_R2);

  ovm_xml_newc(ovm, OVM_R1, "  <Integer>   42 </Integer>     ");
  OVM_NEW(ovm, OVM_R2, ovm_cl_integer, OVM_R1);
  inst_print(ovm, OVM_R2);

#endif

#if 1
  
  ovm_xml_newc(ovm, OVM_R1, "  <List> <Boolean> 1  </Boolean><Integer>13</Integer></List> ");
  OVM_NEW(ovm, OVM_R2, OVM_CL_LIST, OVM_R1);
  inst_print(ovm, OVM_R2);

  ovm_xml_newc(ovm, OVM_R1, "  <Array> <Boolean> 1  </Boolean><Integer>13</Integer></Array> ");
  OVM_NEW(ovm, OVM_R2, OVM_CL_ARRAY, OVM_R1);
  inst_print(ovm, OVM_R2);

  ovm_array_newc(ovm, OVM_R2, 10);

  ovm_integer_newc(ovm, OVM_R1, 3);
  ovm_integer_newc(ovm, OVM_R3, 42);
  OVM_METHOD_CALL(ovm, OVM_R0, OVM_R2, OVM_METHOD_CALL_SEL_AT_PUT, OVM_R1, OVM_R3);

  inst_print(ovm, OVM_R2);

  OVM_NEW(ovm, OVM_R3, OVM_CL_XML, OVM_R2);
  inst_print(ovm, OVM_R3);

  OVM_NEW(ovm, OVM_R4, OVM_CL_ARRAY, OVM_R3);
  inst_print(ovm, OVM_R4);
  

#endif

#if 0

  void foo(void)
    {
      ovm_xml_newc(ovm, OVM_R1, "  <Boolean> x0  </Boolean>    ");
      OVM_NEW(ovm, OVM_R2, ovm_cl_boolean, OVM_R1);
      inst_print(ovm, OVM_R2);
    }



  OVM_EXCEPT_FRAME_BEGIN(ovm) {
    OVM_EXCEPT_TRY_BEGIN(ovm) {
      foo();
    } OVM_EXCEPT_TRY_END(ovm);

#if 0
    OVM_EXCEPT_CATCH_BEGIN(ovm, OVM_EXCEPT_CODE(ovm) == 1) {
      printf("Got bad value: ");
      ovm_except_arg_get(ovm, OVM_R1);
      inst_print(ovm, OVM_R1);
    } OVM_EXCEPT_CATCH_END(ovm);
#endif

    OVM_EXCEPT_FINALLY_BEGIN(ovm) {
      printf("No exception\n");
    } OVM_EXCEPT_FINALLY_END(ovm);

  } OVM_EXCEPT_FRAME_END(ovm);

#endif

#if 0
  void add(void)
  {
    void *old;

    old = ovm_falloc(ovm, 0);

    ovm_pushm(ovm, OVM_R1, 2);
    ovm_fload(ovm, OVM_R1, 0);
    ovm_fload(ovm, OVM_R2, 1);
    OVM_METHOD_CALL(ovm, OVM_R1, OVM_R1, OVM_METHOD_CALL_SEL_ADD, OVM_R2);
    ovm_drop(ovm);
    ovm_pop(ovm, OVM_R2);

    ovm_ffree(ovm, old);
  }

  ovm_integer_newc(ovm, OVM_R1, 13);
  ovm_integer_newc(ovm, OVM_R2, 42);
  ovm_push(ovm, OVM_R2);
  ovm_push(ovm, OVM_R1);
  add();
  ovm_dropm(ovm, 2);
  inst_print(ovm, OVM_R1);

#endif

#ifndef NDEBUG
  ovm_stats_print(ovm);
#endif

  return (0);
}
