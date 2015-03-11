typedef unsigned char ovm_boolval_t;
typedef long long     ovm_intval_t;
typedef long double   ovm_floatval_t;
typedef unsigned long ovm_bmval_unit_t;
enum {
  OVM_BMVAL_UNIT_BITS_LOG2 = 5,
  OVM_BMVAL_UNIT_BITS      = 1 << OVM_BMVAL_UNIT_BITS_LOG2
};
struct ovm_strval {
  unsigned   size;
  const char *data;
};
struct ovm_bmval {
  unsigned               size;
  const ovm_bmval_unit_t *data;
};

enum {
  OVM_METHOD_CALL_SEL_ADD,
  OVM_METHOD_CALL_SEL_AND,
  OVM_METHOD_CALL_SEL_AT,
  OVM_METHOD_CALL_SEL_AT_LEN_PUT,
  OVM_METHOD_CALL_SEL_AT_PUT,
  OVM_METHOD_CALL_SEL_CNT,
  OVM_METHOD_CALL_SEL_DIV,
  OVM_METHOD_CALL_SEL_EQUAL,
  OVM_METHOD_CALL_SEL_HASH,
  OVM_METHOD_CALL_SEL_MULT,
  OVM_METHOD_CALL_SEL_NOT,
  OVM_METHOD_CALL_SEL_OR,
  OVM_METHOD_CALL_SEL_PUT,
  OVM_METHOD_CALL_SEL_SIZE,
  OVM_METHOD_CALL_SEL_SUB,
  OVM_METHOD_CALL_SEL_XOR,
  OVM_METHOD_CALL_NUM_SELS
};

struct ovm;
typedef struct ovm *ovm_t;

struct ovm_class;
typedef const struct ovm_class *ovm_class_t;

void ovm_init(ovm_t ovm, unsigned inst_page_size, void *glob, unsigned glob_size, void *stack, unsigned stack_size);

ovm_class_t ovm_inst_of(ovm_t ovm, unsigned src);
const char *ovm_class_name(ovm_class_t cl);
ovm_class_t ovm_class_parent(ovm_class_t cl);
unsigned    ovm_is_subclass_of(ovm_class_t cl1, ovm_class_t cl2);
unsigned    ovm_is_kind_of(ovm_t ovm, unsigned src, ovm_class_t cl);
void ovm_move(ovm_t ovm, unsigned dst, unsigned src);
void ovm_push(ovm_t ovm, unsigned src);
void ovm_pushm(ovm_t ovm, unsigned src, unsigned n);
void ovm_pop(ovm_t ovm, unsigned dst);
void ovm_popm(ovm_t ovm, unsigned dst, unsigned n);
void ovm_drop(ovm_t ovm);
void ovm_dropm(ovm_t ovm, unsigned n);
void *ovm_falloc(ovm_t ovm, unsigned n);
void ovm_ffree(ovm_t ovm, void *old);
void ovm_fload(ovm_t ovm, unsigned dst, int src);
void ovm_fstore(ovm_t ovm, int dst, unsigned src);
void ovm_gload(ovm_t ovm, unsigned dst, unsigned src);
void ovm_gstore(ovm_t ovm, unsigned dst, unsigned src);

#define OVM_NEW(_ovm, _dst, _cl, ...) \
  (_ovm_new((_ovm), (_dst), (_cl), sizeof((unsigned []) { 0, ## __VA_ARGS__ }) / sizeof(unsigned) - 1, ## __VA_ARGS__))
#define OVM_METHOD_CALL(_ovm, _dst, _recvr, _sel, ...) \
  (_ovm_method_call((_ovm), (_dst), (_recvr), 0, (_sel), sizeof((unsigned []) { 0, ## __VA_ARGS__ }) / sizeof(unsigned) - 1, ## __VA_ARGS__))
#define OVM_METHOD_CALL_CL(_ovm, _dst, _recvr, _cl, _sel, ...)		\
  (_ovm_method_call((_ovm), (_dst), (_recvr), (_cl), (_sel), sizeof((unsigned []) { 0, ## __VA_ARGS__ }) / sizeof(unsigned) - 1, ## __VA_ARGS__))

void ovm_boolean_newc(ovm_t ovm, unsigned dst, ovm_boolval_t val);
void ovm_integer_newc(ovm_t ovm, unsigned dst, ovm_intval_t val);
void ovm_float_newc(ovm_t ovm, unsigned dst, ovm_floatval_t val);
void ovm_string_newc(ovm_t ovm, unsigned dst, char *s);
void ovm_xml_newc(ovm_t ovm, unsigned dst, char *s);
void ovm_bitmap_newc(ovm_t ovm, unsigned dst, unsigned size, ovm_bmval_unit_t *data);

union ovm_cval {
  ovm_boolval_t     boolval;
  ovm_intval_t      intval;
  ovm_floatval_t    floatval;
  struct ovm_strval strval[1];
  struct ovm_bmval  bmval[1];
};
typedef union ovm_cval *ovm_cval_t, ovm_cval_var[1];

void ovm_cval_get(ovm_t ovm, ovm_cval_t cval, unsigned src);

void ovm_stats_print(ovm_t ovm);

#include "ovm_int.h"

typedef struct ovm ovm_var[1];

const struct ovm_class ovm_cl_object[1];
const struct ovm_class ovm_cl_boolean[1];
const struct ovm_class ovm_cl_integer[1];
const struct ovm_class ovm_cl_float[1];
const struct ovm_class ovm_cl_string[1];
const struct ovm_class ovm_cl_xml[1];
const struct ovm_class ovm_cl_bitmap[1];
const struct ovm_class ovm_cl_pair[1];
const struct ovm_class ovm_cl_list[1];
const struct ovm_class ovm_cl_array[1];
const struct ovm_class ovm_cl_set[1];
const struct ovm_class ovm_cl_dictionary[1];
