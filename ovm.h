#include "ovm_cfg.h"

struct ovm;
typedef struct ovm *ovm_t;

enum {
  OVM_CL_OBJECT,
  OVM_CL_BOOLEAN,
  OVM_CL_NUMBER,
  OVM_CL_INTEGER,
  OVM_CL_FLOAT,
  OVM_CL_STRING,
  OVM_CL_XML,
  OVM_CL_BITMAP,
  OVM_CL_DPTR,
  OVM_CL_PAIR,
  OVM_CL_LIST,
  OVM_CL_ARRAY,
  OVM_CL_SET,
  OVM_CL_DICTIONARY,
  OVM_NUM_CLS
};

/**
 * @brief Method call selectors
 */
enum {
  OVM_METHOD_CALL_SEL_ADD,
  OVM_METHOD_CALL_SEL_AND,
  OVM_METHOD_CALL_SEL_AT,
  OVM_METHOD_CALL_SEL_AT_LEN,
  OVM_METHOD_CALL_SEL_AT_LEN_PUT,
  OVM_METHOD_CALL_SEL_AT_PUT,
  OVM_METHOD_CALL_SEL_CAR,
  OVM_METHOD_CALL_SEL_CDR,
  OVM_METHOD_CALL_SEL_CNT,
  OVM_METHOD_CALL_SEL_DEL,
  OVM_METHOD_CALL_SEL_DIV,
  OVM_METHOD_CALL_SEL_EQUAL,
  OVM_METHOD_CALL_SEL_HASH,
  OVM_METHOD_CALL_SEL_MULT,
  OVM_METHOD_CALL_SEL_NOT,
  OVM_METHOD_CALL_SEL_OR,
  OVM_METHOD_CALL_SEL_PARSE,
  OVM_METHOD_CALL_SEL_PUT,
  OVM_METHOD_CALL_SEL_SIZE,
  OVM_METHOD_CALL_SEL_SUB,
  OVM_METHOD_CALL_SEL_XOR,
  OVM_METHOD_CALL_NUM_SELS
};

/**
 * @brief Initialize a virtual machine
 *
 * @param[in] ovm            Virtual machine
 * @param[in] inst_page_size Size (in bytes) of an instance page
 * @param[in] glob           Start of global storage area
 * @param[in] glob_size      Size (in bytes) of global storage area
 * @param[in] stack          Start of stack storage area
 * @param[in] stack_size     Size (in bytes) of stack storage area
 */
void ovm_init(ovm_t ovm, unsigned inst_page_size, void *glob, unsigned glob_size, void *stack, unsigned stack_size);

/**
 * @brief Test if a register contains nil
 *
 * For a given VM register, return 1 if the register contains nil, otherwise return 0.
 *
 * @param[in] ovm Virtual machine
 * @param[in] src Source register
 *
 * @return 0 iff given register contains nil
 */
unsigned ovm_is_nil(ovm_t ovm, unsigned src);

/**
 * @brief Test if a register contains Boolean "true"
 *
 * For a given VM register, return 1 if the register contains the Boolean value "true", else return 0.
 *
 * @param[in] ovm Virtual machine
 * @param[in] src Source register
 *
 * @return 0 iff given register contains nil
 */
unsigned ovm_is_true(ovm_t ovm, unsigned src);

/**
 * @brief Get class of object
 *
 * Return the class of the object in the given VM register.
 *
 * @param[in] ovm Virtual machine
 * @param[in] src Source register
 *
 * @return Class of object in given register
 */
unsigned ovm_inst_of(ovm_t ovm, unsigned src);

/**
 * @brief Get name of class
 *
 * Return the printable name of the given class.
 *
 * @param[in] cl Class
 *
 * @return Pointer to name (character string) of class
 */
const char *ovm_class_name(unsigned cl);


unsigned ovm_class_parent(unsigned cl);
unsigned ovm_is_subclass_of(unsigned cl1, unsigned cl2);
unsigned ovm_is_kind_of(ovm_t ovm, unsigned src, unsigned cl);
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

/**
 * @brief Create a new instance
 *
 * @param[in] _ovm Virtual machine
 * @param[in] _dst Destination register
 * @param[in] _cl  Class of new instance
 *
 * @exception OVM_EXCEPT_BAD_VALUE Bad argument value
 */
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
void ovm_array_newc(ovm_t ovm, unsigned dst, unsigned size);

union ovm_cval {
  ovm_boolval_t     boolval;
  ovm_intval_t      intval;
  ovm_floatval_t    floatval;
  struct ovm_strval strval[1];
  struct ovm_bmval  bmval[1];
};
typedef union ovm_cval *ovm_cval_t, ovm_cval_var[1];

void ovm_cval_get(ovm_t ovm, ovm_cval_t cval, unsigned src);

#ifndef NDEBUG
void ovm_stats_print(ovm_t ovm);
#endif

#include "ovm_int.h"

typedef struct ovm ovm_var[1];

enum {
  OVM_EXCEPT_CODE_BAD_VALUE = 1, /**< Bad value for argument */
  OVM_EXCEPT_CODE_RANGE_ERR	 /**< Argument out of range  */
};

#define OVM_EXCEPT_FRAME_BEGIN(_ovm) \
{ \
  struct ovm_except_frame OVM_EXCEPT_FRAME_VAR(__LINE__) [1];  _ovm_except_frame_begin((_ovm), OVM_EXCEPT_FRAME_VAR(__LINE__)); \
  (_ovm)->xfp->code = setjmp((_ovm)->xfp->jmp_buf);

#define OVM_EXCEPT_TRY_BEGIN(_ovm) \
  if ((_ovm)->xfp->code == 0) {
 
#define OVM_EXCEPT_TRY_END(_ovm) \
  }

#define OVM_EXCEPT_CATCH_BEGIN(_ovm, _cond) \
  if ((_ovm)->xfp->code != 0 && !(_ovm)->xfp->caughtf && (_cond)) {
 
#define OVM_EXCEPT_CATCH_END(_ovm) \
    (_ovm)->xfp->caughtf = 1; \
  }
 
#define OVM_EXCEPT_FINALLY_BEGIN(_ovm) \
  if ((_ovm)->xfp->code == 0) {
 
#define OVM_EXCEPT_FINALLY_END(_ovm) \
  }
 
#define OVM_EXCEPT_FRAME_END(_ovm) \
  _ovm_except_frame_end(_ovm); \
}

#define OVM_EXCEPT_RAISE(_ovm, _code, _src) \
  (_ovm_except_raise((_ovm), (_code), (_src), __FILE__, __LINE__))

static inline int        ovm_except_code(ovm_t ovm);
static inline const char *ovm_except_file(ovm_t ovm);
static inline unsigned   ovm_except_line(ovm_t ovm);
void ovm_except_arg_get(ovm_t ovm, unsigned dst);
