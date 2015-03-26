#include <setjmp.h>

struct list {
  struct list *prev, *next;
};

struct ovm_class;
struct ovm_inst_page;
struct ovm_inst;

typedef const struct ovm_class *ovm_class_t;
typedef struct ovm_inst  *ovm_inst_t;
typedef void (*ovm_method_call_t)(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv);

struct ovm_class {
  const char  *name;
  ovm_class_t parent;
  void (*new)(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv);
  void (*init)(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv);
  void (*walk)(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst));
  void (*free)(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl);
  ovm_method_call_t inst_method_tbl[OVM_METHOD_CALL_NUM_SELS];
};

struct ovm_inst_page {
  struct list list_node[1];
  unsigned in_use_cnt;
};

struct ovm_inst {
  struct list list_node[1];
  struct ovm_inst_page *page;
  ovm_class_t inst_of;
  unsigned    ref_cnt;
  union {
    ovm_boolval_t boolval;
#define BOOLVAL(x)  ((x)->val->boolval)
    ovm_intval_t  intval;
#define INTVAL(x)  ((x)->val->intval)
    ovm_floatval_t floatval;
#define FLOATVAL(x)  ((x)->val->floatval)
    struct ovm_strval strval[1];
#define STRVAL(x)  ((x)->val->strval)
    struct ovm_bmval bmval[1];
#define BMVAL(x)  ((x)->val->bmval)
    struct ovm_dptrval {
      ovm_inst_t car, cdr;
    } dptrval[1];
#define CAR(x)  ((x)->val->dptrval->car)
#define CDR(x)  ((x)->val->dptrval->cdr)
    struct ovm_arrayval {
      unsigned   size;
      ovm_inst_t *data;
    } arrayval[1];
#define ARRAYVAL(x)  ((x)->val->arrayval)
    struct ovm_setval {
      struct ovm_arrayval base[1];
      unsigned            cnt;
    } setval[1];
#define SETVAL(x)  ((x)->val->setval)
  } val[1];
};

struct ovm_except_frame;

struct ovm {
  struct list inst_page_list[1], inst_free_list[1];
  unsigned inst_page_size;
  unsigned insts_per_page;

  ovm_inst_t *glob;
  unsigned   glob_size;
  ovm_inst_t *stack, *stack_end;

  ovm_inst_t regs[OVM_NUM_REGS];
  ovm_inst_t *sp, *fp;
  ovm_inst_t except_arg;
  struct ovm_except_frame *xfp;

#ifndef NDEBUG
  struct {
    unsigned long long alloc_cnt, alloc_bytes;
    unsigned long long free_cnt, free_bytes;
    unsigned long long bytes_in_use, bytes_in_use_max;
    unsigned long long stack_depth, stack_depth_max;
  } stats[1];
#endif
};

struct ovm_except_frame {
  struct ovm_except_frame *prev;
  
  ovm_inst_t    *sp, *fp;
  int           code;
  ovm_inst_t    arg;
  const char    *file;
  unsigned      line;
  unsigned char caughtf;
  jmp_buf       jmp_buf;
};

void _ovm_except_frame_begin(ovm_t ovm, struct ovm_except_frame *xfr);
void _ovm_except_raise(ovm_t ovm, int code, unsigned src, char *file, unsigned line);
void _ovm_except_reraise(ovm_t ovm);
void _ovm_expcet_frame_end(ovm_t ovm);

static inline int
ovm_except_code(ovm_t ovm)
{
  return (ovm->xfp->code);
}

static inline const char *
ovm_except_file(ovm_t ovm)
{
  return (ovm->xfp->file);
}

static inline unsigned
ovm_except_line(ovm_t ovm)
{
  return (ovm->xfp->line);
}

#define OVM_CPP_CONCAT(x, y)  x ## y
#define OVM_EXCEPT_FRAME_VAR(x)  OVM_CPP_CONCAT(__ovm_xfr_, x)

void _ovm_new(ovm_t ovm, unsigned dst, unsigned cl, unsigned argc, ...);
void _ovm_method_call(ovm_t ovm, unsigned dst, unsigned recvr, unsigned sel, unsigned argc, ...);
void _ovm_method_call_cl(ovm_t ovm, unsigned dst, unsigned recvr, unsigned cl, unsigned sel, unsigned argc, ...);

