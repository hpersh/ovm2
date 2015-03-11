#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "ovm.h"

#define ARRAY_SIZE(a)  (sizeof(a) / sizeof((a)[0]))

#define PTR_TO_INT(x)    ((long long)(x))
#define FIELD_OFS(s, f)  PTR_TO_INT(&((s *) 0)->f)
#define FIELD_PTR_TO_STRUCT_PTR(p, s, f)  ((s *)((unsigned char *)(p) - FIELD_OFS(s, f)))

#define OVM_ASSERT(x)  assert(x)

#define REG_CHK(r)  OVM_ASSERT((r) < ARRAY_SIZE(ovm->regs))


static const unsigned crc32_tbl[] = {
	0x00000000, 0x77073096, 0xee0e612c, 0x990951ba, 0x076dc419, 0x706af48f,
	0xe963a535, 0x9e6495a3,	0x0edb8832, 0x79dcb8a4, 0xe0d5e91e, 0x97d2d988,
	0x09b64c2b, 0x7eb17cbd, 0xe7b82d07, 0x90bf1d91, 0x1db71064, 0x6ab020f2,
	0xf3b97148, 0x84be41de,	0x1adad47d, 0x6ddde4eb, 0xf4d4b551, 0x83d385c7,
	0x136c9856, 0x646ba8c0, 0xfd62f97a, 0x8a65c9ec,	0x14015c4f, 0x63066cd9,
	0xfa0f3d63, 0x8d080df5,	0x3b6e20c8, 0x4c69105e, 0xd56041e4, 0xa2677172,
	0x3c03e4d1, 0x4b04d447, 0xd20d85fd, 0xa50ab56b,	0x35b5a8fa, 0x42b2986c,
	0xdbbbc9d6, 0xacbcf940,	0x32d86ce3, 0x45df5c75, 0xdcd60dcf, 0xabd13d59,
	0x26d930ac, 0x51de003a, 0xc8d75180, 0xbfd06116, 0x21b4f4b5, 0x56b3c423,
	0xcfba9599, 0xb8bda50f, 0x2802b89e, 0x5f058808, 0xc60cd9b2, 0xb10be924,
	0x2f6f7c87, 0x58684c11, 0xc1611dab, 0xb6662d3d,	0x76dc4190, 0x01db7106,
	0x98d220bc, 0xefd5102a, 0x71b18589, 0x06b6b51f, 0x9fbfe4a5, 0xe8b8d433,
	0x7807c9a2, 0x0f00f934, 0x9609a88e, 0xe10e9818, 0x7f6a0dbb, 0x086d3d2d,
	0x91646c97, 0xe6635c01, 0x6b6b51f4, 0x1c6c6162, 0x856530d8, 0xf262004e,
	0x6c0695ed, 0x1b01a57b, 0x8208f4c1, 0xf50fc457, 0x65b0d9c6, 0x12b7e950,
	0x8bbeb8ea, 0xfcb9887c, 0x62dd1ddf, 0x15da2d49, 0x8cd37cf3, 0xfbd44c65,
	0x4db26158, 0x3ab551ce, 0xa3bc0074, 0xd4bb30e2, 0x4adfa541, 0x3dd895d7,
	0xa4d1c46d, 0xd3d6f4fb, 0x4369e96a, 0x346ed9fc, 0xad678846, 0xda60b8d0,
	0x44042d73, 0x33031de5, 0xaa0a4c5f, 0xdd0d7cc9, 0x5005713c, 0x270241aa,
	0xbe0b1010, 0xc90c2086, 0x5768b525, 0x206f85b3, 0xb966d409, 0xce61e49f,
	0x5edef90e, 0x29d9c998, 0xb0d09822, 0xc7d7a8b4, 0x59b33d17, 0x2eb40d81,
	0xb7bd5c3b, 0xc0ba6cad, 0xedb88320, 0x9abfb3b6, 0x03b6e20c, 0x74b1d29a,
	0xead54739, 0x9dd277af, 0x04db2615, 0x73dc1683, 0xe3630b12, 0x94643b84,
	0x0d6d6a3e, 0x7a6a5aa8, 0xe40ecf0b, 0x9309ff9d, 0x0a00ae27, 0x7d079eb1,
	0xf00f9344, 0x8708a3d2, 0x1e01f268, 0x6906c2fe, 0xf762575d, 0x806567cb,
	0x196c3671, 0x6e6b06e7, 0xfed41b76, 0x89d32be0, 0x10da7a5a, 0x67dd4acc,
	0xf9b9df6f, 0x8ebeeff9, 0x17b7be43, 0x60b08ed5, 0xd6d6a3e8, 0xa1d1937e,
	0x38d8c2c4, 0x4fdff252, 0xd1bb67f1, 0xa6bc5767, 0x3fb506dd, 0x48b2364b,
	0xd80d2bda, 0xaf0a1b4c, 0x36034af6, 0x41047a60, 0xdf60efc3, 0xa867df55,
	0x316e8eef, 0x4669be79, 0xcb61b38c, 0xbc66831a, 0x256fd2a0, 0x5268e236,
	0xcc0c7795, 0xbb0b4703, 0x220216b9, 0x5505262f, 0xc5ba3bbe, 0xb2bd0b28,
	0x2bb45a92, 0x5cb36a04, 0xc2d7ffa7, 0xb5d0cf31, 0x2cd99e8b, 0x5bdeae1d,
	0x9b64c2b0, 0xec63f226, 0x756aa39c, 0x026d930a, 0x9c0906a9, 0xeb0e363f,
	0x72076785, 0x05005713, 0x95bf4a82, 0xe2b87a14, 0x7bb12bae, 0x0cb61b38,
	0x92d28e9b, 0xe5d5be0d, 0x7cdcefb7, 0x0bdbdf21, 0x86d3d2d4, 0xf1d4e242,
	0x68ddb3f8, 0x1fda836e, 0x81be16cd, 0xf6b9265b, 0x6fb077e1, 0x18b74777,
	0x88085ae6, 0xff0f6a70, 0x66063bca, 0x11010b5c, 0x8f659eff, 0xf862ae69,
	0x616bffd3, 0x166ccf45, 0xa00ae278, 0xd70dd2ee, 0x4e048354, 0x3903b3c2,
	0xa7672661, 0xd06016f7, 0x4969474d, 0x3e6e77db, 0xaed16a4a, 0xd9d65adc,
	0x40df0b66, 0x37d83bf0, 0xa9bcae53, 0xdebb9ec5, 0x47b2cf7f, 0x30b5ffe9,
	0xbdbdf21c, 0xcabac28a, 0x53b39330, 0x24b4a3a6, 0xbad03605, 0xcdd70693,
	0x54de5729, 0x23d967bf, 0xb3667a2e, 0xc4614ab8, 0x5d681b02, 0x2a6f2b94,
	0xb40bbe37, 0xc30c8ea1, 0x5a05df1b, 0x2d02ef8d
};

static void
crc32_init(unsigned *r)
{
  *r = ~0;
}

static unsigned
crc32_sh(unsigned *r, void *buf, unsigned n)
{
  unsigned char *p = (unsigned char *) buf;

  for ( ; n; --n, ++p) {
    *r = crc32_tbl[(*r ^ *p) & 0xff] ^ (*r >> 8);
  }
}

static unsigned
crc32_get(unsigned *r)
{
  return (~*r);
}

static inline void
list_init(struct list *li)
{
  li->prev = li->next = li;
}

static inline struct list *
list_end(struct list *li)
{
  return (li);
}

static inline unsigned
list_empty(struct list *li)
{
  return (li->next == list_end(li));
}

static inline struct list *
list_first(struct list *li)
{
  return (li->next);
}

static inline struct list *
list_last(struct list *li)
{
  return (li->prev);
}

static inline struct list *
list_prev(struct list *nd)
{
  return (nd->prev);
}

static inline struct list *
list_next(struct list *nd)
{
  return (nd->next);
}

static inline struct list *
list_insert(struct list *node, struct list *before)
{
  struct list *p = before->prev;

  node->prev = p;
  node->next = before;

  return (p->next = before->prev = node);
}

static inline struct list *
list_erase(struct list *node)
{
  struct list *p = node->prev, *q = node->next;

  p->next = q;
  q->prev = p;

  return (node);
}



static inline ovm_class_t
_ovm_inst_of(ovm_inst_t inst)
{
  return (inst == 0 ? ovm_cl_object : inst->inst_of);
}

static unsigned
_ovm_is_kind_of(ovm_inst_t inst, ovm_class_t cl)
{
  return (ovm_is_subclass_of(_ovm_inst_of(inst), cl));
}

static void *
_ovm_malloc(ovm_t ovm, unsigned size)
{
  void *result = malloc(size);

  OVM_ASSERT(result != 0);

  ++ovm->stats->alloc_cnt;
  ovm->stats->alloc_bytes += size;
  if ((ovm->stats->bytes_in_use += size) > ovm->stats->bytes_in_use_max) {
    ovm->stats->bytes_in_use_max = ovm->stats->bytes_in_use;
  }

  return (result);
}

static void *
_ovm_zmalloc(ovm_t ovm, unsigned size)
{
  void *result = _ovm_malloc(ovm, size);

  memset(result, 0, size);

  return (result);
}

static void
_ovm_free(ovm_t ovm, unsigned size, void *p)
{
  if (p != 0)  free(p);

  ++ovm->stats->free_cnt;
  ovm->stats->free_bytes += size;
  ovm->stats->bytes_in_use -= size;
}

static ovm_inst_t
_ovm_inst_retain(ovm_inst_t inst)
{
  if (inst != 0) {
    ++inst->ref_cnt;
    OVM_ASSERT(inst->ref_cnt != 0);
  }

  return (inst);
}

static void _ovm_inst_free(ovm_t ovm, ovm_inst_t inst);

static void
_ovm_inst_release(ovm_t ovm, ovm_inst_t inst)
{
  if (inst == 0)  return;
  OVM_ASSERT(inst->ref_cnt != 0);
  if (--inst->ref_cnt == 0)  _ovm_inst_free(ovm, inst);
}

static void
__ovm_assign(ovm_t ovm, ovm_inst_t *dst, ovm_inst_t src)
{
  ovm_inst_t temp;

  temp = *dst;
  *dst = src;
  _ovm_inst_release(ovm, temp);
}

static void
_ovm_assign(ovm_t ovm, ovm_inst_t *dst, ovm_inst_t src)
{
  __ovm_assign(ovm, dst, _ovm_inst_retain(src));
}

#define OVM_CASSIGN(_ovm, _dst, _x) \
  do { if (_dst)  _ovm_assign((_ovm), (_dst), (_x)); } while (0)

static void
_ovm_inst_alloc(ovm_t ovm, ovm_class_t cl, ovm_inst_t *dst)
{
  struct list     *p;
  struct ovm_inst *r;

  if (list_empty(ovm->inst_free_list)) {
    struct ovm_inst_page *q;
    unsigned             k;

    q = (struct ovm_inst_page *) _ovm_malloc(ovm, ovm->inst_page_size);

    q->in_use_cnt = 0;
    list_insert(q->list_node, list_end(ovm->inst_page_list));

    for (r = (struct ovm_inst *)(q + 1), k = ovm->insts_per_page; k; --k, ++r) {
      r->page = q;
      list_insert(r->list_node, ovm->inst_free_list);
    }
  }

  p = list_first(ovm->inst_free_list);
  list_erase(p);

  r = FIELD_PTR_TO_STRUCT_PTR(p, struct ovm_inst, list_node);

  ++r->page->in_use_cnt;
  r->inst_of = cl;
  memset(r->val, 0, sizeof(r->val));
  
  _ovm_assign(ovm, dst, r);
}

static void
_ovm_inst_free(ovm_t ovm, ovm_inst_t inst)
{
  ovm_class_t cl = _ovm_inst_of(inst);

  (*cl->walk)(ovm, inst, cl, _ovm_inst_release);
  (*cl->free)(ovm, inst, cl);
}

static void
_ovm_new_run(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  void (*f)(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv) = 0;

  for ( ; cl; cl = cl->parent) {
    if (f = cl->new)  break;
  }

  OVM_ASSERT(f != 0);

  (*f)(ovm, dst, cl, argc, argv);
}

static void
__ovm_new(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ...)
{
  va_list    ap;
  ovm_inst_t argv[argc], *p = &argv[0];
  unsigned   k, r;

  va_start(ap, argc);

  for (k = argc; k; --k) {
    *p++ = va_arg(ap, ovm_inst_t);
  }

  va_end(ap);

  _ovm_new_run(ovm, dst, cl, argc, argv);
}

static void
_ovm_inst_new2(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  void *old;

  old = ovm_falloc(ovm, 1);

  _ovm_inst_alloc(ovm, cl, &ovm->fp[-1]);
  (*cl->init)(ovm, ovm->fp[-1], cl, argc, argv);
  _ovm_assign(ovm, dst, ovm->fp[-1]);

  ovm_ffree(ovm, old);
}

static void
_ovm_inst_new1(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc == 1 && _ovm_inst_of(argv[0]) == cl) {
    _ovm_assign(ovm, dst, argv[0]);
  } else {
    _ovm_inst_new2(ovm, dst, cl, argc, argv);
  }
}

static void
_ovm_method_run(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned sel, unsigned argc, ovm_inst_t *argv)
{
  ovm_method_call_t f = 0;
  
  for ( ; cl; cl = cl->parent) {
    if (f = cl->inst_method_tbl[sel])  break;
  }

  OVM_ASSERT(f != 0);

  (*f)(ovm, dst, argc, argv);
}

static void
__ovm_method_call(ovm_t ovm, ovm_inst_t *dst, ovm_inst_t recvr, unsigned sel, unsigned argc, ...)
{
  va_list    ap;
  ovm_inst_t argv[1 + argc], *p = &argv[0];
  unsigned   k;

  *p++ = recvr;

  va_start(ap, argc);

  for (k = argc; k; --k)  *p++ = va_arg(ap, ovm_inst_t);

  va_end(ap);

  _ovm_method_run(ovm, dst, _ovm_inst_of(argv[0]), sel, argc, argv);
}

static void
_ovm_init_parent(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  ovm_class_t p = cl->parent;

  (*p->init)(ovm, inst, p, argc, argv);
}

static void
_ovm_walk_parent(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
  ovm_class_t p = cl->parent;

  (*p->walk)(ovm, inst, p, func);
}

static void
_ovm_free_parent(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  ovm_class_t p = cl->parent;

  (*p->free)(ovm, inst, p);
}

/***************************************************************************/

static void
_ovm_object_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(argc == 0);
}

static void
_ovm_object_walk(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
}

static void
_ovm_object_free(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  struct ovm_inst_page *q = inst->page;

  list_insert(inst->list_node, ovm->inst_free_list);
  
  if (--q->in_use_cnt == 0) {
    ovm_inst_t r;
    unsigned   k;

    for (r = (struct ovm_inst *)(q + 1), k = ovm->insts_per_page; k; --k, ++r) {
      list_erase(r->list_node);
    }

    list_erase(q->list_node);

    _ovm_free(ovm, ovm->inst_page_size, q);
  }
}

const struct ovm_class ovm_cl_object[1] = {
  { .name = "Object",
    .init = _ovm_object_init,
    .walk = _ovm_object_walk,
    .free = _ovm_object_free
  }
};

/***************************************************************************/

static void _ovm_integer_newc(ovm_t ovm, ovm_inst_t *dst, ovm_intval_t val);

static void
_ovm_boolean_newc(ovm_t ovm, ovm_inst_t *dst, ovm_boolval_t val)
{
  _ovm_inst_alloc(ovm, ovm_cl_boolean, dst);
  BOOLVAL(*dst) = (val != 0);
}

static void
_ovm_boolean_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg    = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);
    ovm_boolval_t val   = 0;

    if (arg_cl == ovm_cl_integer) {
      val = (INTVAL(arg) != 0);
    } else {
      OVM_ASSERT(0);
    }

    BOOLVAL(inst) = val;

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

static void
_ovm_boolean_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_boolean));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_boolean));

  _ovm_boolean_newc(ovm, dst, (BOOLVAL(argv[0]) != 0) == (BOOLVAL(argv[1]) != 0));
}

static void
_ovm_boolean_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_boolean));
  OVM_ASSERT(argc == 0);

  _ovm_integer_newc(ovm, dst, BOOLVAL(argv[0]) != 0);
}

const struct ovm_class ovm_cl_boolean[1] = {
  { .name   = "Boolean",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_boolean_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_boolean_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_boolean_hash
    }
  }
};

/***************************************************************************/

static void
_ovm_integer_newc(ovm_t ovm, ovm_inst_t *dst, ovm_intval_t val)
{
  _ovm_inst_alloc(ovm, ovm_cl_integer, dst);
  INTVAL(*dst) = val;
}

static void
_ovm_integer_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg    = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);
    ovm_intval_t val   = 0;

    if (arg_cl == ovm_cl_boolean) {
      val = (BOOLVAL(arg) != 0);
    } else {
      OVM_ASSERT(0);
    }

    INTVAL(inst) = val;

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

static void
_ovm_integer_add(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_inst_of(argv[0]) == ovm_cl_integer);
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_inst_of(argv[1]) == ovm_cl_integer);

  if (dst == 0)  return;

  _ovm_integer_newc(ovm, dst, INTVAL(argv[0]) + INTVAL(argv[1]));
}

static void
_ovm_integer_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_integer));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_integer));

  _ovm_boolean_newc(ovm, dst, INTVAL(argv[0]) == INTVAL(argv[1]));
}

static void
_ovm_integer_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_integer));
  OVM_ASSERT(argc == 0);

  crc32_init(h);
  crc32_sh(h, (char *) &INTVAL(argv[0]), sizeof(INTVAL(argv[0])));
  
  _ovm_integer_newc(ovm, dst, crc32_get(h));
}

const struct ovm_class ovm_cl_integer[1] = {
  { .name   = "Integer",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_integer_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_ADD]   = _ovm_integer_add,
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_integer_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_integer_hash
    }
  }
};

/***************************************************************************/

static void
_ovm_strval_alloc(ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  STRVAL(inst)->size = size;
  STRVAL(inst)->data = _ovm_malloc(ovm, size);
}

static void
_ovm_strval_initc(ovm_t ovm, ovm_inst_t inst, unsigned argc, ...)
{
  va_list  ap;
  unsigned k, n, size;
  char     *p;

  va_start(ap, argc);

  for (size = 0, k = argc; k; --k) {
    size += va_arg(ap, unsigned) - 1;
    va_arg(ap, char *);
  }
  ++size;

  va_end(ap);

  _ovm_strval_alloc(ovm, inst, size);  

  va_start(ap, argc);

  for (p = (char *) STRVAL(inst)->data, k = argc; k; --k) {
    n = va_arg(ap, unsigned) - 1;
    memcpy(p, va_arg(ap, char *), n);
    p += n;
  }
  *p = 0;

  va_end(ap);
}

static void
_ovm_strval_initv(ovm_t ovm, ovm_inst_t inst, unsigned argc, struct ovm_strval *argv)
{
  unsigned          k, n, size;
  char              *p;
  struct ovm_strval *q;

  for (size = 0, q = argv, k = argc; k; --k, ++q) {
    size += q->size - 1;
  }
  ++size;

  _ovm_strval_alloc(ovm, inst, size);  

  for (p = (char *) STRVAL(inst)->data, q = argv, k = argc; k; --k, ++q) {
    n = q->size - 1;
    memcpy(p, q->data, n);
    p += n;
  }
  *p = 0;
}

static void
_ovm_strval_inita(ovm_t ovm, ovm_inst_t inst, unsigned argc, ovm_inst_t *argv)
{
  unsigned   k, n, size;
  char       *p;
  ovm_inst_t *q, r;

  for (size = 0, q = argv, k = argc; k; --k, ++q) {
    size += STRVAL(*q)->size - 1;
  }
  ++size;

  _ovm_strval_alloc(ovm, inst, size);  

  for (p = (char *) STRVAL(inst)->data, q = argv, k = argc; k; --k, ++q) {
    r = *q;
    n = STRVAL(r)->size - 1;
    memcpy(p, STRVAL(r)->data, n);
    p += n;
  }
  *p = 0;
}

static void
_ovm_string_newc(ovm_t ovm, ovm_inst_t *dst, char *s)
{
  _ovm_inst_alloc(ovm, ovm_cl_string, dst);
  _ovm_strval_initc(ovm, *dst, 1, strlen(s) + 1, s);
}

static unsigned _list_len(ovm_inst_t inst);

static void
_ovm_string_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg == 0) {
      _ovm_strval_initc(ovm, inst, 1, 4, "#nil");
    } else if (arg_cl == ovm_cl_boolean) {
      if (BOOLVAL(arg)) {
	_ovm_strval_initc(ovm, inst, 1, 5, "#true");
      } else {
	_ovm_strval_initc(ovm, inst, 1, 6, "#false");      
      }
    } else if (arg_cl == ovm_cl_integer) {
      char buf[32];

      snprintf(buf, sizeof(buf), "%lld", INTVAL(arg));
      _ovm_strval_initc(ovm, inst, 1, strlen(buf) + 1, buf);
    } else if (arg_cl == ovm_cl_float) {
      char buf[64];

      snprintf(buf, sizeof(buf), "%Lg", FLOATVAL(arg));
      _ovm_strval_initc(ovm, inst, 1, strlen(buf) + 1, buf);
    } else if (arg_cl == ovm_cl_xml) {
      _ovm_strval_initc(ovm, inst, 1, STRVAL(arg)->size, STRVAL(arg)->data);
    } else if (arg_cl == ovm_cl_bitmap) {
      unsigned nn = 2 + BMVAL(arg)->size + (BMVAL(arg)->size == 0 ? 0 : (BMVAL(arg)->size - 1) / 4) + 1;
      char buf[nn], *p;
      ovm_bmval_unit_t *q, u;
      unsigned i, j, k;

      p = &buf[nn];
      *--p = 0;
      for (i = 0, q = (ovm_bmval_unit_t *) BMVAL(arg)->data, j = BMVAL(arg)->size; j; ++q) {
	for (u = *q, k = OVM_BMVAL_UNIT_BITS; k != 0 && j != 0; --k, u >>= 1, --j, ++i) {
	  if (i > 0 && (i & 3) == 0) {
	    *--p = '_';
	  }
	  *--p = (u & 1) ? '1' : '0';
	}
      }
      *--p = 'b';
      *--p = '0';
      
      _ovm_strval_initc(ovm, inst, 1, nn, buf);
    } else if (arg_cl == ovm_cl_pair) {
      void *old;

      old = ovm_falloc(ovm, 5);

      _ovm_string_newc(ovm, &ovm->fp[-5], "<");
      __ovm_new(ovm, &ovm->fp[-4], ovm_cl_string, 1, CAR(arg));
      _ovm_string_newc(ovm, &ovm->fp[-3], ", ");
      __ovm_new(ovm, &ovm->fp[-2], ovm_cl_string, 1, CDR(arg));
      _ovm_string_newc(ovm, &ovm->fp[-1], ">");

      _ovm_strval_inita(ovm, inst, 5, &ovm->fp[-5]);

      ovm_ffree(ovm, old);
    } else if (arg_cl == ovm_cl_list) {
      unsigned n = _list_len(arg);
      unsigned nn = 1 + (n < 2 ? n : 2 * n - 1) + 1;
      void *old;
      unsigned i;
      ovm_inst_t p, *q, *d, *a;

      old = ovm_falloc(ovm, nn + 1);
      q = ovm->sp;

      _ovm_string_newc(ovm, q, ", ");
      d = q;
      ++q;

      _ovm_string_newc(ovm, q, "(");
      a = q;
      ++q;
      for (i = 0, p = arg; p; p = CDR(p), ++i) {
	if (i > 0) {
	  _ovm_assign(ovm, q, *d);
	  ++q;
	}

	__ovm_new(ovm, q, ovm_cl_string, 1, CAR(p));
	++q;
      }
      _ovm_string_newc(ovm, q, ")");

      _ovm_strval_inita(ovm, inst, nn, a);

      ovm_ffree(ovm, old);
    } else {
      OVM_ASSERT(0);
    } 

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

static void 
_ovm_string_free(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  _ovm_free(ovm, STRVAL(inst)->size, (void *) STRVAL(inst)->data);

  _ovm_free_parent(ovm, inst, cl);
}

static void
_ovm_string_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_string));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_string));

  _ovm_boolean_newc(ovm, dst,
		    STRVAL(argv[0])->size == STRVAL(argv[1])->size
		    && memcmp(STRVAL(argv[0])->data, STRVAL(argv[1])->data, STRVAL(argv[0])->size - 1) == 0
		    );
}

static void
_ovm_string_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_string));
  OVM_ASSERT(argc == 0);

  crc32_init(h);
  crc32_sh(h, (char *) STRVAL(argv[0])->data, STRVAL(argv[0])->size - 1);
  
  _ovm_integer_newc(ovm, dst, crc32_get(h));
}

const struct ovm_class ovm_cl_string[1] = {
  { .name   = "String",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_string_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_string_free,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_string_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_string_hash
    }
  }
};

/***************************************************************************/

static void
_ovm_xml_newc(ovm_t ovm, ovm_inst_t *dst, char *s)
{
  _ovm_inst_alloc(ovm, ovm_cl_xml, dst);
  _ovm_strval_initc(ovm, *dst, 1, strlen(s) + 1, s);
}

static void
_ovm_xml_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t  arg;
  ovm_class_t arg_cl;

  OVM_ASSERT(argc == 1);

  arg = argv[0];
  arg_cl = _ovm_inst_of(arg);

  if (arg == 0) {
    _ovm_strval_initc(ovm, inst, 1, 7, "<nil/>");
  } else if (arg_cl == ovm_cl_boolean) {
    char buf[21];

    snprintf(buf, sizeof(buf), "<Boolean>%c</Boolean>", BOOLVAL(arg) ? '1' : '0');
    _ovm_strval_initc(ovm, inst, 1, sizeof(buf), buf);
  } else if (arg_cl == ovm_cl_integer) {
    char buf[64];

    snprintf(buf, sizeof(buf), "<Integer>%lld</Integer>", INTVAL(arg));
    _ovm_strval_initc(ovm, inst, 1, strlen(buf) + 1, buf);
  } else if (arg_cl == ovm_cl_float) {
    char buf[96];

    snprintf(buf, sizeof(buf), "<Float>%Lg</Float>", FLOATVAL(arg));
    _ovm_strval_initc(ovm, inst, 1, strlen(buf) + 1, buf);
  } else if (arg_cl == ovm_cl_string) {
    unsigned nn, k;
    const char *p;

    for (nn = 0, p = STRVAL(arg)->data, k = STRVAL(arg)->size - 1; k; --k, ++p) {
      switch (*p) {
      case '\'':
      case '"':
	nn += 6;
	break;
      case '&':
	nn += 5;
	break;
      case '<':
      case '>':
	nn += 4;
	break;
      default:
	++nn;
      }
    }
    ++nn;
      
    {
      char buf[nn], c, *q;

      for (nn = 0, q = buf, p = STRVAL(arg)->data, k = STRVAL(arg)->size - 1; k; --k, ++p) {
	c = *p;
	switch (c) {
	case '\'':
	  strcpy(q, "&apos;");
	  q += 6;
	  break;
	case '"':
	  strcpy(q, "&quot;");
	  q += 6;
	  break;
	case '&':
	  strcpy(q, "&amp;");
	  q += 5;
	  break;
	case '<':
	  strcpy(q, "&lt;");
	  q += 4;
	  break;
	case '>':
	  strcpy(q, "&gtt;");
	  q += 4;
	  break;
	default:
	  *q++ = c;
	}
      }
      *q = 0;
	
      _ovm_strval_initc(ovm, inst, 1, nn, buf);
    }
  } else if (arg_cl == ovm_cl_bitmap) {
    unsigned nn = 8 + BMVAL(arg)->size + 9 + 1;
    char buf[nn], *p;
    ovm_bmval_unit_t *q, u;
    unsigned j, k;

    p = buf;
    strcpy(p, "<Bitmap>");
    p += 8;
    for (q = (ovm_bmval_unit_t *) BMVAL(arg)->data, j = BMVAL(arg)->size; j; ++q) {
      for (u = *q, k = OVM_BMVAL_UNIT_BITS; k != 0 && j != 0; --k, u >>= 1, --j) {
	*p++ = (u & 1) ? '1' : '0';
      }
    }
    strcpy(p, "</Bitmap>");
      
    _ovm_strval_initc(ovm, inst, 1, nn, buf);
  } else if (arg_cl == ovm_cl_pair) {
    void *old;

    old = ovm_falloc(ovm, 4);

    _ovm_string_newc(ovm, &ovm->fp[-4], "<Pair>");
    __ovm_new(ovm, &ovm->fp[-3], ovm_cl_xml, 1, CAR(arg));
    __ovm_new(ovm, &ovm->fp[-2], ovm_cl_xml, 1, CDR(arg));
    _ovm_string_newc(ovm, &ovm->fp[-1], "</Pair>");

    _ovm_strval_inita(ovm, inst, 4, &ovm->fp[-4]);

    ovm_ffree(ovm, old);
  } else if (arg_cl == ovm_cl_list) {
    unsigned n = _list_len(arg);
    unsigned nn = 1 + n + 1;
    void *old;
    ovm_inst_t p, *q;

    old = ovm_falloc(ovm, nn);
    q = ovm->sp;

    _ovm_string_newc(ovm, q, "<List>");
    ++q;

    for (p = arg; p; p = CDR(p)) {
      __ovm_new(ovm, q, ovm_cl_xml, 1, CAR(p));
      ++q;
    }
    _ovm_string_newc(ovm, q, "</List>");

    _ovm_strval_inita(ovm, inst, nn, ovm->sp);

    ovm_ffree(ovm, old);
  } else {
    OVM_ASSERT(0);
  } 

  --argc;  ++argv;

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

const struct ovm_class ovm_cl_xml[1] = {
  { .name   = "Xml",
    .parent = ovm_cl_string,
    .new    = _ovm_inst_new1,
    .init   = _ovm_xml_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
    }
  }
};

/***************************************************************************/

static ovm_bmval_unit_t
bmval_bit(unsigned sh)
{
  return (((ovm_bmval_unit_t) 1) << sh);
}

static ovm_bmval_unit_t
bmval_bits(unsigned n)
{
  return (n >= (OVM_BMVAL_UNIT_BITS - 1) ? (ovm_bmval_unit_t) -1 : bmval_bit(n) - 1);
}

static unsigned
bmap_unit_idx(unsigned b)
{
  return (b >> OVM_BMVAL_UNIT_BITS_LOG2);
}

static unsigned
bmap_unit_sh(unsigned b)
{
  return (b & (OVM_BMVAL_UNIT_BITS - 1));
}

static unsigned
bmap_bits_to_units(unsigned size)
{
  return (bmap_unit_idx(size - 1) + 1);
}

static unsigned
bmap_units_to_bytes(unsigned units)
{
  return (units * sizeof(ovm_bmval_unit_t));
}

static unsigned
bmap_bits_to_bytes(unsigned size)
{
  return (bmap_units_to_bytes(bmap_bits_to_units(size)));
}

static void
_ovm_bmval_alloc(ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  BMVAL(inst)->size = size;
  BMVAL(inst)->data = _ovm_malloc(ovm, bmap_bits_to_bytes(size));
}

static void
_ovm_bmval_clr(ovm_inst_t inst)
{
  memset((void *) BMVAL(inst)->data, 0, bmap_bits_to_bytes(BMVAL(inst)->size));
}

void
_ovm_bitmap_newc(ovm_t ovm, ovm_inst_t *dst, unsigned size, ovm_bmval_unit_t *data)
{
  unsigned n = bmap_bits_to_units(size);
  unsigned sh = bmap_unit_sh(size);
  ovm_bmval_unit_t *p;

  _ovm_inst_alloc(ovm, ovm_cl_bitmap, dst);
  _ovm_bmval_alloc(ovm, *dst, size);
  p = (ovm_bmval_unit_t *) BMVAL(*dst)->data;
  memcpy(p, data, bmap_units_to_bytes(n));
  if (sh != 0)  p[n - 1] &= bmval_bit(sh) - 1;
}

static void
_ovm_bmap_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg_cl == ovm_cl_integer) {
      OVM_ASSERT(INTVAL(arg) >= 0);

      _ovm_bmval_alloc(ovm, inst, INTVAL(arg));
      _ovm_bmval_clr(inst);
    } else {
      OVM_ASSERT(0);
    }

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

static void 
_ovm_bmap_free(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  _ovm_free(ovm, bmap_bits_to_bytes(BMVAL(inst)->size), (void *) BMVAL(inst)->data);

  _ovm_free_parent(ovm, inst, cl);
}

static void
_ovm_bmap_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_bitmap));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_bitmap));

  _ovm_boolean_newc(ovm, dst,
		    BMVAL(argv[0])->size == BMVAL(argv[1])->size
		    && memcmp(BMVAL(argv[0])->data, BMVAL(argv[1])->data, bmap_bits_to_bytes(BMVAL(argv[0])->size)) == 0
		    );			      
}

static void
_ovm_bmap_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_bitmap));
  OVM_ASSERT(argc == 0);

  crc32_init(h);
  crc32_sh(h, (void *) BMVAL(argv[0])->data, bmap_bits_to_bytes(BMVAL(argv[0])->size));

  _ovm_integer_newc(ovm, dst, crc32_get(h));
}

const struct ovm_class ovm_cl_bitmap[1] = {
  { .name   = "Bitmap",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_bmap_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_bmap_free,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_bmap_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_bmap_hash
    }
  }
};

/***************************************************************************/

static const struct ovm_class ovm_cl_dptr[1];

static void
_ovm_dptr_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(argc >= 2);

  _ovm_assign(ovm, &CAR(inst), argv[0]);
  _ovm_assign(ovm, &CDR(inst), argv[1]);

  argc -= 2;  argv += 2;

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

static void
_ovm_dptr_walk(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
  (*func)(ovm, CAR(inst));
  (*func)(ovm, CDR(inst));

  _ovm_walk_parent(ovm, inst, cl, func);
}

static void
_ovm_dptr_car(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 0);

  OVM_CASSIGN(ovm, dst, CAR(argv[0]));
}

static void
_ovm_dptr_cdr(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 0);

  OVM_CASSIGN(ovm, dst, CDR(argv[0]));
}

static void
_ovm_dptr_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  void     *old;
  unsigned f;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_dptr));

  old = ovm_falloc(ovm, 1);

  __ovm_method_call(ovm, &ovm->fp[-1], CAR(argv[0]), OVM_METHOD_CALL_SEL_EQUAL, 1, CAR(argv[1]));
  f = BOOLVAL(ovm->fp[-1]);
  if (f) {
    __ovm_method_call(ovm, &ovm->fp[-1], CDR(argv[0]), OVM_METHOD_CALL_SEL_EQUAL, 1, CDR(argv[1]));
    f = BOOLVAL(ovm->fp[-1]);
  }

  ovm_ffree(ovm, old);

  _ovm_boolean_newc(ovm, dst, f);
}

static void
_ovm_dptr_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  void     *old;
  unsigned h;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 0);

  old = ovm_falloc(ovm, 1);

  __ovm_method_call(ovm, &ovm->fp[-1], CAR(argv[0]), OVM_METHOD_CALL_SEL_HASH, 0);
  h = INTVAL(ovm->fp[-1]);
  __ovm_method_call(ovm, &ovm->fp[-1], CDR(argv[0]), OVM_METHOD_CALL_SEL_HASH, 0);
  h += INTVAL(ovm->fp[-1]);

  ovm_ffree(ovm, old);

  _ovm_integer_newc(ovm, dst, h);
}

static const struct ovm_class ovm_cl_dptr[1] = {
  { .name   = "Dptr",
    .parent = ovm_cl_object,
    .init   = _ovm_dptr_init,
    .walk   = _ovm_dptr_walk,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_CAR]   = _ovm_dptr_car,
      [OVM_METHOD_CALL_SEL_CDR]   = _ovm_dptr_cdr,
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_dptr_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_dptr_hash
    }
  }
};

/***************************************************************************/

const struct ovm_class ovm_cl_pair[1] = {
  { .name   = "Pair",
    .parent = ovm_cl_dptr,
    .new    = _ovm_inst_new1,
    .init   = _ovm_init_parent,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
    }
  }
};

/***************************************************************************/

static unsigned
_list_len(ovm_inst_t inst)
{
  unsigned result;

  for (result = 0; inst; inst = CDR(inst), ++result) {
    OVM_ASSERT(_ovm_inst_of(inst) == ovm_cl_list);
  }

  return (result);
}

static void
_ovm_list_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(argc >= 2);
  OVM_ASSERT(argv[1] == 0 || _ovm_inst_of(argv[1]) == ovm_cl_list);

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

static void
_ovm_list_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  void     *old;
  unsigned f;
  ovm_inst_t p, q;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_list));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_list));

  old = ovm_falloc(ovm, 1);

  for (f = 1, p = argv[0], q = argv[1]; p && q; p = CDR(p), q = CDR(q)) {
    __ovm_method_call(ovm, &ovm->fp[-1], CAR(p), OVM_METHOD_CALL_SEL_EQUAL, 1, CAR(q));
    f = BOOLVAL(ovm->fp[-1]);
    if (f == 0)  break;
  }
  if (p || q)  f = 0;

  ovm_ffree(ovm, old);

  _ovm_boolean_newc(ovm, dst, f);
}

static void
_ovm_list_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  void     *old;
  unsigned h;
  ovm_inst_t p;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_list));
  OVM_ASSERT(argc == 0);

  old = ovm_falloc(ovm, 1);

  for (h = 0, p = argv[0]; p; p = CDR(p)) {
    __ovm_method_call(ovm, &ovm->fp[-1], CAR(p), OVM_METHOD_CALL_SEL_HASH, 0);
    h += INTVAL(ovm->fp[-1]);
  }

  ovm_ffree(ovm, old);

  _ovm_integer_newc(ovm, dst, h);
}

const struct ovm_class ovm_cl_list[1] = {
  { .name   = "List",
    .parent = ovm_cl_dptr,
    .new    = _ovm_inst_new1,
    .init   = _ovm_list_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_list_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_list_hash
    }
  }
};

/***************************************************************************/

ovm_class_t
ovm_inst_of(ovm_t ovm, unsigned src)
{
  REG_CHK(src);

  return (_ovm_inst_of(ovm->regs[src]));
}

const char *
ovm_class_name(ovm_class_t cl)
{
  return (cl->name);
}

ovm_class_t
ovm_class_parent(ovm_class_t cl)
{
  return (cl->parent);
}

unsigned
ovm_is_subclass_of(ovm_class_t cl1, ovm_class_t cl2)
{
  for ( ; cl1; cl1 = cl1->parent) {
    if (cl1 == cl2)  return (1);
  }
  
  return (0);
}

unsigned
ovm_is_kind_of(ovm_t ovm, unsigned src, ovm_class_t cl)
{
  REG_CHK(src);

  return (_ovm_is_kind_of(ovm->regs[src], cl));
}

void
ovm_move(ovm_t ovm, unsigned dst, unsigned src)
{
  REG_CHK(dst);
  REG_CHK(src);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], ovm->regs[src]);
}

static void
_ovm_stk_stats_up(ovm_t ovm, unsigned n)
{
  if ((ovm->stats->stack_depth += n) > ovm->stats->stack_depth_max) {
    ovm->stats->stack_depth_max = ovm->stats->stack_depth;
  }
}

static void
_ovm_stk_stats_dn(ovm_t ovm, unsigned n)
{
  ovm->stats->stack_depth -= n;
}

void
ovm_push(ovm_t ovm, unsigned src)
{
  REG_CHK(src);
  OVM_ASSERT(ovm->sp > ovm->stack);

  *--ovm->sp = _ovm_inst_retain(ovm->regs[src]);

  _ovm_stk_stats_up(ovm, 1);
}

void
ovm_pushm(ovm_t ovm, unsigned src, unsigned n)
{
  ovm_inst_t *p, *q;
  unsigned   k;

  REG_CHK(src + n - 1);
  OVM_ASSERT((ovm->sp - n) >= ovm->stack);

  for (p = (ovm->sp -= n), q = &ovm->regs[src], k = n; k; --k, ++p, ++q) {
    *p = _ovm_inst_retain(*q);
  }

  _ovm_stk_stats_up(ovm, n);
}

void
ovm_pop(ovm_t ovm, unsigned dst)
{
  REG_CHK(dst);
  OVM_ASSERT(ovm->sp < ovm->stack_end);

  if (dst != 0) {
    __ovm_assign(ovm, &ovm->regs[dst], *ovm->sp++);
  } else {
    _ovm_inst_release(ovm, *ovm->sp++);
  }

  _ovm_stk_stats_dn(ovm, 1);
}

void
ovm_popm(ovm_t ovm, unsigned dst, unsigned n)
{
  ovm_inst_t *p, *q;
  unsigned   k;

  REG_CHK(dst + n - 1);
  OVM_ASSERT((ovm->sp + n) <= ovm->stack_end);

  for (p = ovm->sp, q = &ovm->regs[dst], k = n; k; --k, ++p, ++q, ++dst) {
    if (dst != 0) {
      __ovm_assign(ovm, q , *p);
    } else {
      _ovm_inst_release(ovm, *p);
    }
  }

  ovm->sp += n;

  _ovm_stk_stats_dn(ovm, n);
}

void
ovm_drop(ovm_t ovm)
{
  OVM_ASSERT(ovm->sp < ovm->stack_end);

  _ovm_inst_release(ovm, *ovm->sp++);

  _ovm_stk_stats_dn(ovm, 1);
}

void
ovm_dropm(ovm_t ovm, unsigned n)
{
  ovm_inst_t *p;
  unsigned   k;

  OVM_ASSERT((ovm->sp + n) <= ovm->stack_end);

  for (p = ovm->sp, k = n; k; --k, ++p) {
    _ovm_inst_release(ovm, *p);
  }

  ovm->sp += n;

  _ovm_stk_stats_dn(ovm, n);
}

void *
ovm_falloc(ovm_t ovm, unsigned n)
{
  void *result;

  OVM_ASSERT((ovm->sp - n) >= ovm->stack);

  result = ovm->fp;

  ovm->fp = ovm->sp;
  memset(ovm->sp -= n, 0, n * sizeof(*ovm->sp));

  _ovm_stk_stats_up(ovm, n);

  return (result);
}

void
ovm_ffree(ovm_t ovm, void *old)
{
  ovm_inst_t *p;
  unsigned   n;

  for (n = 0, p = ovm->sp; p < ovm->fp; ++p, ++n) {
    _ovm_inst_release(ovm, *p);
  }
  ovm->sp = p;

  ovm->fp = (ovm_inst_t *) old;  

  _ovm_stk_stats_dn(ovm, n);
}

void
ovm_fload(ovm_t ovm, unsigned dst, int src)
{
  ovm_inst_t *p;

  REG_CHK(dst);
  p = ovm->fp + src;
  OVM_ASSERT(p >= ovm->sp && p < ovm->stack_end);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], *p);
}

void
ovm_fstore(ovm_t ovm, int dst, unsigned src)
{
  ovm_inst_t *p;

  p = ovm->fp + dst;
  OVM_ASSERT(p >= ovm->sp && p < ovm->stack_end);
  REG_CHK(src);

  _ovm_assign(ovm, p, ovm->regs[src]);
}

void
ovm_gload(ovm_t ovm, unsigned dst, unsigned src)
{
  REG_CHK(dst);
  OVM_ASSERT(src < ovm->glob_size);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], ovm->glob[src]);
}

void
ovm_gstore(ovm_t ovm, unsigned dst, unsigned src)
{
  OVM_ASSERT(dst < ovm->glob_size);
  REG_CHK(src);
  
  _ovm_assign(ovm, &ovm->glob[dst], ovm->regs[src]);
}

void
_ovm_new(ovm_t ovm, unsigned dst, ovm_class_t cl, unsigned argc, ...)
{
  va_list    ap;
  ovm_inst_t argv[argc], *p = &argv[0];
  unsigned   k, r;

  REG_CHK(dst);

  va_start(ap, argc);

  for (k = argc; k; --k) {
    r = va_arg(ap, unsigned);
    REG_CHK(r);
    *p++ = ovm->regs[r];
  }

  va_end(ap);

  if (dst == 0)  return;

  _ovm_new_run(ovm, &ovm->regs[dst], cl, argc, argv);
}

void
_ovm_method_call(ovm_t ovm, unsigned dst, unsigned recvr, ovm_class_t cl, unsigned sel, unsigned argc, ...)
{
  va_list    ap;
  ovm_inst_t argv[1 + argc], *p = &argv[0];
  unsigned   k, r;

  REG_CHK(dst);
  
  REG_CHK(recvr);
  *p++ = ovm->regs[recvr];

  va_start(ap, argc);

  for (k = argc; k; --k) {
    r = va_arg(ap, unsigned);
    REG_CHK(r);
    *p++ = ovm->regs[r];
  }

  va_end(ap);

  _ovm_method_run(ovm, dst == 0 ? 0 : &ovm->regs[dst], cl == 0 ? _ovm_inst_of(argv[0]) : cl, sel, argc, argv);
}

void
ovm_boolean_newc(ovm_t ovm, unsigned dst, ovm_boolval_t val)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_boolean_newc(ovm, &ovm->regs[dst], val);
}

void
ovm_integer_newc(ovm_t ovm, unsigned dst, ovm_intval_t val)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_integer_newc(ovm, &ovm->regs[dst], val);
}

void
ovm_string_newc(ovm_t ovm, unsigned dst, char *s)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_string_newc(ovm, &ovm->regs[dst], s);
}

void
ovm_bitmap_newc(ovm_t ovm, unsigned dst, unsigned size, ovm_bmval_unit_t *data)
{
  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_bitmap_newc(ovm, &ovm->regs[dst], size, data);
}

void
ovm_cval_get(ovm_t ovm, ovm_cval_t cval, unsigned src)
{
  ovm_inst_t  inst;
  ovm_class_t cl;

  REG_CHK(src);

  inst = ovm->regs[src];
  cl = _ovm_inst_of(inst);

  if (cl == ovm_cl_boolean) {
    cval->boolval = BOOLVAL(inst);
  } else if (cl == ovm_cl_integer) {
    cval->intval = INTVAL(inst);
  } else if (cl == ovm_cl_string) {
    *cval->strval = *STRVAL(inst);
  } else {
    OVM_ASSERT(0);
  }
}

void
ovm_init(ovm_t ovm, unsigned inst_page_size, void *glob, unsigned glob_size, void *stack, unsigned stack_size)
{
  memset(ovm, 0, sizeof(*ovm));

  list_init(ovm->inst_page_list);
  list_init(ovm->inst_free_list);

  ovm->inst_page_size = inst_page_size;
  ovm->insts_per_page = (inst_page_size - sizeof(struct ovm_inst_page)) / sizeof(struct ovm_inst);
  OVM_ASSERT(ovm->insts_per_page > 0);

  memset(glob, 0, glob_size);
  ovm->glob = (ovm_inst_t *) glob;
  ovm->glob_size = glob_size / sizeof(ovm->glob[0]);

  ovm->stack = (ovm_inst_t *) stack;
  ovm->stack_end = ovm->stack + (stack_size / sizeof(ovm->stack[0]));
  ovm->sp = ovm->stack_end;
}

void
ovm_stats_print(ovm_t ovm)
{
  printf("\novm statistics:\n");

#define PRINT_STAT(f)  printf("%-20s%llu\n", #f ":", ovm->stats-> f)

  PRINT_STAT(alloc_cnt);
  PRINT_STAT(alloc_bytes);
  PRINT_STAT(free_cnt);
  PRINT_STAT(free_bytes);
  PRINT_STAT(bytes_in_use);
  PRINT_STAT(bytes_in_use_max);
  PRINT_STAT(stack_depth);
  PRINT_STAT(stack_depth_max);
}
