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

#define PUBLIC
#ifndef NDEBUG
#define PRIVATE
#else
#define PRIVATE static
#endif

PRIVATE const unsigned crc32_tbl[] = {
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

PRIVATE void
crc32_init(unsigned *r)
{
  *r = ~0;
}

PRIVATE unsigned
crc32_sh(unsigned *r, void *buf, unsigned n)
{
  unsigned char *p = (unsigned char *) buf;

  for ( ; n; --n, ++p) {
    *r = crc32_tbl[(*r ^ *p) & 0xff] ^ (*r >> 8);
  }
}

PRIVATE unsigned
crc32_get(unsigned *r)
{
  return (~*r);
}

PRIVATE inline void
list_init(struct list *li)
{
  li->prev = li->next = li;
}

PRIVATE inline struct list *
list_end(struct list *li)
{
  return (li);
}

PRIVATE inline unsigned
list_empty(struct list *li)
{
  return (li->next == list_end(li));
}

PRIVATE inline struct list *
list_first(struct list *li)
{
  return (li->next);
}

PRIVATE inline struct list *
list_last(struct list *li)
{
  return (li->prev);
}

PRIVATE inline struct list *
list_prev(struct list *nd)
{
  return (nd->prev);
}

PRIVATE inline struct list *
list_next(struct list *nd)
{
  return (nd->next);
}

PRIVATE inline struct list *
list_insert(struct list *node, struct list *before)
{
  struct list *p = before->prev;

  node->prev = p;
  node->next = before;

  return (p->next = before->prev = node);
}

PRIVATE inline struct list *
list_erase(struct list *node)
{
  struct list *p = node->prev, *q = node->next;

  p->next = q;
  q->prev = p;

  return (node);
}

PRIVATE int
slice(ovm_intval_t *ofs, ovm_intval_t *len, unsigned size)
{
  if (*ofs < 0)  *ofs = (ovm_intval_t) size + *ofs;
  if (*len < 0) {
    *ofs += *len;
    *len = -*len;
  }

  return (*ofs < 0 || (*ofs + *len) > size ? -1 : 0);
}

PRIVATE void
sv_init(struct ovm_strval *sv, unsigned size, const char *data)
{
  sv->size = size;
  sv->data = data;
}

PRIVATE unsigned
sv_eof(struct ovm_strval *sv)
{
  return (sv->size == 0);
}

PRIVATE void
sv_trim(struct ovm_strval *sv1, struct ovm_strval *sv2, unsigned n)
{
  n += sv2->size;

  OVM_ASSERT(n <= sv1->size);

  sv1->size -= n;
}

PRIVATE int
sv_getc(struct ovm_strval *sv)
{
  char c;

  if (sv_eof(sv))  return (-1);

  c = *sv->data;

  ++sv->data;  --sv->size;

  return (c);
}

PRIVATE unsigned
sv_strcmp(struct ovm_strval *sv, unsigned n, char *s)
{
  if (n > sv->size || memcmp(sv->data, s, n) != 0)  return (0);
  sv->data += n;
  sv->size -= n;
  return (1);
}

PRIVATE int
sv_ungetc(struct ovm_strval *sv)
{
  --sv->data;  ++sv->size;
}

PRIVATE void
sv_spskip(struct ovm_strval *sv)
{
  char c;

  for (;;) {
    if (sv_eof(sv) || !isspace(*sv->data))  return;
    ++sv->data;  --sv->size;
  }
}

PRIVATE int
_xml_end_tag_find(struct ovm_strval *sv, unsigned n, char *s)
{
  int      c;
  char     *t;
  unsigned k;

  for (;;) {
    c = sv_getc(sv);
    if (c < 0)  return (-1);
    if (c == '<') {
      if (sv_getc(sv) == '/') {
	return (!sv_strcmp(sv, n, s) || sv_getc(sv) != '>' ? -1 : 0);
      }

      for (t = (char *) sv->data - 1, k = 1;; ++k) {
	c = sv_getc(sv);
	if (c < 0)  return (-1);
	if (c == '/') {
	  c = sv_getc(sv);
	  if (c != '>')  return (-1);
	  break;
	}
	if (c == '>') {
	  if (_xml_end_tag_find(sv, k, t) < 0)  return (-1);
	  break;
	}
      }
    }
  }
}

PRIVATE inline ovm_class_t
_ovm_inst_of(ovm_inst_t inst)
{
  return (inst == 0 ? ovm_cl_object : inst->inst_of);
}

PRIVATE unsigned
_ovm_is_kind_of(ovm_inst_t inst, ovm_class_t cl)
{
  return (ovm_is_subclass_of(_ovm_inst_of(inst), cl));
}

PRIVATE void *
_ovm_malloc(ovm_t ovm, unsigned size)
{
  void *result = malloc(size);

  OVM_ASSERT(result != 0);

#ifndef NDEBUG
  ++ovm->stats->alloc_cnt;
  ovm->stats->alloc_bytes += size;
  if ((ovm->stats->bytes_in_use += size) > ovm->stats->bytes_in_use_max) {
    ovm->stats->bytes_in_use_max = ovm->stats->bytes_in_use;
  }
#endif

  return (result);
}

PRIVATE void *
_ovm_zmalloc(ovm_t ovm, unsigned size)
{
  void *result = _ovm_malloc(ovm, size);

  memset(result, 0, size);

  return (result);
}

PRIVATE void
_ovm_free(ovm_t ovm, unsigned size, void *p)
{
  if (p != 0)  free(p);

#ifndef NDEBUG
  ++ovm->stats->free_cnt;
  ovm->stats->free_bytes += size;
  ovm->stats->bytes_in_use -= size;
#endif
}

PRIVATE ovm_inst_t
_ovm_inst_retain(ovm_inst_t inst)
{
  if (inst != 0) {
    ++inst->ref_cnt;
    OVM_ASSERT(inst->ref_cnt != 0);
  }

  return (inst);
}

PRIVATE void _ovm_inst_free(ovm_t ovm, ovm_inst_t inst);

PRIVATE void
_ovm_inst_release(ovm_t ovm, ovm_inst_t inst)
{
  if (inst == 0)  return;
  OVM_ASSERT(inst->ref_cnt != 0);
  if (--inst->ref_cnt == 0)  _ovm_inst_free(ovm, inst);
}

PRIVATE void
__ovm_assign(ovm_t ovm, ovm_inst_t *dst, ovm_inst_t src)
{
  ovm_inst_t temp;

  temp = *dst;
  *dst = src;
  _ovm_inst_release(ovm, temp);
}

PRIVATE void
_ovm_assign(ovm_t ovm, ovm_inst_t *dst, ovm_inst_t src)
{
  __ovm_assign(ovm, dst, _ovm_inst_retain(src));
}

#define OVM_CASSIGN(_ovm, _dst, _x) \
  do { if (_dst)  _ovm_assign((_ovm), (_dst), (_x)); } while (0)

PRIVATE void
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

PRIVATE void
_ovm_inst_free(ovm_t ovm, ovm_inst_t inst)
{
  ovm_class_t cl = _ovm_inst_of(inst);

  (*cl->walk)(ovm, inst, cl, _ovm_inst_release);
  (*cl->free)(ovm, inst, cl);
}

#ifndef NDEBUG

PRIVATE void
_ovm_stk_stats_up(ovm_t ovm, unsigned n)
{
  if ((ovm->stats->stack_depth += n) > ovm->stats->stack_depth_max) {
    ovm->stats->stack_depth_max = ovm->stats->stack_depth;
  }
}

PRIVATE void
_ovm_stk_stats_dn(ovm_t ovm, unsigned n)
{
  ovm->stats->stack_depth -= n;
}

#endif

PRIVATE ovm_inst_t *
_ovm_falloc(ovm_t ovm, unsigned n, ovm_inst_t **old)
{
  OVM_ASSERT((ovm->sp - n) >= ovm->stack);

  *old = ovm->fp;

  ovm->fp = ovm->sp;
  memset(ovm->sp -= n, 0, n * sizeof(*ovm->sp));

#ifndef NDEBUG
  _ovm_stk_stats_up(ovm, n);
#endif

  return (ovm->sp);
}

PRIVATE void
_ovm_new_run(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  void (*f)(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv) = 0;

  for ( ; cl; cl = cl->parent) {
    if (f = cl->new)  break;
  }

  OVM_ASSERT(f != 0);

  (*f)(ovm, dst, cl, argc, argv);
}

PRIVATE void
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

PRIVATE void
_ovm_inst_new2(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t *wp, *old;

  wp = _ovm_falloc(ovm, 1, &old);

  _ovm_inst_alloc(ovm, cl, &wp[0]);
  (*cl->init)(ovm, wp[0], cl, argc, argv);
  _ovm_assign(ovm, dst, wp[0]);

  ovm_ffree(ovm, old);
}

PRIVATE void
_ovm_inst_new1(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc == 1 && _ovm_inst_of(argv[0]) == cl) {
    _ovm_assign(ovm, dst, argv[0]);
  } else {
    _ovm_inst_new2(ovm, dst, cl, argc, argv);
  }
}

PRIVATE void
_ovm_method_run(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned sel, unsigned argc, ovm_inst_t *argv)
{
  ovm_method_call_t f = 0;
  
  for ( ; cl; cl = cl->parent) {
    if (f = cl->inst_method_tbl[sel])  break;
  }

  OVM_ASSERT(f != 0);

  (*f)(ovm, dst, argc, argv);
}

PRIVATE void
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

PRIVATE void
_ovm_init_parent(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  ovm_class_t p = cl->parent;

  (*p->init)(ovm, inst, p, argc, argv);
}

PRIVATE void
_ovm_walk_parent(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
  ovm_class_t p = cl->parent;

  (*p->walk)(ovm, inst, p, func);
}

PRIVATE void
_ovm_free_parent(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  ovm_class_t p = cl->parent;

  (*p->free)(ovm, inst, p);
}

PRIVATE void
_ovm_except_uncaught(ovm_t ovm, int code, ovm_inst_t arg, const char *file, unsigned line)
{
  printf("\novm: ovm=%p Uncaught exception, code=%d arg=%p file=%s line=%u\n",
	 ovm, code, arg, file, line
	 );

  abort();
}

PRIVATE void
__ovm_except_raise(ovm_t ovm, int code, ovm_inst_t arg, char *file, unsigned line)
{
  struct ovm_except_frame *xfr = ovm->xfp;
  ovm_inst_t              *p;
  unsigned                n;

  if (xfr == 0)  _ovm_except_uncaught(ovm, code, arg, file, line);

  OVM_ASSERT(code != 0);

  for (n = 0, p = ovm->sp; p < xfr->sp; ++p, ++n) {
    _ovm_inst_release(ovm, *p);
  }
#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, n);
#endif
  ovm->fp = xfr->fp;

  _ovm_assign(ovm, &xfr->arg, arg);
  xfr->file = file;
  xfr->line = line;

  longjmp(xfr->jmp_buf, code);
}

/***************************************************************************/

PRIVATE void
_ovm_object_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(argc == 0);
}

PRIVATE void
_ovm_object_walk(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
}

PRIVATE void
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

PUBLIC const struct ovm_class ovm_cl_object[1] = {
  { .name = "Object",
    .init = _ovm_object_init,
    .walk = _ovm_object_walk,
    .free = _ovm_object_free
  }
};

/***************************************************************************/

PRIVATE void _ovm_integer_newc(ovm_t ovm, ovm_inst_t *dst, ovm_intval_t val);

PRIVATE void
_ovm_boolean_newc(ovm_t ovm, ovm_inst_t *dst, ovm_boolval_t val)
{
  _ovm_inst_alloc(ovm, ovm_cl_boolean, dst);
  BOOLVAL(*dst) = (val != 0);
}

PRIVATE int
_xml_parse_bool2(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t inst)
{
  int      c;
  unsigned val;
  
  sv_spskip(pb);

  c = sv_getc(pb);
  switch (c) {
  case '0':  val = 0;  break;
  case '1':  val = 1;  break;
  default:   return (-1);
  }

  sv_spskip(pb);
  if (!sv_strcmp(pb, 10, "</Boolean>"))  return (-1);

  BOOLVAL(inst) = val;

  return (0);
}

PRIVATE int
_xml_parse_bool(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t inst)
{
  sv_spskip(pb);
  return (!sv_strcmp(pb, 9, "<Boolean>") ? -1 : _xml_parse_bool2(ovm, pb, inst));
}

PRIVATE void
_ovm_boolean_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t  arg;
  ovm_class_t arg_cl;

  if (argc > 0) {
    arg    = argv[0];
    arg_cl = _ovm_inst_of(arg);

    if (arg_cl == ovm_cl_integer) {
      BOOLVAL(inst) = (INTVAL(arg) != 0);
    } else if (arg_cl == ovm_cl_xml) {
      struct ovm_strval pb[1];
      
      sv_init(pb, STRVAL(arg)->size - 1, STRVAL(arg)->data);
      if (_xml_parse_bool(ovm, pb, inst) < 0)  goto err;
      sv_spskip(pb);
      if (!sv_eof(pb))  goto err;
    } else {
      goto err;
    }

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);

  return;

 err:
  __ovm_except_raise(ovm, OVM_EXCEPT_CODE_BAD_VALUE, arg, __FILE__, __LINE__);
}

PRIVATE void
_ovm_boolean_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_boolean));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_boolean));

  if (dst == 0)  return;

  _ovm_boolean_newc(ovm, dst, (BOOLVAL(argv[0]) != 0) == (BOOLVAL(argv[1]) != 0));
}

PRIVATE void
_ovm_boolean_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_boolean));
  OVM_ASSERT(argc == 0);

  if (dst == 0)  return;

  _ovm_integer_newc(ovm, dst, BOOLVAL(argv[0]) != 0);
}

PUBLIC const struct ovm_class ovm_cl_boolean[1] = {
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

PUBLIC const struct ovm_class ovm_cl_number[1] = {
  { .name   = "Number",
    .parent = ovm_cl_object,
    .init   = _ovm_init_parent,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
  }
};

/***************************************************************************/

PRIVATE void
_ovm_integer_newc(ovm_t ovm, ovm_inst_t *dst, ovm_intval_t val)
{
  _ovm_inst_alloc(ovm, ovm_cl_integer, dst);
  INTVAL(*dst) = val;
}

PRIVATE int
_xml_parse_int2(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t inst)
{
  int      c;
  char     buf[32], *p;
  unsigned n;
  
  sv_spskip(pb);

  for (p = buf, n = 0;;) {
    c = sv_getc(pb);
    if (c < 0)  return (-1);
    if (c == '<') {
      sv_ungetc(pb);
      break;
    }
    if (isspace(c)) {
      sv_spskip(pb);
      break;
    }
    if (c == '-') {
      if (n > 0)  return (-1);
    } else if (c < '0' || c > '9') {
      return (-1);
    }
    if (n >= (sizeof(buf) - 1))  return (-1);
    *p = c;
    ++p;  ++n;
  }
  *p = 0;

  if (!sv_strcmp(pb, 10, "</Integer>"))  return (-1);

  return (sscanf(buf, "%lld", &INTVAL(inst)) == 1 ? 0 : -1);
}

PRIVATE int
_xml_parse_int(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t inst)
{
  sv_spskip(pb);
  return (!sv_strcmp(pb, 9, "<Integer>") ? -1 : _xml_parse_int2(ovm, pb, inst));
}
PRIVATE void
_ovm_integer_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg    = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg_cl == ovm_cl_boolean) {
      INTVAL(inst) = (BOOLVAL(arg) != 0);
    } else if (arg_cl == ovm_cl_float) {
      INTVAL(inst) = (ovm_intval_t) FLOATVAL(arg);
    } else if (arg_cl == ovm_cl_xml) {
      struct ovm_strval pb[1];
      
      sv_init(pb, STRVAL(arg)->size - 1, STRVAL(arg)->data);
      if (_xml_parse_int(ovm, pb, inst) < 0)  goto err;
      sv_spskip(pb);
      if (!sv_eof(pb))  goto err;
    } else {
      goto err;
    }

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);

  return;

 err:
  __ovm_except_raise(ovm, OVM_EXCEPT_CODE_BAD_VALUE, argv[0], __FILE__, __LINE__);
}

PRIVATE void
_ovm_integer_add(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t   arg;
  ovm_class_t  arg_cl;
  ovm_intval_t val;

  OVM_ASSERT(_ovm_inst_of(argv[0]) == ovm_cl_integer);
  OVM_ASSERT(argc == 1);
  arg    = argv[1];
  arg_cl = _ovm_inst_of(arg);
  OVM_ASSERT(ovm_is_subclass_of(arg_cl, ovm_cl_number));

  if (dst == 0)  return;

  val = INTVAL(argv[0]);
  if (arg_cl == ovm_cl_integer) {
    val += INTVAL(arg);
  } else if (arg_cl == ovm_cl_float) {
    val += (ovm_intval_t) FLOATVAL(arg);
  } else  OVM_ASSERT(0);

  _ovm_integer_newc(ovm, dst, val);
}

PRIVATE void
_ovm_integer_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_integer));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_integer));

  if (dst == 0)  return;

  _ovm_boolean_newc(ovm, dst, INTVAL(argv[0]) == INTVAL(argv[1]));
}

PRIVATE void
_ovm_integer_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_integer));
  OVM_ASSERT(argc == 0);

  if (dst == 0)  return;

  crc32_init(h);
  crc32_sh(h, (char *) &INTVAL(argv[0]), sizeof(INTVAL(argv[0])));
  
  _ovm_integer_newc(ovm, dst, crc32_get(h));
}

PUBLIC const struct ovm_class ovm_cl_integer[1] = {
  { .name   = "Integer",
    .parent = ovm_cl_number,
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

PRIVATE void
_ovm_strval_alloc(ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  STRVAL(inst)->size = size;
  STRVAL(inst)->data = _ovm_malloc(ovm, size);
}

PRIVATE void
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

PRIVATE void
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

PRIVATE void
_ovm_string_newc(ovm_t ovm, ovm_inst_t *dst, char *s)
{
  _ovm_inst_alloc(ovm, ovm_cl_string, dst);
  _ovm_strval_initc(ovm, *dst, 1, strlen(s) + 1, s);
}

PRIVATE unsigned _list_len(ovm_inst_t inst);

PRIVATE void
_ovm_string_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg == 0) {
      _ovm_strval_initc(ovm, inst, 1, 5, "#nil");
    } else if (arg_cl == ovm_cl_boolean) {
      if (BOOLVAL(arg)) {
	_ovm_strval_initc(ovm, inst, 1, 6, "#true");
      } else {
	_ovm_strval_initc(ovm, inst, 1, 7, "#false");      
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
      ovm_inst_t *wp, *old;

      wp = _ovm_falloc(ovm, 5, &old);

      _ovm_string_newc(ovm, &wp[0], "<");
      __ovm_new(ovm, &wp[1], ovm_cl_string, 1, CAR(arg));
      _ovm_string_newc(ovm, &wp[2], ", ");
      __ovm_new(ovm, &wp[3], ovm_cl_string, 1, CDR(arg));
      _ovm_string_newc(ovm, &wp[4], ">");

      _ovm_strval_inita(ovm, inst, 5, wp);

      ovm_ffree(ovm, old);
    } else if (arg_cl == ovm_cl_list) {
      unsigned n = _list_len(arg);
      unsigned nn = 1 + (n < 1 ? n : 2 * n - 1) + 1;
      unsigned i;
      ovm_inst_t *wp, *old, p, *q;

      wp = _ovm_falloc(ovm, nn + 1, &old);

      _ovm_string_newc(ovm, &wp[0], ", ");

      _ovm_string_newc(ovm, &wp[1], "(");
      for (i = 0, q = &wp[2], p = arg; p; p = CDR(p), ++i) {
	if (i > 0) {
	  _ovm_assign(ovm, q, wp[0]);
	  ++q;
	}

	__ovm_new(ovm, q, ovm_cl_string, 1, CAR(p));
	++q;
      }
      _ovm_string_newc(ovm, q, ")");

      _ovm_strval_inita(ovm, inst, nn, &wp[1]);

      ovm_ffree(ovm, old);
    } else if (arg_cl == ovm_cl_array) {
      unsigned n = ARRAYVAL(arg)->size;
      unsigned nn = 1 + (n < 1 ? n : 2 * n - 1) + 1;
      unsigned i;
      ovm_inst_t *wp, *old, *p, *q;

      wp = _ovm_falloc(ovm, nn + 1, &old);

      _ovm_string_newc(ovm, &wp[0], ", ");

      _ovm_string_newc(ovm, &wp[1], "[");
      for (i = 0, q = &wp[2], p = ARRAYVAL(arg)->data; n; --n, ++p, ++i) {
	if (i > 0) {
	  _ovm_assign(ovm, q, wp[0]);
	  ++q;
	}

	__ovm_new(ovm, q, ovm_cl_string, 1, *p);
	++q;
      }
      _ovm_string_newc(ovm, q, "]");

      _ovm_strval_inita(ovm, inst, nn, &wp[1]);

      ovm_ffree(ovm, old);
    } else if (arg_cl == ovm_cl_set) {
      unsigned n = SETVAL(arg)->cnt;
      unsigned nn = 1 + (n < 1 ? n : 2 * n - 1) + 1;
      unsigned i;
      ovm_inst_t *wp, *old, *p, *q, r;

      wp = _ovm_falloc(ovm, nn + 1, &old);

      _ovm_string_newc(ovm, &wp[0], ", ");

      _ovm_string_newc(ovm, &wp[1], "{");
      for (i = 0, q = &wp[2], p = SETVAL(arg)->base->data, n = SETVAL(arg)->base->size; n; --n, ++p) {
	for (r = *p; r; r = CDR(r), ++i) {
	  if (i > 0) {
	    _ovm_assign(ovm, q, wp[0]);
	    ++q;
	  }
	  
	  __ovm_new(ovm, q, ovm_cl_string, 1, CAR(r));
	  ++q;
	}
      }
      _ovm_string_newc(ovm, q, "}");

      _ovm_strval_inita(ovm, inst, nn, &wp[1]);

      ovm_ffree(ovm, old);
    } else if (arg_cl == ovm_cl_dictionary) {
      unsigned n = SETVAL(arg)->cnt;
      unsigned nn = 1 + (n < 1 ? n : 4 * n - 1) + 1;
      unsigned i;
      ovm_inst_t *wp, *old, *p, *q, r, s;

      wp = _ovm_falloc(ovm, nn + 2, &old);

      _ovm_string_newc(ovm, &wp[0], ": ");
      _ovm_string_newc(ovm, &wp[1], ", ");

      _ovm_string_newc(ovm, &wp[2], "{");
      for (i = 0, q = &wp[3], p = SETVAL(arg)->base->data, n = SETVAL(arg)->base->size; n; --n, ++p) {
	for (r = *p; r; r = CDR(r), ++i) {
	  if (i > 0) {
	    _ovm_assign(ovm, q, wp[1]);
	    ++q;
	  }
	  
	  s = CAR(r);
	  __ovm_new(ovm, q, ovm_cl_string, 1, CAR(s));
	  ++q;
	  _ovm_assign(ovm, q, wp[0]);
	  ++q;
	  __ovm_new(ovm, q, ovm_cl_string, 1, CDR(s));
	  ++q;
	}
      }
      _ovm_string_newc(ovm, q, "}");

      _ovm_strval_inita(ovm, inst, nn, &wp[2]);

      ovm_ffree(ovm, old);
    } else {
      OVM_ASSERT(0);
    } 

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

PRIVATE void 
_ovm_string_free(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  _ovm_free(ovm, STRVAL(inst)->size, (void *) STRVAL(inst)->data);

  _ovm_free_parent(ovm, inst, cl);
}

PRIVATE void
_ovm_string_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_string));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_string));

  if (dst == 0)  return;

  _ovm_boolean_newc(ovm, dst,
		    STRVAL(argv[0])->size == STRVAL(argv[1])->size
		    && memcmp(STRVAL(argv[0])->data, STRVAL(argv[1])->data, STRVAL(argv[0])->size - 1) == 0
		    );
}

PRIVATE void
_ovm_string_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_string));
  OVM_ASSERT(argc == 0);

  if (dst == 0)  return;

  crc32_init(h);
  crc32_sh(h, (char *) STRVAL(argv[0])->data, STRVAL(argv[0])->size - 1);
  
  _ovm_integer_newc(ovm, dst, crc32_get(h));
}

PUBLIC const struct ovm_class ovm_cl_string[1] = {
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

PRIVATE void
_ovm_xml_newc(ovm_t ovm, ovm_inst_t *dst, char *s)
{
  _ovm_inst_alloc(ovm, ovm_cl_xml, dst);
  _ovm_strval_initc(ovm, *dst, 1, strlen(s) + 1, s);
}

PRIVATE void
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
    nn += 8 + 9 + 1;
      
    {
      
      char buf[nn], c, *q;

      strcpy(buf, "<String>");
      for (q = buf + 8, p = STRVAL(arg)->data, k = STRVAL(arg)->size - 1; k; --k, ++p) {
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
      strcpy(q, "</String>");
	
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
    ovm_inst_t *wp, *old;

    wp = _ovm_falloc(ovm, 4, &old);

    _ovm_string_newc(ovm, &wp[0], "<Pair>");
    __ovm_new(ovm, &wp[1], ovm_cl_xml, 1, CAR(arg));
    __ovm_new(ovm, &wp[2], ovm_cl_xml, 1, CDR(arg));
    _ovm_string_newc(ovm, &wp[3], "</Pair>");

    _ovm_strval_inita(ovm, inst, 4, wp);

    ovm_ffree(ovm, old);
  } else if (arg_cl == ovm_cl_list) {
    unsigned n = _list_len(arg);
    unsigned nn = 1 + n + 1;
    ovm_inst_t *wp, *old, p, *q;

    wp = _ovm_falloc(ovm, nn, &old);

    q = wp;

    _ovm_string_newc(ovm, &wp[0], "<List>");

    for (q = &wp[1], p = arg; p; p = CDR(p)) {
      __ovm_new(ovm, q, ovm_cl_xml, 1, CAR(p));
      ++q;
    }
    _ovm_string_newc(ovm, q, "</List>");

    _ovm_strval_inita(ovm, inst, nn, wp);

    ovm_ffree(ovm, old);
  } else if (arg_cl == ovm_cl_array) {
    unsigned n = ARRAYVAL(arg)->size;
    unsigned nn = 1 + n + 1;
    ovm_inst_t *wp, *old, *p, *q;

    wp = _ovm_falloc(ovm, nn, &old);

    q = wp;

    _ovm_string_newc(ovm, &wp[0], "<Array>");

    for (q = &wp[1], p = ARRAYVAL(arg)->data; n; --n, ++p) {
      __ovm_new(ovm, q, ovm_cl_xml, 1, *p);
      ++q;
    }
    _ovm_string_newc(ovm, q, "</Array>");

    _ovm_strval_inita(ovm, inst, nn, wp);

    ovm_ffree(ovm, old);
  } else if (arg_cl == ovm_cl_set) {
    unsigned n = SETVAL(arg)->cnt;
    unsigned nn = 1 + n + 1;
    ovm_inst_t *wp, *old, *p, *q, r;

    wp = _ovm_falloc(ovm, nn, &old);

    q = wp;

    _ovm_string_newc(ovm, &wp[0], "<Set>");

    for (q = &wp[1], p = SETVAL(arg)->base->data, n = SETVAL(arg)->base->size; n; --n, ++p) {
      for (r = *p; r; r = CDR(r)) {
	__ovm_new(ovm, q, ovm_cl_xml, 1, CAR(r));
	++q;
      }
    }
    _ovm_string_newc(ovm, q, "</Set>");

    _ovm_strval_inita(ovm, inst, nn, wp);

    ovm_ffree(ovm, old);
  } else if (arg_cl == ovm_cl_dictionary) {
    unsigned n = SETVAL(arg)->cnt;
    unsigned nn = 1 + n + 1;
    ovm_inst_t *wp, *old, *p, *q, r;

    wp = _ovm_falloc(ovm, nn, &old);

    q = wp;

    _ovm_string_newc(ovm, &wp[0], "<Dictionary>");

    for (q = &wp[1], p = SETVAL(arg)->base->data, n = SETVAL(arg)->base->size; n; --n, ++p) {
      for (r = *p; r; r = CDR(r)) {
	__ovm_new(ovm, q, ovm_cl_xml, 1, CAR(r));
	++q;
      }
    }
    _ovm_string_newc(ovm, q, "</Dictionary>");

    _ovm_strval_inita(ovm, inst, nn, wp);

    ovm_ffree(ovm, old);
  } else {
    OVM_ASSERT(0);
  } 

  --argc;  ++argv;

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

PRIVATE int
__xml_parse(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t *dst, ovm_class_t cl, int (*func)(ovm_t, struct ovm_strval *, ovm_inst_t))
{
  int result;
  ovm_inst_t *wp, *old;
  
  wp = _ovm_falloc(ovm, 1, &old);
  
  _ovm_inst_alloc(ovm, cl, &wp[0]);
  result = (*func)(ovm, pb, wp[0]);
  if (result == 0)  _ovm_assign(ovm, dst, wp[0]);
  
  ovm_ffree(ovm, old);
  
  return (result);
}

PRIVATE int _xml_parse_list2(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t *dst);

PRIVATE int
_xml_parse(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t *dst)
{
  int  result;

  sv_spskip(pb);

  if (sv_strcmp(pb, 6, "<nil/>")) {
    _ovm_assign(ovm, dst, 0);
    return (0);
  }

  if (sv_strcmp(pb, 9, "<Boolean>")) {
    return (__xml_parse(ovm, pb, dst, ovm_cl_boolean, _xml_parse_bool2));
  }

  if (sv_strcmp(pb, 9, "<Integer>")) {
    return (__xml_parse(ovm, pb, dst, ovm_cl_integer, _xml_parse_int2));
  }

  if (sv_strcmp(pb, 6, "<List>")) {
    return (_xml_parse_list2(ovm, pb, dst));
  }

  return (-1);
}

PRIVATE void
_ovm_xml_parse(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  struct ovm_strval pb[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_xml));
  OVM_ASSERT(argc == 0);

  if (dst == 0)  return;

  sv_init(pb, STRVAL(argv[0])->size - 1, STRVAL(argv[0])->data);

  if (_xml_parse(ovm, pb, dst) < 0)  goto err;

  sv_spskip(pb);
  if (!sv_eof(pb))  goto err;

  return;

 err:
  __ovm_except_raise(ovm, OVM_EXCEPT_CODE_BAD_VALUE, argv[0], __FILE__, __LINE__);
}

PUBLIC const struct ovm_class ovm_cl_xml[1] = {
  { .name   = "Xml",
    .parent = ovm_cl_string,
    .new    = _ovm_inst_new1,
    .init   = _ovm_xml_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_PARSE] = _ovm_xml_parse
    }
  }
};

/***************************************************************************/

PRIVATE ovm_bmval_unit_t
bmval_bit(unsigned sh)
{
  return (((ovm_bmval_unit_t) 1) << sh);
}

PRIVATE ovm_bmval_unit_t
bmval_bits(unsigned n)
{
  return (n >= (OVM_BMVAL_UNIT_BITS - 1) ? (ovm_bmval_unit_t) -1 : bmval_bit(n) - 1);
}

PRIVATE unsigned
bmap_unit_idx(unsigned b)
{
  return (b >> OVM_BMVAL_UNIT_BITS_LOG2);
}

PRIVATE unsigned
bmap_unit_sh(unsigned b)
{
  return (b & (OVM_BMVAL_UNIT_BITS - 1));
}

PRIVATE unsigned
bmap_bits_to_units(unsigned size)
{
  return (bmap_unit_idx(size - 1) + 1);
}

PRIVATE unsigned
bmap_units_to_bytes(unsigned units)
{
  return (units * sizeof(ovm_bmval_unit_t));
}

PRIVATE unsigned
bmap_bits_to_bytes(unsigned size)
{
  return (bmap_units_to_bytes(bmap_bits_to_units(size)));
}

PRIVATE void
_ovm_bmval_alloc(ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  BMVAL(inst)->size = size;
  BMVAL(inst)->data = _ovm_malloc(ovm, bmap_bits_to_bytes(size));
}

PRIVATE void
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

PRIVATE void
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

PRIVATE void 
_ovm_bmap_free(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  _ovm_free(ovm, bmap_bits_to_bytes(BMVAL(inst)->size), (void *) BMVAL(inst)->data);

  _ovm_free_parent(ovm, inst, cl);
}

PRIVATE void
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

PRIVATE void
_ovm_bmap_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h[1];

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_bitmap));
  OVM_ASSERT(argc == 0);

  crc32_init(h);
  crc32_sh(h, (void *) BMVAL(argv[0])->data, bmap_bits_to_bytes(BMVAL(argv[0])->size));

  _ovm_integer_newc(ovm, dst, crc32_get(h));
}

PUBLIC const struct ovm_class ovm_cl_bitmap[1] = {
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

PRIVATE const struct ovm_class ovm_cl_dptr[1];

PRIVATE void
_ovm_dptr_newc(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, ovm_inst_t car, ovm_inst_t cdr)
{
  ovm_inst_t *wp, *old;

  wp = _ovm_falloc(ovm, 1, &old);

  _ovm_inst_alloc(ovm, cl, &wp[0]);
  _ovm_assign(ovm, &CAR(wp[0]), car);
  _ovm_assign(ovm, &CDR(wp[0]), cdr);

  _ovm_assign(ovm, dst, wp[0]);

  ovm_ffree(ovm, old);
}

PRIVATE void
_ovm_dptr_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc >= 2) {
    _ovm_assign(ovm, &CAR(inst), argv[0]);
    _ovm_assign(ovm, &CDR(inst), argv[1]);
    
    argc -= 2;  argv += 2;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

PRIVATE void
_ovm_dptr_walk(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
  (*func)(ovm, CAR(inst));
  (*func)(ovm, CDR(inst));

  _ovm_walk_parent(ovm, inst, cl, func);
}

PRIVATE void
_ovm_dptr_car(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 0);

  OVM_CASSIGN(ovm, dst, CAR(argv[0]));
}

PRIVATE void
_ovm_dptr_cdr(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 0);

  OVM_CASSIGN(ovm, dst, CDR(argv[0]));
}

PRIVATE void
_ovm_dptr_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned f;
  ovm_inst_t *wp, *old;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_dptr));

  wp = _ovm_falloc(ovm, 1, &old);

  __ovm_method_call(ovm, &wp[0], CAR(argv[0]), OVM_METHOD_CALL_SEL_EQUAL, 1, CAR(argv[1]));
  f = BOOLVAL(wp[0]);
  if (f) {
    __ovm_method_call(ovm, &wp[0], CDR(argv[0]), OVM_METHOD_CALL_SEL_EQUAL, 1, CDR(argv[1]));
    f = BOOLVAL(wp[0]);
  }

  ovm_ffree(ovm, old);

  _ovm_boolean_newc(ovm, dst, f);
}

PRIVATE void
_ovm_dptr_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h;
  ovm_inst_t *wp, *old;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dptr));
  OVM_ASSERT(argc == 0);

  wp = _ovm_falloc(ovm, 1, &old);

  __ovm_method_call(ovm, &wp[0], CAR(argv[0]), OVM_METHOD_CALL_SEL_HASH, 0);
  h = INTVAL(wp[0]);
  __ovm_method_call(ovm, &wp[0], CDR(argv[0]), OVM_METHOD_CALL_SEL_HASH, 0);
  h += INTVAL(wp[0]);

  ovm_ffree(ovm, old);

  _ovm_integer_newc(ovm, dst, h);
}

PRIVATE const struct ovm_class ovm_cl_dptr[1] = {
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

PUBLIC const struct ovm_class ovm_cl_pair[1] = {
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

PRIVATE unsigned
_list_len(ovm_inst_t inst)
{
  unsigned result;

  for (result = 0; inst; inst = CDR(inst), ++result) {
    OVM_ASSERT(_ovm_inst_of(inst) == ovm_cl_list);
  }

  return (result);
}

PRIVATE int
_xml_parse_list2(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t *dst)
{
  struct ovm_strval pb2[1];
  ovm_inst_t        *wp, *old, *p;

  *pb2 = *pb;

  if (_xml_end_tag_find(pb, 4, "List") < 0)  return (-1);

  sv_trim(pb2, pb, 7);

  wp = _ovm_falloc(ovm, 2, &old);

  for (p = wp;;) {
    sv_spskip(pb2);
    if (sv_eof(pb2))  break;

    if (_xml_parse(ovm, pb2, &wp[1]) < 0)  return (-1);
    _ovm_dptr_newc(ovm, p, ovm_cl_list, wp[1], 0);
    p = &CDR(*p);
  }

  _ovm_assign(ovm, dst, wp[0]);
  
  ovm_ffree(ovm, old);
  
  return (0);
}

PRIVATE int
_xml_parse_list(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t *dst)
{
  sv_spskip(pb);
  return (!sv_strcmp(pb, 6, "<List>") ? -1 : _xml_parse_list2(ovm, pb, dst));
}

PRIVATE void
_ovm_list_new(ovm_t ovm, ovm_inst_t *dst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t *wp, *old;

  if (argc == 1) {
    ovm_inst_t arg     = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg_cl == cl) {
      _ovm_assign(ovm, dst, arg);
      
      return;
    }
    if (arg_cl == ovm_cl_xml) {
      struct ovm_strval pb[1];

      sv_init(pb, STRVAL(arg)->size - 1, STRVAL(arg)->data);

      OVM_ASSERT(_xml_parse_list(ovm, pb, dst) == 0);

      sv_spskip(pb);
      OVM_ASSERT(sv_eof(pb));

      return;
    }

    OVM_ASSERT(0);
  }

  if (argc == 2) {
    OVM_ASSERT(argv[1] == 0 || _ovm_inst_of(argv[1]) == ovm_cl_list);
    
    wp = _ovm_falloc(ovm, 1, &old);
    
    _ovm_inst_alloc(ovm, cl, &wp[0]);
    _ovm_init_parent(ovm, wp[0], cl, argc, argv);
    _ovm_assign(ovm, dst, wp[0]);
    
    ovm_ffree(ovm, old);

    return;
  }

  OVM_ASSERT(0);
}

PRIVATE void
_ovm_list_equal(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned f;
  ovm_inst_t *wp, *old, p, q;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_list));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_list));

  wp = _ovm_falloc(ovm, 1, &old);

  for (f = 1, p = argv[0], q = argv[1]; p && q; p = CDR(p), q = CDR(q)) {
    __ovm_method_call(ovm, &wp[0], CAR(p), OVM_METHOD_CALL_SEL_EQUAL, 1, CAR(q));
    f = BOOLVAL(wp[0]);
    if (f == 0)  break;
  }
  if (p || q)  f = 0;

  ovm_ffree(ovm, old);

  _ovm_boolean_newc(ovm, dst, f);
}

PRIVATE void
_ovm_list_hash(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  unsigned h;
  ovm_inst_t *wp, *old, p;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_list));
  OVM_ASSERT(argc == 0);

  wp = _ovm_falloc(ovm, 1, &old);

  for (h = 0, p = argv[0]; p; p = CDR(p)) {
    __ovm_method_call(ovm, &wp[0], CAR(p), OVM_METHOD_CALL_SEL_HASH, 0);
    h += INTVAL(wp[0]);
  }

  ovm_ffree(ovm, old);

  _ovm_integer_newc(ovm, dst, h);
}

PUBLIC const struct ovm_class ovm_cl_list[1] = {
  { .name   = "List",
    .parent = ovm_cl_dptr,
    .new    = _ovm_list_new,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_EQUAL] = _ovm_list_equal,
      [OVM_METHOD_CALL_SEL_HASH]  = _ovm_list_hash
    }
  }
};

/***************************************************************************/

PRIVATE void
_ovm_arrayval_alloc(ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  unsigned n = size * sizeof(ARRAYVAL(inst)->data[0]);

  ARRAYVAL(inst)->size = size;
  ARRAYVAL(inst)->data = _ovm_malloc(ovm, n);
}

PRIVATE void
_ovm_arrayval_init(ovm_t ovm, ovm_inst_t inst, unsigned size)
{
  _ovm_arrayval_alloc(ovm, inst, size);
  memset(ARRAYVAL(inst)->data, 0, size * sizeof(ARRAYVAL(inst)->data[0]));
}

void
_ovm_array_newc(ovm_t ovm, ovm_inst_t *dst, unsigned size)
{
  _ovm_inst_alloc(ovm, ovm_cl_array, dst);
  _ovm_arrayval_init(ovm, *dst, size);
}

PRIVATE int
_xml_parse_array2(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t inst)
{
  int               result = -1;
  struct ovm_strval pb2[1], pb3[1];
  ovm_inst_t        *wp, *old, *p;
  unsigned          size;

  *pb2 = *pb;

  if (_xml_end_tag_find(pb, 5, "Array") < 0)  return (-1);

  sv_trim(pb2, pb, 8);

  *pb3 = *pb2;

  wp = _ovm_falloc(ovm, 1, &old);

  for (size = 0;; ++size) {
    sv_spskip(pb3);
    if (sv_eof(pb3))  break;

    if (_xml_parse(ovm, pb3, &wp[0]) < 0)  goto done;
  }

  _ovm_arrayval_alloc(ovm, inst, size);

  for (p = ARRAYVAL(inst)->data; ; ++p) {
    sv_spskip(pb2);
    if (sv_eof(pb2))  break;

    if (_xml_parse(ovm, pb2, &wp[0]) < 0)  goto done;
    *p = _ovm_inst_retain(wp[0]);
  }

  result = 0;

 done:  
  ovm_ffree(ovm, old);
  
  return (result);
}

PRIVATE int
_xml_parse_array(ovm_t ovm, struct ovm_strval *pb, ovm_inst_t inst)
{
  sv_spskip(pb);
  return (!sv_strcmp(pb, 7, "<Array>") ? -1 : _xml_parse_array2(ovm, pb, inst));
}

PRIVATE void
_ovm_array_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc > 0) {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg_cl == ovm_cl_integer) {
      OVM_ASSERT(INTVAL(arg) >= 0);

      _ovm_arrayval_init(ovm, inst, INTVAL(arg));
    } else if (arg_cl == ovm_cl_xml) {
      struct ovm_strval pb[1];

      sv_init(pb, STRVAL(arg)->size - 1, STRVAL(arg)->data);
      OVM_ASSERT(_xml_parse_array(ovm, pb, inst) == 0);
      sv_spskip(pb);
      OVM_ASSERT(sv_eof(pb));
    } else if (arg_cl == ovm_cl_list) {
      unsigned   n = _list_len(arg);
      ovm_inst_t *p, q;

      _ovm_arrayval_alloc(ovm, inst, n);

      for (p = ARRAYVAL(inst)->data, q = arg; q; q = CDR(q), ++p) {
	*p = _ovm_inst_retain(CAR(q));
      }
    } else if (arg_cl == ovm_cl_array) {
      unsigned   n = ARRAYVAL(arg)->size;
      ovm_inst_t *p, *q;

      _ovm_arrayval_alloc(ovm, inst, n);

      for (p = ARRAYVAL(inst)->data, q = ARRAYVAL(arg)->data; n; --n, ++q, ++p) {
	*p = _ovm_inst_retain(*q);
      }
    } else if (ovm_is_subclass_of(arg_cl, ovm_cl_set)) {
      unsigned   n;
      ovm_inst_t *p, *q, r;

      _ovm_arrayval_alloc(ovm, inst, SETVAL(arg)->cnt);

      for (p = ARRAYVAL(inst)->data, q = ARRAYVAL(arg)->data, n = ARRAYVAL(arg)->size;
	   n;
	   --n, ++q
	   ) {
	for (r = *q; r; r = CDR(r), ++p) {
	  *p = _ovm_inst_retain(CAR(r));
	}
      }
    } else {
      OVM_ASSERT(0);
    }
    
    --argc;  ++argv;
  }
  
  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

PRIVATE void
_ovm_array_walk(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, void (*func)(ovm_t ovm, ovm_inst_t inst))
{
  ovm_inst_t *p;
  unsigned   n;

  for (p = ARRAYVAL(inst)->data, n = ARRAYVAL(inst)->size; n; --n, ++p) {
    (*func)(ovm, *p);
  }

  _ovm_walk_parent(ovm, inst, cl, func);
}

PRIVATE void 
_ovm_array_free(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl)
{
  _ovm_free(ovm, ARRAYVAL(inst)->size * sizeof(ARRAYVAL(inst)->data[0]), ARRAYVAL(inst)->data);

  _ovm_free_parent(ovm, inst, cl);  
}

PRIVATE void
_ovm_array_at(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_intval_t ofs, len;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_array));
  OVM_ASSERT(argc == 1);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_integer));

  ofs = INTVAL(argv[1]);
  len = 1;
  if (slice(&ofs, &len, ARRAYVAL(argv[0])->size) < 0) {
    __ovm_except_raise(ovm, OVM_EXCEPT_CODE_RANGE_ERR, argv[1], __FILE__, __LINE__);
  }

  OVM_CASSIGN(ovm, dst, ARRAYVAL(argv[0])->data[ofs]);
}

PRIVATE void
_ovm_array_at_put(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_intval_t ofs, len;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_array));
  OVM_ASSERT(argc == 2);
  OVM_ASSERT(_ovm_is_kind_of(argv[1], ovm_cl_integer));

  ofs = INTVAL(argv[1]);
  len = 1;
  slice(&ofs, &len, ARRAYVAL(argv[0])->size);

  _ovm_assign(ovm, &ARRAYVAL(argv[0])->data[ofs], argv[2]);

  OVM_CASSIGN(ovm, dst, argv[2]);
}

PUBLIC const struct ovm_class ovm_cl_array[1] = {
  { .name   = "Array",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new2,
    .init   = _ovm_array_init,
    .walk   = _ovm_array_walk,
    .free   = _ovm_array_free,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_AT]     = _ovm_array_at,
      [OVM_METHOD_CALL_SEL_AT_PUT] = _ovm_array_at_put
    }
  }
};

/***************************************************************************/

PRIVATE unsigned
round_up_to_power_of_2(unsigned i)
{
  unsigned j;

  OVM_ASSERT(i > 0);

  j = i & (i - 1);
  if (j == 0)  return (i);
  for (;;) {
    i = j;
    j = i & (i - 1);
    if (j == 0)  return (i << 1);
  }
}

PRIVATE ovm_inst_t *
_set_find(ovm_t ovm, ovm_inst_t set, ovm_inst_t val, ovm_inst_t **bb)
{
  ovm_inst_t *wp, *old, *b, *p, q;

  wp = _ovm_falloc(ovm, 1, &old);

  __ovm_method_call(ovm, &wp[0], val, OVM_METHOD_CALL_SEL_HASH, 0);
  b = &SETVAL(set)->base->data[INTVAL(wp[0]) & (SETVAL(set)->base->size - 1)];

  for (p = b; q = *p; p = &CDR(q)) {
    __ovm_method_call(ovm, &wp[0], val, OVM_METHOD_CALL_SEL_EQUAL, 1, CAR(q));
    if (BOOLVAL(wp[0]))  break;
  }
  if (q == 0)  p = 0;

  if (bb)  *bb = b;

  ovm_ffree(ovm, old);

  return (p);
}

PRIVATE unsigned
_set_at(ovm_t ovm, ovm_inst_t set, ovm_inst_t val)
{
  return (_set_find(ovm, set, val, 0) != 0);
}

PRIVATE void
_set_put(ovm_t ovm, ovm_inst_t set, ovm_inst_t val)
{
  ovm_inst_t *b;

  if (_set_find(ovm, set, val, &b) == 0) {
    _ovm_dptr_newc(ovm, b, ovm_cl_list, val, *b);

    ++SETVAL(set)->cnt;
  }
}

PRIVATE void
_set_del(ovm_t ovm, ovm_inst_t set, ovm_inst_t val)
{
  ovm_inst_t *p;

  p = _set_find(ovm, set, val, 0);
  if (p != 0) {
    _ovm_assign(ovm, p, CDR(*p));

    --SETVAL(set)->cnt;
  }
}

PRIVATE void
_ovm_set_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc == 0) {
    _ovm_arrayval_init(ovm, inst, OVM_SET_SIZE_DFLT);
  } else {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg_cl == ovm_cl_integer) {
      ovm_intval_t size = INTVAL(arg);

      OVM_ASSERT(size > 0);

      _ovm_arrayval_init(ovm, inst, round_up_to_power_of_2(size));
    } else if (arg_cl == ovm_cl_list) {
      ovm_inst_t p;

      _ovm_arrayval_init(ovm, inst, OVM_SET_SIZE_DFLT);
      
      for (p = arg; p; p = CDR(p)) {
	_set_put(ovm, inst, CAR(p));
      }
    } else if (arg_cl == ovm_cl_array) {
      ovm_inst_t *p;
      unsigned   n;

      _ovm_arrayval_init(ovm, inst, OVM_SET_SIZE_DFLT);
      
      for (p = ARRAYVAL(arg)->data, n = ARRAYVAL(arg)->size; n; --n, ++p) {
	_set_put(ovm, inst, *p);
      }
    } else if (ovm_is_subclass_of(arg_cl, ovm_cl_set)) {
      ovm_inst_t *p, q;
      unsigned   n;

      _ovm_arrayval_init(ovm, inst, OVM_SET_SIZE_DFLT);
      
      for (p = SETVAL(arg)->base->data, n = SETVAL(arg)->base->size; n; --n, ++p) {
	for (q = *p; q; q = CDR(q)) {
	  _set_put(ovm, inst, CAR(q));
	}
      }
    } else {
      OVM_ASSERT(0);
    }

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

PRIVATE void
_ovm_set_at(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_set));
  OVM_ASSERT(argc == 1);

  if (dst == 0)  return;

  _ovm_boolean_newc(ovm, dst, _set_at(ovm, argv[0], argv[1]));
}

PRIVATE void
_ovm_set_del(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t *p;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_set));
  OVM_ASSERT(argc == 1);

  _set_del(ovm, argv[0], argv[1]);

  OVM_CASSIGN(ovm, dst, argv[1]);
}

PRIVATE void
_ovm_set_put(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t *b;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_set));
  OVM_ASSERT(argc == 1);

  _set_put(ovm, argv[0], argv[1]);

  OVM_CASSIGN(ovm, dst, argv[1]);
}

PUBLIC const struct ovm_class ovm_cl_set[1] = {
  { .name   = "Set",
    .parent = ovm_cl_array,
    .new    = _ovm_inst_new2,
    .init   = _ovm_set_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_AT]  = _ovm_set_at,
      [OVM_METHOD_CALL_SEL_DEL] = _ovm_set_del,
      [OVM_METHOD_CALL_SEL_PUT] = _ovm_set_put
    }
  }
};

/***************************************************************************/

PRIVATE ovm_inst_t *
_dict_find(ovm_t ovm, ovm_inst_t dict, ovm_inst_t key, ovm_inst_t **bb)
{
  ovm_inst_t *wp, *old, *b, *p, q;

  wp = _ovm_falloc(ovm, 1, &old);

  __ovm_method_call(ovm, &wp[0], key, OVM_METHOD_CALL_SEL_HASH, 0);
  b = &SETVAL(dict)->base->data[INTVAL(wp[0]) & (SETVAL(dict)->base->size - 1)];

  for (p = b; q = *p; p = &CDR(q)) {
    __ovm_method_call(ovm, &wp[0], key, OVM_METHOD_CALL_SEL_EQUAL, 1, CAR(CAR(q)));
    if (BOOLVAL(wp[0]))  break;
  }
  if (q == 0)  p = 0;

  if (bb)  *bb = b;

  ovm_ffree(ovm, old);

  return (p);
}

PRIVATE ovm_inst_t
_dict_at(ovm_t ovm, ovm_inst_t dict, ovm_inst_t key)
{
  ovm_inst_t *p;

  p = _dict_find(ovm, dict, key, 0);
  return (p == 0 ? 0 : CAR(*p));
}

PRIVATE void
_dict_at_put(ovm_t ovm, ovm_inst_t dict, ovm_inst_t key, ovm_inst_t val)
{
  ovm_inst_t *p, *b;

  p = _dict_find(ovm, dict, key, &b);
  if (p == 0) {
    ovm_inst_t *wp, *old;

    wp = _ovm_falloc(ovm, 1, &old);

    _ovm_dptr_newc(ovm, &wp[0], ovm_cl_pair, key, val);
    _ovm_dptr_newc(ovm, b, ovm_cl_list, wp[0], *b);

    ++SETVAL(dict)->cnt;

    ovm_ffree(ovm, old);
  } else {
    _ovm_assign(ovm, &CDR(CAR(*p)), val);
  }
}

PRIVATE void
_dict_put(ovm_t ovm, ovm_inst_t dict, ovm_inst_t val)
{
  ovm_inst_t k, v;

  if (_ovm_inst_of(val) == ovm_cl_pair) {
    k = CAR(val);
    v = CDR(val);
  } else {
    k = val;
    v = 0;
  }

  _dict_at_put(ovm, dict, k, v);
}

PRIVATE void
_dict_del(ovm_t ovm, ovm_inst_t dict, ovm_inst_t key)
{
  ovm_inst_t *p;

  p = _dict_find(ovm, dict, key, 0);
  if (p != 0) {
    _ovm_assign(ovm, p, CDR(*p));

    --SETVAL(dict)->cnt;
  }
}

PRIVATE void
_ovm_dict_init(ovm_t ovm, ovm_inst_t inst, ovm_class_t cl, unsigned argc, ovm_inst_t *argv)
{
  if (argc == 0) {
    _ovm_arrayval_init(ovm, inst, OVM_DICT_SIZE_DFLT);
  } else {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

    if (arg_cl == ovm_cl_integer) {
      ovm_intval_t size = INTVAL(arg);

      OVM_ASSERT(size > 0);

      _ovm_arrayval_init(ovm, inst, round_up_to_power_of_2(size));
    } else if (arg_cl == ovm_cl_list) {
      ovm_inst_t p;

      _ovm_arrayval_init(ovm, inst, OVM_DICT_SIZE_DFLT);
      
      for (p = arg; p; p = CDR(p)) {
	_dict_put(ovm, inst, CAR(p));
      }
    } else if (arg_cl == ovm_cl_array) {
      ovm_inst_t *p;
      unsigned   n;

      _ovm_arrayval_init(ovm, inst, OVM_DICT_SIZE_DFLT);
      
      for (p = ARRAYVAL(arg)->data, n = ARRAYVAL(arg)->size; n; --n, ++p) {
	_dict_put(ovm, inst, *p);
      }
    } else if (ovm_is_subclass_of(arg_cl, ovm_cl_set)) {
      ovm_inst_t *p, q;
      unsigned   n;

      _ovm_arrayval_init(ovm, inst, OVM_DICT_SIZE_DFLT);
      
      for (p = SETVAL(arg)->base->data, n = SETVAL(arg)->base->size; n; --n, ++p) {
	for (q = *p; q; q = CDR(q)) {
	  _dict_put(ovm, inst, CAR(q));
	}
      }
    } else {
      OVM_ASSERT(0);
    }

    --argc;  ++argv;
  }

  _ovm_init_parent(ovm, inst, cl, argc, argv);
}

PRIVATE void
_ovm_dict_at(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t p;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dictionary));
  OVM_ASSERT(argc == 1);

  if (dst == 0)  return;

  p = _dict_at(ovm, argv[0], argv[1]);
  if (p == 0) {
    _ovm_assign(ovm, dst, p);
  } else {
    _ovm_dptr_newc(ovm, dst, ovm_cl_pair, CAR(p), CDR(p));
  }
}

PRIVATE void
_ovm_dict_at_put(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t *b;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dictionary));
  OVM_ASSERT(argc == 2);

  _dict_at_put(ovm, argv[0], argv[1], argv[2]);

  OVM_CASSIGN(ovm, dst, argv[1]);
}

PRIVATE void
_ovm_dict_del(ovm_t ovm, ovm_inst_t *dst, unsigned argc, ovm_inst_t *argv)
{
  ovm_inst_t *p;

  OVM_ASSERT(_ovm_is_kind_of(argv[0], ovm_cl_dictionary));
  OVM_ASSERT(argc == 1);

  _dict_del(ovm, argv[0], argv[1]);

  OVM_CASSIGN(ovm, dst, argv[1]);
}

PUBLIC const struct ovm_class ovm_cl_dictionary[1] = {
  { .name   = "Dictionary",
    .parent = ovm_cl_set,
    .new    = _ovm_inst_new2,
    .init   = _ovm_dict_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_AT]     = _ovm_dict_at,
      [OVM_METHOD_CALL_SEL_AT_PUT] = _ovm_dict_at_put,
      [OVM_METHOD_CALL_SEL_DEL]    = _ovm_dict_del
    }
  }
};

/***************************************************************************/

PUBLIC unsigned
ovm_is_nil(ovm_t ovm, unsigned src)
{
  REG_CHK(src);

  return (ovm->regs[src] == 0);
}

PUBLIC unsigned
ovm_is_true(ovm_t ovm, unsigned src)
{
  ovm_inst_t inst;

  REG_CHK(src);

  inst = ovm->regs[src];
  return (_ovm_inst_of(inst) == ovm_cl_boolean && BOOLVAL(inst) != 0);
}

PUBLIC ovm_class_t
ovm_inst_of(ovm_t ovm, unsigned src)
{
  REG_CHK(src);

  return (_ovm_inst_of(ovm->regs[src]));
}

PUBLIC const char *
ovm_class_name(ovm_class_t cl)
{
  return (cl->name);
}

PUBLIC ovm_class_t
ovm_class_parent(ovm_class_t cl)
{
  return (cl->parent);
}

PUBLIC unsigned
ovm_is_subclass_of(ovm_class_t cl1, ovm_class_t cl2)
{
  for ( ; cl1; cl1 = cl1->parent) {
    if (cl1 == cl2)  return (1);
  }
  
  return (0);
}

PUBLIC unsigned
ovm_is_kind_of(ovm_t ovm, unsigned src, ovm_class_t cl)
{
  REG_CHK(src);

  return (_ovm_is_kind_of(ovm->regs[src], cl));
}

PUBLIC void
ovm_move(ovm_t ovm, unsigned dst, unsigned src)
{
  REG_CHK(dst);
  REG_CHK(src);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], ovm->regs[src]);
}

PUBLIC void
ovm_push(ovm_t ovm, unsigned src)
{
  REG_CHK(src);
  OVM_ASSERT(ovm->sp > ovm->stack);

  *--ovm->sp = _ovm_inst_retain(ovm->regs[src]);

#ifndef NDEBUG
  _ovm_stk_stats_up(ovm, 1);
#endif
}

PUBLIC void
ovm_pushm(ovm_t ovm, unsigned src, unsigned n)
{
  ovm_inst_t *p, *q;
  unsigned   k;

  REG_CHK(src + n - 1);
  OVM_ASSERT((ovm->sp - n) >= ovm->stack);

  for (p = (ovm->sp -= n), q = &ovm->regs[src], k = n; k; --k, ++p, ++q) {
    *p = _ovm_inst_retain(*q);
  }

#ifndef NDEBUG
  _ovm_stk_stats_up(ovm, n);
#endif
}

PUBLIC void
ovm_pop(ovm_t ovm, unsigned dst)
{
  REG_CHK(dst);
  OVM_ASSERT(ovm->sp < ovm->stack_end);

  if (dst != 0) {
    __ovm_assign(ovm, &ovm->regs[dst], *ovm->sp++);
  } else {
    _ovm_inst_release(ovm, *ovm->sp++);
  }

#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, 1);
#endif
}

PUBLIC void
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

#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, n);
#endif
}

PUBLIC void
ovm_drop(ovm_t ovm)
{
  OVM_ASSERT(ovm->sp < ovm->stack_end);

  _ovm_inst_release(ovm, *ovm->sp++);

#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, 1);
#endif
}

PUBLIC void
ovm_dropm(ovm_t ovm, unsigned n)
{
  ovm_inst_t *p;
  unsigned   k;

  OVM_ASSERT((ovm->sp + n) <= ovm->stack_end);

  for (p = ovm->sp, k = n; k; --k, ++p) {
    _ovm_inst_release(ovm, *p);
  }

  ovm->sp += n;

#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, n);
#endif
}

PUBLIC void *
ovm_falloc(ovm_t ovm, unsigned n)
{
  ovm_inst_t *old;

  _ovm_falloc(ovm, n, &old);

  return (old);
}

PUBLIC void
ovm_ffree(ovm_t ovm, void *old)
{
  ovm_inst_t *p;
  unsigned   n;

  for (n = 0, p = ovm->sp; p < ovm->fp; ++p, ++n) {
    _ovm_inst_release(ovm, *p);
  }
  ovm->sp = p;

  ovm->fp = (ovm_inst_t *) old;  

#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, n);
#endif
}

PUBLIC void
ovm_fload(ovm_t ovm, unsigned dst, int src)
{
  ovm_inst_t *p;

  REG_CHK(dst);
  p = ovm->fp + src;
  OVM_ASSERT(p >= ovm->sp && p < ovm->stack_end);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], *p);
}

PUBLIC void
ovm_fstore(ovm_t ovm, int dst, unsigned src)
{
  ovm_inst_t *p;

  p = ovm->fp + dst;
  OVM_ASSERT(p >= ovm->sp && p < ovm->stack_end);
  REG_CHK(src);

  _ovm_assign(ovm, p, ovm->regs[src]);
}

PUBLIC void
ovm_gload(ovm_t ovm, unsigned dst, unsigned src)
{
  REG_CHK(dst);
  OVM_ASSERT(src < ovm->glob_size);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], ovm->glob[src]);
}

PUBLIC void
ovm_gstore(ovm_t ovm, unsigned dst, unsigned src)
{
  OVM_ASSERT(dst < ovm->glob_size);
  REG_CHK(src);
  
  _ovm_assign(ovm, &ovm->glob[dst], ovm->regs[src]);
}

PUBLIC void
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

PUBLIC void
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

PUBLIC void
ovm_boolean_newc(ovm_t ovm, unsigned dst, ovm_boolval_t val)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_boolean_newc(ovm, &ovm->regs[dst], val);
}

PUBLIC void
ovm_integer_newc(ovm_t ovm, unsigned dst, ovm_intval_t val)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_integer_newc(ovm, &ovm->regs[dst], val);
}

PUBLIC void
ovm_string_newc(ovm_t ovm, unsigned dst, char *s)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_string_newc(ovm, &ovm->regs[dst], s);
}

PUBLIC void
ovm_xml_newc(ovm_t ovm, unsigned dst, char *s)
{
  ovm_inst_t *p;

  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_xml_newc(ovm, &ovm->regs[dst], s);
}

PUBLIC void
ovm_bitmap_newc(ovm_t ovm, unsigned dst, unsigned size, ovm_bmval_unit_t *data)
{
  REG_CHK(dst);

  if (dst == 0)  return;
  
  _ovm_bitmap_newc(ovm, &ovm->regs[dst], size, data);
}

PUBLIC void
ovm_array_newc(ovm_t ovm, unsigned dst, unsigned size)
{
  REG_CHK(dst);

  if (dst == 0)  return;

  _ovm_array_newc(ovm, &ovm->regs[dst], size);
}

PUBLIC void
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

PUBLIC void
_ovm_except_frame_begin(ovm_t ovm, struct ovm_except_frame *xfr)
{
  memset(xfr, 0, sizeof(*xfr));

  xfr->sp = ovm->sp;
  xfr->fp = ovm->fp;

  xfr->prev = ovm->xfp;
  ovm->xfp = xfr;
}

PUBLIC void
_ovm_except_reraise(ovm_t ovm)
{
  struct ovm_except_frame *xfr = ovm->xfp, *pxfr;
  ovm_inst_t              *p;
  unsigned                n;

  OVM_ASSERT(xfr != 0);
  pxfr = xfr->prev;
  if (pxfr == 0)  _ovm_except_uncaught(ovm, xfr->code, xfr->arg, xfr->file, xfr->line);

  for (n = 0, p = ovm->sp; p < pxfr->sp; ++p, ++n) {
    _ovm_inst_release(ovm, *p);
  }
#ifndef NDEBUG
  _ovm_stk_stats_dn(ovm, n);
#endif
  ovm->fp = pxfr->fp;

  __ovm_assign(ovm, &pxfr->arg, xfr->arg);
  pxfr->file = xfr->file;
  pxfr->line = xfr->line;

  ovm->xfp = pxfr;

  longjmp(pxfr->jmp_buf, xfr->code);
}

PUBLIC void
_ovm_except_raise(ovm_t ovm, int code, unsigned src, char *file, unsigned line)
{
  REG_CHK(src);

  __ovm_except_raise(ovm, code, ovm->regs[src], file, line);
}

PUBLIC void
_ovm_except_frame_end(ovm_t ovm)
{
  struct ovm_except_frame *xfr = ovm->xfp;

  if (xfr->code != 0 && !xfr->caughtf)  _ovm_except_reraise(ovm);
  
  _ovm_inst_release(ovm, xfr->arg);

  ovm->xfp = xfr->prev;
}

PUBLIC void
ovm_except_arg_get(ovm_t ovm, unsigned dst)
{
  OVM_ASSERT(ovm->xfp);
  REG_CHK(dst);

  if (dst == 0)  return;

  _ovm_assign(ovm, &ovm->regs[dst], ovm->xfp->arg);
}

PUBLIC void
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

#ifndef NDEBUG

PUBLIC void
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

#endif
