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

const struct ovm_class ovm_cl_boolean[1] = {
  { .name   = "Boolean",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_boolean_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
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

const struct ovm_class ovm_cl_integer[1] = {
  { .name   = "Integer",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_integer_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
      [OVM_METHOD_CALL_SEL_ADD] = _ovm_integer_add
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

const struct ovm_class ovm_cl_string[1] = {
  { .name   = "String",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_string_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_string_free,
    .inst_method_tbl = {
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
  if (argc > 0) {
    ovm_inst_t  arg = argv[0];
    ovm_class_t arg_cl = _ovm_inst_of(arg);

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
  }

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

const struct ovm_class ovm_cl_bitmap[1] = {
  { .name   = "Bitmap",
    .parent = ovm_cl_object,
    .new    = _ovm_inst_new1,
    .init   = _ovm_bmap_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_bmap_free,
    .inst_method_tbl = {
    }
  }
};

/***************************************************************************/

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

static const struct ovm_class ovm_cl_dptr[1] = {
  { .name   = "Dptr",
    .parent = ovm_cl_object,
    .init   = _ovm_dptr_init,
    .walk   = _ovm_dptr_walk,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
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

const struct ovm_class ovm_cl_list[1] = {
  { .name   = "List",
    .parent = ovm_cl_dptr,
    .new    = _ovm_inst_new1,
    .init   = _ovm_list_init,
    .walk   = _ovm_walk_parent,
    .free   = _ovm_free_parent,
    .inst_method_tbl = {
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

  return (ovm_is_subclass_of(_ovm_inst_of(ovm->regs[src]), cl));
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
