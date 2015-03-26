typedef unsigned char      ovm_boolval_t;
typedef long long          ovm_intval_t;
typedef unsigned long long ovm_uintval_t;
typedef long double        ovm_floatval_t;
typedef unsigned long      ovm_bmval_unit_t;
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
  OVM_SET_SIZE_DFLT = 32,	/**< Default (hash table) size for Set */
  OVM_DICT_SIZE_DFLT = 32	/**< Default (hash table) size for Dictionary */
};

enum {
  OVM_R0 = 0,
  OVM_R1,
  OVM_R2,
  OVM_R3,
  OVM_R4,
  OVM_R5,
  OVM_R6,
  OVM_R7,
  OVM_R8,
  OVM_R9,
  OVM_R10,
  OVM_R11,
  OVM_R12,
  OVM_R13,
  OVM_R14,
  OVM_R15,
  OVM_NUM_REGS
};

