#ifndef IR_H
#define IR_H

#include "general.h"

typedef struct Value Value;
typedef struct Relation Relation;

typedef enum RelationKind RelationKind;
typedef enum ValueKind ValueKind;

typedef u32 V32;

enum RelationKind {
	REL_E,
	REL_NE,
	REL_SL,
	REL_SLE,
	REL_SG,
	REL_SGE,
	REL_UL,
	REL_ULE,
	REL_UG,
	REL_UGE,
	REL_SUB,
	REL_UMUL,
};

struct Relation {
	RelationKind kind;
	V32 to;
	V32 v;
	V32 key;
};

struct Value {
	u16 relation_count;
	Relation* relations;

	union {
		u64 const_int;
		f32 const_f32;
		f64 const_f64;
	};
};

static void init_ir(void);
static V32 make_value(void);
static inline Value* get_value(V32 id);

static V32 const_int(u64 n);
static V32 const_f32(f32 n);
static V32 const_f64(f64 n);

#endif
