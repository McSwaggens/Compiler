#ifndef IR_H
#define IR_H

#include "general.h"

typedef struct Value       Value;
typedef struct Relation    Relation;
typedef struct RelationSet RelationSet;
typedef struct Key         Key;
typedef struct Context     Context;
typedef enum Resolution   Resolution;
typedef enum RelationKind RelationKind;
typedef enum ValueFlags   ValueFlags;

enum ValueFlags {
	VALUE_CONST_INT  = 1 << 0,
	VALUE_CONST_F32  = 1 << 1,
	VALUE_CONST_F64  = 1 << 2,
	VALUE_CONST_MASK = VALUE_CONST_INT | VALUE_CONST_F32 | VALUE_CONST_F64,
};

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
	REL_INDEX,
};

enum Resolution { UNKNOWN = 0, KNOWN_FALSE = 1, KNOWN_TRUE = 2 };

struct Relation {
	RelationKind kind;
	struct Value* to;
	struct Value* value;
};

struct RelationSet {
	Relation* relations;
	u16 count;
	u16 capacity;
};

struct Value {
	ValueFlags flags;
	RelationSet relations;

	u64 constant_value;
};

struct Key {
	RelationKind relation;
	Value* row;
	Value* col;
};

struct Context {
	u16 count;
	u16 capacity;
	Key* keys;
};

struct Trigger {
	Relation relation;
	Context context;
};

static void ir_init(void);
static Value* make_value(void);
static RelationSet expand_relation_set(RelationSet* old);

static Value* ir_const_int(u64 n);
static Value* ir_const_f32(f32 f);
static Value* ir_const_f64(f64 f);

#endif
