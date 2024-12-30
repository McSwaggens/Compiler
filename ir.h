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
	VALUE_FLAG_CONSTANT = 1 << 0,
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
	REL_IMUL,
	REL_INDEX,
};

#define RELATION_KIND_COUNT (REL_INDEX + 1)

enum Resolution { UNKNOWN = 0, KNOWN_FALSE = 1, KNOWN_TRUE = 2 };

struct Relation {
	Context* context;
	RelationKind kind;
	Value* to;
	Value* value;
};

struct RelationSet {
	Relation* relations;
	u32 count;
};

struct Value {
	ValueFlags flags;
	RelationSet relations;

	union {
		u64 integer;
		f32 fp32;
		f64 fp64;
	};
};

struct Key {
	RelationKind kind;
	Value* row;
	Value* col;
};

struct Context {
	u16 counts[RELATION_KIND_COUNT];
	byte keydata[];
};

struct Trigger {
	Relation relation;
	Context context;
};

static void ir_init(void);
static Value* make_value(void);

static Value* ir_int(u64 n);
static Value* ir_f32(f32 f);
static Value* ir_f64(f64 f);

#endif
