#ifndef IR_H
#define IR_H

#include "general.h"

typedef struct Value    Value;
typedef struct Relation Relation;
typedef struct Context  Context;
typedef struct Key      Key;
typedef struct Trigger  Trigger;

typedef enum RelationKind RelationKind;
typedef enum ValueKind ValueKind;

typedef u32 V32;

enum Resolution { UNKNOWN = 0, KNOWN_FALSE = 1, KNOWN_TRUE = 2 };

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

struct Key {
	RelationKind relation;
	V32 row;
	V32 col;
};

struct Relation {
	RelationKind kind;
	V32 col;
	V32 cell;
	Key key;
};

struct Context {
	Key* keys;
	u16 count;
	u16 capacity;
};

struct Trigger {
	Relation relation;
	Context context;
	Ast* ast;
};

struct Value {
	u16 relation_count;
	u16 relation_capacity;
	Relation* relations;

	union {
		u64 const_int;
		f32 const_f32;
		f64 const_f64;
	};
};

static void init_ir(void);
static void relate(V32 v, Relation rel);
static V32 make_value(void);
static inline Value* get_value(V32 id);
static void insert_trigger(V32 v, Trigger trigger);

static bool    context_check_for_key(Context* context, Key key);
static void    context_add(Context* context, Key key);
static void    context_free(Context context);
static Context context_duplicate(Context context);

static V32 const_int(u64 n);
static V32 const_f32(f32 n);
static V32 const_f64(f64 n);

#endif
