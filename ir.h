#ifndef IR_H
#define IR_H

typedef struct Value    Value;
typedef struct Relation Relation;
typedef struct Context  Context;
typedef struct Key      Key;
typedef struct Trigger  Trigger;
typedef struct Procedure Procedure;
typedef struct ProcedureList ProcedureList;

typedef enum RelationKind RelationKind;
typedef enum ValueKind ValueKind;

typedef u32 V32;

#include "general.h"
#include "ir.h"

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
	union Ast* ast;
};

struct Value {
	bool is_const;

	u16 relation_count;
	u16 relation_capacity;
	Relation* relations;

	union {
		u64 const_int;
		f32 const_f32;
		f64 const_f64;
	};
};

struct Procedure {
	Context context;
	V32 return_values;
};

struct ProcedureList {
	Procedure* data;
	u32 count;
	u32 capacity;
};

static void init_ir(void);
static void ir_relate(V32 v, Relation rel);
static V32 ir_make_value(void);
static inline Value* ir_get_value(V32 id);
static void ir_insert_trigger(V32 v, Trigger trigger);

static bool    ir_context_check_for_key(Context* context, Key key);
static void    ir_context_add(Context* context, Key key);
static void    ir_context_free(Context context);
static Context ir_context_duplicate(Context context);

static u64 ir_get_const_int(V32 v);

static V32 ir_int(u64 n);
static V32 ir_f32(f32 n);
static V32 ir_f64(f64 n);

#endif
