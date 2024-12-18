#include "ir.h"
#include "ast.h"

typedef struct ValueConstantHashTable_Node {
	u64 value;
	Value* value;
} ValueConstantHashTable_Node;

typedef struct ValueConstantHashTable {
	u32 count;
} ValueConstantHashTable;

ValueConstantHashTable constant_hashtable[4096];

static void ir_init(void) {
	zero(constant_hashtable, sizeof(constant_hashtable));
}

static Value* make_value(void) {
}

static RelationSet expand_relation_set(RelationSet* old) {
}

static Value* ir_const_int(u64 n) {

}

static Value* ir_const_f32(f32 f) {
	return ir_const_int((u64)*(u32*)&f);
}

static Value* ir_const_f64(f64 f) {
	return ir_const_int(*(u64*)&f);
}


