#include "ir.h"
#include "ast.h"
#include "alloc.h"

static Value* value_stack = null;
static const u64 VALUE_POOL_LENGTH = 1 << 30;
static Value* value_stack_head = null;
static Value* value_stack_end  = null;

typedef struct VcHashTable_Entry {
	u64 const_value;
	Value* value;
} VcHashTable_Entry;

typedef struct VcHashTable_EntryBlock {
	u32 count;
	VcHashTable_Entry* entries;
} VcHashTable_EntryBlock;

VcHashTable_EntryBlock constant_hashtable[4096];

static void ir_init(void) {
	value_stack = alloc(sizeof(Value) * VALUE_POOL_LENGTH);
	value_stack_head = value_stack;
	value_stack_end = value_stack + VALUE_POOL_LENGTH;

	for (int i = 0; i < 256; i++) {
		value_stack[i] = (Value){
			.flags   = VALUE_FLAG_CONSTANT,
			.integer = -128 + i,
		};
	}

	zero(constant_hashtable, sizeof(constant_hashtable));
}

static Value* make_value(void) {
	return value_stack_head++;
}

static Value* ir_int(u64 n) {
	if (n + 128 < 256)
		return value_stack + (n + 128);

	VcHashTable_EntryBlock* block = &constant_hashtable[hash64(n) & 4095];

	for (u32 i = 0; i < block->count; i++) {
		VcHashTable_Entry* entry = &block->entries[i];

		if (entry->value->integer != n) continue;

		return entry->value;
	}

	Value* new_value = make_value();
	*new_value = (Value) {
		.integer = n,
		.flags = VALUE_FLAG_CONSTANT,
	};

	VcHashTable_Entry new_entry = {
		.value = new_value,
		.const_value = n,
	};

	block->entries = realloc(
		block->entries,
		sizeof(VcHashTable_Entry) * block->count,
		sizeof(VcHashTable_Entry) * (block->count + 1)
	);

	block->entries[block->count++] = new_entry;

	return new_value;
}

static Value* ir_f32(f32 f) {
	Value* v = ir_int((u64)*(u32*)&f);
	v->fp32 = f;
	return v;
}

static Value* ir_f64(f64 f) {
	Value* v = ir_int(*(u64*)&f);
	v->fp64 = f;
	return v;
}

static void ir_insert_relation(RelationSet* set, Relation relation) {
	set->relations = realloc(set->relations, sizeof(Relation)*set->count, sizeof(Relation)*(set->count+1));
	set->relations[set->count++] = relation;
}

static void ir_insert_distance_relation(Value* from, Value* to, Value* dist, Context* context) {
	// Filter out const to const relations?
	if (value == ir_int(0))
		return value;

	if (from->flags & to->flags & VALUE_FLAG_CONSTANT)
		return ir_int(to-integer - from->integer);

	for (u32 i = 0; i < value->relations.count; i++) {
		Relation* relation = &value->relations.relations[i];
		if (relation->kind != REL_SUB) continue;
		if (relation->to != ir_int(0)) continue;
		return relation->value;
	}

	Value* result = make_value();
	ir_relate_distance(value, ir_int(0), result);
	return result;
}

static Value* ir_int_negative(Value* value) {
	if (value == ir_int(0))
		return value;

	if (value->flags & VALUE_FLAG_CONSTANT)
		return ir_int(-value->integer);

	for (u32 i = 0; i < value->relations.count; i++) {
		Relation* relation = &value->relations.relations[i];
		if (relation->kind != REL_SUB)   continue;
		if (relation->to   != ir_int(0)) continue;
		return relation->value;
	}

	Value* result= make_value();
	ir_relate_distance(value, ir_int(0), result);
	return result;
}

