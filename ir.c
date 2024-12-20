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

static Value* alloc_value(void) {
	return value_stack_head++;
}

static RelationSet expand_relation_set(RelationSet* old);

static Value* ir_const_int(u64 n) {
	if (n + 128 < 256)
		return value_stack + (n + 128);

	VcHashTable_EntryBlock* block = &constant_hashtable[hash64(n) & 4095];

	for (u32 i = 0; i < block->count; i++) {
		VcHashTable_Entry* entry = &block->entries[i];

		if (entry->value->integer != n) continue;

		return entry->value;
	}

	Value* new_value = alloc_value();

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

static Value* ir_const_f32(f32 f) {
	return ir_const_int((u64)*(u32*)&f);
}

static Value* ir_const_f64(f64 f) {
	return ir_const_int(*(u64*)&f);
}


