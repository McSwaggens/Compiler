#include "ir.h"
#include "ast.h"
#include "alloc.h"

static Value* value_pool = null;
static const u64 VALUE_POOL_LENGTH = 1 << 30;
static Value* value_pool_head = null;

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
	value_pool = alloc(sizeof(Value) * VALUE_POOL_LENGTH);
	value_pool_head = value_pool;
	value_pool_end = value_pool + VALUE_POOL_LENGTH;

	zero(constant_hashtable, sizeof(constant_hashtable));
}

static Value* alloc_value(void) {
	return value_pool_head++;
}

static RelationSet expand_relation_set(RelationSet* old) {
}

static Value* ir_const_int(u64 n) {
	if (n + 128 < 256)
		return value_pool + (n + 128);

	VcHashTable_EntryBlock* block = &constant_hashtable[hash64(n) & 4095];

	for (u32 i = 0; i < block->count; i++) {
		VcHashTable_Entry* entry = &block->entries[i];

		if (entry->value != n) continue;

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


