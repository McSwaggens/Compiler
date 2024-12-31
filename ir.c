#include "ir.h"
#include "ast.h"
#include "alloc.h"

static Context* empty_context;

static Context* context_stack = null;
static Context* context_stack_head = null;
static Context* context_stack_end = null;
static const u64 CONTEXT_POOL_LENGTH = 1 << 20;

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
	context_stack = alloc(sizeof(Context) * CONTEXT_POOL_LENGTH);
	context_stack_head = context_stack;
	context_stack_end = context_stack + CONTEXT_POOL_LENGTH;
	empty_context = make_context(null, 0);

	value_stack = alloc(sizeof(Value) * VALUE_POOL_LENGTH);
	value_stack_head = value_stack;
	value_stack_end = value_stack + VALUE_POOL_LENGTH;

	for (int i = 0; i < 256; i++) {
		value_stack[i] = (Value){
			.flags   = VALUE_FLAG_CONSTANT,
			.integer = -128 + i,
		};
	}

	value_stack_head += 256;

	zero(constant_hashtable, sizeof(constant_hashtable));
}

static Context* make_context(Key* keys, u32 key_count) {
	Context* result = context_stack_head++;
	*result = (Context){
		.children = null,
		.child_count = 0,
		.keys = copyalloc(keys, sizeof(Key) * key_count),
	};

	return result;
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
	return v;
}

static Value* ir_f64(f64 f) {
	Value* v = ir_int(*(u64*)&f);
	return v;
}

static void ir_insert_relation(RelationSet* set, Relation relation) {
	set->relations = realloc(set->relations, sizeof(Relation)*set->count, sizeof(Relation)*(set->count+1));
	set->relations[set->count++] = relation;
}

static Value* ir_dist(Context* context, Value* from, Value* to) {
	// Filter out const to const relations?
	if (from == ir_int(0))
		return from;

	if (from->flags & to->flags & VALUE_FLAG_CONSTANT)
		return ir_int(to->integer - from->integer);

	for (u32 i = 0; i < from->relations.count; i++) {
		Relation* relation = &from->relations.relations[i];
		if (relation->kind != REL_SUB) continue;
		if (relation->to != ir_int(0)) continue;
		return relation->value;
	}

	Value* stub = make_value();
	ir_insert_relation(&from->relations, (Relation){
		.context = context,
		.kind = REL_SUB,
		.value = stub,
		.to = to,
	});

	return stub;
}

static Value* ir_ineg(Context* context, Value* value) {
	return ir_dist(context, value, ir_int(0));
}

static Value* ir_imul(Context* context, Value* from, Value* value) {
	if (from == ir_int(0) || value == ir_int(0)) return ir_int(0);
	if (from == ir_int(1) || value == ir_int(1)) return ir_int(1);

	if (from  == ir_int(-1)) return ir_ineg(context, value);
	if (value == ir_int(-1)) return ir_ineg(context, from);

	if (from->flags & value->flags & VALUE_FLAG_CONSTANT)
		return ir_int(from->integer * value->integer);

	Value* to = make_value();
	ir_insert_relation(&from->relations, (Relation){
		.context = context,
		.kind = REL_IMUL,
		.value = value,
		.to = to,
	});

	return to;
}

static Context* find_subcontext(Context* context, Key* keys, u32 key_count) {
	for (u32 i = 0; i < key_count; i++) {
		bool found = false;
		Key* key = &keys[i];
		for (u32 j = 0; j < context->child_count; j++) {
			ContextChild* child = &context->children[j];

			if (!compare(&child->key, key, sizeof(Key)))
				continue;

			context = child->context;
			found = true;
			break;
		}

		if (found)
			continue;

		Context* new_context = make_context(keys, key_count);
		context->children = realloc(context->children, sizeof(ContextChild) * context->child_count, sizeof(ContextChild) * (context->child_count + 1));
		context->children[context->child_count++] = (ContextChild){
			.context = new_context,
			.key = *key,
		};

		return new_context;
	}

	return context;
}

static Context* find_context(Key* keys, u32 key_count) {
	if (key_count == 0)
		return empty_context;

	return find_subcontext(empty_context, keys, key_count);
}

