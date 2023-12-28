#include "ir.h"
#include "ast.h"

static Value value_table[1llu<<32llu];
static V32 value_table_index_head;

#define IR_PRELOAD_BEGIN -1
#define IR_PRELOAD_END   32

// -------------------------------------------------- //

static void init_ir(void) {
	value_table_index_head = 4;
	for (s64 i = IR_PRELOAD_BEGIN; i <= IR_PRELOAD_BEGIN; i++) {
		*get_value(const_int(i)) = (Value){ .const_int = i };
	}
}

static s32 compare_key(Key a, Key b) {
	if (a.relation < b.relation) return -1;
	if (a.relation > b.relation) return  1;

	if (a.row < b.row) return -1;
	if (a.row > b.row) return  1;

	if (a.col < b.col) return -1;
	if (a.col > b.col) return  1;

	return 0;
}

static inline s32 compare_relation(Relation a, Relation b) {
	if (a.kind < b.kind) return -1;
	if (a.kind > b.kind) return  1;

	if (a.col < b.col) return -1;
	if (a.col > b.col) return  1;

	if (a.cell < b.cell) return -1;
	if (a.cell > b.cell) return  1;

	return compare_key(a.key, b.key);
}

static bool find_relation(Value* value, Relation rel, u16* out_index) {
	u32 d = value->relation_count >> 1;
	u32 index = d;

	for (u32 i = 0; i < boi(value->relation_count); i++) {
		assert(index < value->relation_count);
		assert(d);

		s32 cmp = compare_relation(value->relations[index], rel);

		if (!cmp) {
			*out_index = index;
			return true;
		}

		d >>= 1;

		if (cmp < 0) index -= d;
		if (cmp > 0) index += d;
	}

	*out_index = index;
	return false;
}

static void relate(V32 vid, Relation rel) {
	Value* value = get_value(vid);

	u16 insertion_index = 0;
	if (find_relation(value, rel, &insertion_index))
		return;

	if (value->relation_count == value->relation_capacity) {
		value->relation_capacity = next_pow2(value->relation_capacity|15);
		value->relations = realloc(
			value->relations,
			sizeof(Relation)*value->relation_count,
			sizeof(Relation)*value->relation_capacity
		);
	}

	move(
		value->relations+insertion_index+1,
		value->relations+insertion_index,
		sizeof(Relation)*(value->relation_count-insertion_index)
	);

	value->relations[insertion_index] = rel;
	value->relation_count++;
}

static V32 make_value(void) {
	return value_table_index_head++;
}

static inline Value* get_value(V32 id) {
	return &value_table[id];
}

// -------------------------------------------------- //

typedef struct ConstHashTableNode ConstHashTableNode;

struct ConstHashTableNode {
	u64 n;
	ConstHashTableNode* next;
	V32 id;
};

#define CONST_HASH_TABLE_SIZE (4096)
#define CONST_HASH_TABLE_MASK (CONST_HASH_TABLE_SIZE-1)
static ConstHashTableNode* const_hash_table[CONST_HASH_TABLE_SIZE] = { 0 };

static V32 const_int(u64 n) {
	if ((s64)n >= IR_PRELOAD_BEGIN && (s64)n <= IR_PRELOAD_END) {
		return 1+n+abs(IR_PRELOAD_BEGIN);
	}

	ConstHashTableNode** ppnode = &const_hash_table[hash64(n) & CONST_HASH_TABLE_MASK];

	while (*ppnode && (*ppnode)->n < n) {
		ppnode = &(*ppnode)->next;
	}

	if (*ppnode && (*ppnode)->n == n) {
		return (*ppnode)->id;
	}

	V32 id = make_value();
	*get_value(id) = (Value) {
		.const_int = n,
	};

	ConstHashTableNode* new_node = alloc(sizeof(ConstHashTableNode));

	*new_node = (ConstHashTableNode){
		.id   = id,
		.n    = n,
		.next = *ppnode,
	};

	*ppnode = new_node;

	return id;
}

static V32 const_f32(f32 n) {
	return const_int(*(u32*)&n);
}

static V32 const_f64(f64 n) {
	return const_int(*(u64*)&n);
}

// -------------------------------------------------- //

static bool context_check_for_key(Context* context, Key key) {
	for (u32 i = 0; i < context->count; i++) {
		if (compare(&context->keys[i], &key, sizeof(Key))) {
			return true;
		}
	}

	return false;
}

static void context_add(Context* context, Key key) {
	if (context_check_for_key(context, key)) {
		return;
	}

	if (context->count == context->capacity) {
		context->capacity = next_pow2(context->capacity|3);
		context->keys = realloc(
			context->keys,
			context->count*sizeof(*context->keys),
			context->capacity*sizeof(*context->keys)
		);
	}

	context->keys[context->count++] = key;
}

static void context_free(Context context) {
	free(context.keys, context.capacity*sizeof(context.keys));
}

static Context context_duplicate(Context context) {
	if (!context.count) {
		return (Context){ 0 };
	}

	u64 cap = round_pow2(context.count);

	Context result = (Context){
		.keys     = alloc(cap*sizeof(*context.keys)),
		.capacity = cap,
		.count    = context.count,
	};

	copy(result.keys, context.keys, context.count*sizeof(*context.keys));

	return result;
}

static bool is_contained_in_context(Context* context, Key key) {
	for (u32 i = 0; i < context->count; i++) {
		Key k = context->keys[i];

		if (compare(&k, &key, sizeof(Key)))
			return true;
	}

	return false;
}

// -------------------------------------------------- //

static V32 resolve(Context* context, V32 v) {
	Value* value = get_value(v);

	for (u32 i = 0; i < value->relation_count; i++) {
		Relation* rel = &value->relations[i];

		if (rel->kind != REL_E)
			continue;

		if (!is_contained_in_context(context, rel->key))
			continue;

		return rel->col;
	}

	return 0;
}

// -------------------------------------------------- //

