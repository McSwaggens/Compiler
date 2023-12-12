#include "ir.h"
#include "ast.h"

static Value value_table[1llu<<32llu];
static V32 value_table_index_head;

// -------------------------------------------------- //

static void init_ir(void) {
	value_table_index_head = 4;
	*get_value(const_int(-1)) = (Value){ .const_int = -1, };
	*get_value(const_int(0))  = (Value){ .const_int =  0, };
	*get_value(const_int(1))  = (Value){ .const_int =  1, };
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
	switch (n) {
		case -1:
		case 0:
		case 1:
			return n+1+1;
		default: break;
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
		.id   = 123,
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

