#include "mm.h"

#define MM_NODE_BLOCK_LENGTH (1llu<<32llu)

static MNode mm_node_block[MM_NODE_BLOCK_LENGTH];
static M32 mm_node_head;

static void init_mm(void) {
	mm_node_head = 1;
}

static M32 mm_next_id(void) {
	return mm_node_head++;
}

static inline MNode* mm_get(M32 mid) {
	return &mm_node_block[mid];
}

static M32 mm_make_load(M32 prev, V32 addr, V32 size) {
	M32 mid = mm_next_id();
	MNode* mnode = mm_get(mid);

	*mnode = (MNode){
		.kind = MM_LOAD,
		.addr = addr,
		.size = size,
		.prev = prev,
	};

	return mid;
}

static M32 mm_make_store(M32 prev, V32 addr, V32 value, V32 size) {
	M32 mid = mm_next_id();
	MNode* mnode = mm_get(mid);

	*mnode = (MNode){
		.kind  = MM_STORE,
		.addr  = addr,
		.size  = size,
		.value = value,
		.prev  = prev,
	};

	return mid;
}

