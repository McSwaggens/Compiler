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

static M32 mm_make_load(V32 addr, V32 size) {
	M32 mid = mm_next_id();
	MNode* mnode = mm_get(mid);

	*mnode = (MNode){
		.kind = MM_LOAD,
		.addr = addr,
		.size = size,
	};

	return mid;
}

static M32 mm_make_store(V32 addr, V32 value, V32 size) {
	M32 mid = mm_next_id();
	MNode* mnode = mm_get(mid);

	*mnode = (MNode){
		.kind  = MM_STORE,
		.addr  = addr,
		.size  = size,
		.value = value,
	};

	return mid;
}

