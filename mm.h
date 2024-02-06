#ifndef MM_H_INCLUDED
#define MM_H_INCLUDED

#include "ir.h"

typedef struct MNode MNode;
typedef u32 M32;
typedef enum MKind MKind;

enum MKind {
	MM_LOAD,
	MM_STORE,
};

struct MNode {
	MKind kind;

	M32 prev;

	V32 size;
	V32 addr;
	V32 value;

	Context condition;
};

static void init_mm(void);
static inline MNode* mm_get(M32 mid);
static M32 mm_make_load(M32 prev, V32 addr, V32 size);
static M32 mm_make_store(M32 prev, V32 addr, V32 value, V32 size);

#endif // MM_H_INCLUDED

