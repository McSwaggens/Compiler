#ifndef MM_H_INCLUDED
#define MM_H_INCLUDED

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
};

bool mm_is_load(M32 m);

#endif // MM_H_INCLUDED

