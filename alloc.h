#ifndef ALLOC_H
#define ALLOC_H

// ------------------------------------ //

static void* alloc_virtual_page(u64 size);
static void  free_virtual_page(void* page, u64 size);

// ------------------------------------ //

typedef struct Stack {
	byte* head;
	byte* tail;
} Stack;

static Stack make_stack(u64 size);
static void* stack_alloc(u64 size);

// ------------------------------------ //
//
// General purpose allocators usually used for arrays.
// ... try to avoid these.

static void* alloc(u64 size);
static void  free(void* p, u64 size);
static void* realloc(void* p, u64 oldsize, u64 newsize);
static void* copyalloc(void* p, u64 size);
static void* copyalloc_expand(void* p, u64 old_size, u64 new_size);
#define real_stack_alloc(size) __builtin_alloca(size)

// ------------------------------------ //

#endif // ALLOC_H

