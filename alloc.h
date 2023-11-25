#ifndef ALLOC_H
#define ALLOC_H

// ------------------------------------ //

void* alloc_virtual_page(u64 size);
void  free_virtual_page(void* page, u64 size);

// ------------------------------------ //

typedef struct Stack {
	byte* head;
	byte* tail;
} Stack;

Stack make_stack(u64 size);
void* stack_alloc(u64 size);

// ------------------------------------ //
//
// General purpose allocators usually used for arrays.
// ... try to avoid these.

void* alloc(u64 size);
void  free(void* p, u64 size);
void* realloc(void* p, u64 oldsize, u64 newsize);

// ------------------------------------ //

#endif // ALLOC_H

