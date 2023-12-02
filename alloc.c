#include "alloc.h"

// ------------------------------------ //

static void* alloc_virtual_page(u64 size) {
	void* page = (void*)system_call(
		LINUX_SYSCALL_MMAP,
		0,
		(size+4095) & -4096, // Raise to page size
		LINUX_MMAP_PROTECTION_READ | LINUX_MMAP_PROTECTION_WRITE,
		0x22,
		-1,
		0
	);

	return page;
}

static void free_virtual_page(void* page, u64 size) {
	system_call(LINUX_SYSCALL_MUNMAP, (s64)page, size, 0, 0, 0, 0);
}

// ------------------------------------ //

static Stack stack;

static Stack make_stack(u64 size) {
	size = round_to_nearest_mulpow2(size, 4086);
	byte* p = alloc_virtual_page(size);

	return (Stack){
		.head = p,
		.tail = p + size
	};
}

static void* stack_alloc(u64 size) {
	if (stack.head+size > stack.tail)
		stack = make_stack(max_u64(size<<9, (stack.tail-stack.head)*2));

	void* result = stack.head;
	stack.head += size;

	return result;
}

// ------------------------------------ //

typedef struct GlobalAllocator_Pool {
	// linked list
	void* ll_head;

	// stack
	byte* shead;
	byte* stail;
} GlobalAllocator_Pool;

struct GlobalAllocator {
	u64 plot;
	GlobalAllocator_Pool pools[64];
} static global_allocator;

static GlobalAllocator_Pool* ga_get_pool(u64 pool_index) {
	return &global_allocator.pools[pool_index];
}

static bool ga_is_populated(u64 pool_index) {
	return (global_allocator.plot >> pool_index) & 1;
}

static void ga_mark_empty(u64 pool_index) {
	global_allocator.plot &= ~(1llu << pool_index);
	// print("  % marked empty\n", arg_u64(pool_index));
}

static void ga_mark_populated(u64 pool_index) {
	global_allocator.plot |= (1llu << pool_index);
}

static void ga_do_empty_check(u64 pool_index) {
	GlobalAllocator_Pool* pool = ga_get_pool(pool_index);

	if (!pool->ll_head && pool->shead == pool->stail)
		ga_mark_empty(pool_index);
}

static void ga_set_stack(u64 pool_index, void* p, u64 size) {
	GlobalAllocator_Pool* pool = ga_get_pool(pool_index);
	pool->shead = p;
	pool->stail = p + size;
}

static void ga_set_stack_and_mark(u64 pool_index, void* p, u64 size) {
	ga_set_stack(pool_index, p, size);
	ga_mark_populated(pool_index);
}

static void ga_insert(u64 pool_index, void* p) {
	GlobalAllocator_Pool* pool = ga_get_pool(pool_index);
	*(void**)p = pool->ll_head;
	pool->ll_head = p;
}

static void ga_mark_and_insert(u64 pool_index, void* p) {
	ga_insert(pool_index, p);
	ga_mark_populated(pool_index);
}

static u64 ga_get_next_best_pool(u64 pool_index) {
	// &63 is to crash 64 to 0
	return count_trailing_zeroes64(global_allocator.plot & (-2llu<<pool_index)) & 63;
}

static void* ga_take(u64 pool_index) {
	// print("ga_take(%)\n", arg_u64(pool_index));

	GlobalAllocator_Pool* pool = ga_get_pool(pool_index);

	assert(ga_is_populated(pool_index));

	if (pool->ll_head) {
		void* result = pool->ll_head;
		void* new_head = *(void**)result;
		pool->ll_head = new_head;
		ga_do_empty_check(pool_index);
		return result;
	}

	// print("  pool->shead = %, pool->stail = %, diff = %\n", arg_u64((u64)pool->shead), arg_u64((u64)pool->stail), arg_u64(pool->stail-pool->shead));
	assert(pool->shead < pool->stail);

	void* result = pool->shead;
	pool->shead += (1 << pool_index);
	ga_do_empty_check(pool_index);

	return result;
}

static void ga_splat(u64 top_index, u64 bottom_index) {
	assert(top_index != bottom_index);
	assert(top_index > bottom_index);
	assert(ga_is_populated(top_index));

	u64 original_bottom_index = bottom_index;
	bottom_index = max_u64(bottom_index, 12);

	// print("ga_splat(%, % -> %)\n", arg_u64(top_index), arg_u64(original_bottom_index), arg_u64(bottom_index));

	// u64 plot_before = global_allocator.plot;
	global_allocator.plot |= ((1llu<<top_index)-1) & (-1llu<<bottom_index);

	// if ((global_allocator.plot & 4096) && !(plot_before & 4096))
	// 	print("4096 was marked by splat\n");

	byte* p = ga_take(top_index);
	u64 size = (1llu<<top_index);

	for (u64 i = top_index-1; i >= bottom_index; i--) {
		ga_set_stack(i, p, size >> 1);
		size >>= 1;
		// print("  Splatted %k to %\n", arg_u64(size>>10), arg_u64(i));
		p += size;
	}

	// print("  Bottom(%) got %k leftovers\n", arg_u64(original_bottom_index), arg_u64(size>>10));
	ga_set_stack_and_mark(original_bottom_index, p, size);
}

static u64 ga_correct_size(u64 size) {
	return size ? round_pow2((size+7)&-8) : 0;
}

static u64 ga_size_to_index(u64 size) {
	assert(is_pow2(size));
	return count_trailing_zeroes64(size);
}

static void init_global_allocator(void) {
	zero(&global_allocator, sizeof(struct GlobalAllocator));
}

static void* alloc(u64 size) {
	// print("alloc(% -> % -> 2^%)\n", arg_u64(size), arg_u64(ga_correct_size(size)), arg_u64(count_trailing_zeroes64(ga_correct_size(size))));

	if (!size)
		return null;

	size = ga_correct_size(size);
	u64 pool_index = ga_size_to_index(size);

	if (ga_is_populated(pool_index))
		return ga_take(pool_index);

	u64 next_best_index = ga_get_next_best_pool(pool_index);

	if (!next_best_index) {
		next_best_index = max_u64(30, pool_index + 3);
		u64 new_page_size = 1llu<<next_best_index;
		// print("New page added to %\n", arg_u64(next_best_index));
		void* p = alloc_virtual_page(new_page_size);
		ga_set_stack_and_mark(next_best_index, p, new_page_size);
	}

	ga_splat(next_best_index, pool_index);
	void* result = ga_take(pool_index);
	assert(result);

	return result;
}

static void free(void* p, u64 size) {
	assert(p);
	assert(size);

	size = ga_correct_size(size);
	u64 pool_index = ga_size_to_index(size);

	ga_mark_and_insert(pool_index, p);
}

static void* realloc(void* p, u64 oldsize, u64 newsize) {
	u64 oldsize_corrected = ga_correct_size(oldsize);
	u64 newsize_corrected = ga_correct_size(newsize);

	if (oldsize_corrected == newsize_corrected)
		return p;

	void* new_alloc = alloc(newsize_corrected);

	if (p) {
		assert(oldsize);
		copy(new_alloc, p, min_u64(oldsize, newsize));
		free(p, oldsize_corrected);
	}

	return new_alloc;
}

// ------------------------------------ //
