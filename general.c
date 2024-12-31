#include "general.h"
#include "print.h"
#include "math.c"
#include "alloc.h"

static u64 count_cstring(const char* s) {
	const char* begin = s;
	while (*s) ++s;
	return s - begin;
}

static inline u64 hash64(u64 n) {
	return n * 11400714819323198485llu; // (2^64)/((1+sqrt(5))/2)
}

static byte* array_insert_single(byte* array, u64 element_count, u64 element_size, byte* insertion_data, u64 insertion_index) {
	u64 total_array_size = element_count * element_size;
	array = realloc(array, total_array_size, total_array_size + element_size);

	byte* insertion_ptr = array + insertion_index * element_size;
	move(insertion_ptr, insertion_ptr + element_size, (element_count - insertion_index) * element_size);
	copy(insertion_ptr, insertion_data, element_size);

	return array;
}

static byte* array_insert_multi(byte* array, u64 element_count, u64 element_size, byte** insertion_datas, u64* insertion_indices) {
	// @todo
	return null;
}

