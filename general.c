#include "general.h"
#include "print.h"
#include "math.c"

static u64 count_cstring(const char* s) {
	const char* begin = s;
	while (*s) ++s;
	return s - begin;
}

static inline u64 hash64(u64 n) {
	return n * 11400714819323198485llu; // (2^64)/((1+sqrt(5))/2)
}

