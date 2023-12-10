#include "general.h"
#include "print.h"
#include "linux.h"
#include "math.c"

// ------------------------------------ //

static void exit_program(void) {
	system_call(LINUX_SYSCALL_EXIT, 0, 0, 0, 0, 0, 0);
}

// ------------------------------------ //

static u64 count_cstring(const char* s) {
	const char* begin = s;
	while (*s) ++s;
	return s - begin;
}

// ------------------------------------ //

static Linux_SysInfo linux_poll_sysinfo(void) {
	Linux_SysInfo info = { 0 };
	system_call(LINUX_SYSCALL_SYSINFO, (u64)&info, 0, 0, 0, 0, 0);
	return info;
}

static u64 get_system_ram(void) {
	Linux_SysInfo info = linux_poll_sysinfo();
	return info.ram_total * info.memory_unit;
}

// ------------------------------------ //

// void __stack_chk_fail(void) {
// }

// void* memset(void* s, int c, long int n) {
// 	char* p = s;

// 	for (u64 i = 0; i < n; i++) {
// 		p[i] = c;
// 	}

// 	return s;
// }

// int memcmp(void* s1, void* s2, long int length) {
// 	return __builtin_memcmp(s1, s2, 16);
// }

// void memcpy(void* restrict dest, const void* restrict src, long int n) {
// 	// @Todo: Allignment

// 	char* restrict d = dest;
// 	const char* restrict s = src;

// 	for (u64 i = 0; i < (n>>6); i++) {
// 		__builtin_memcpy_inline(d, s, 64);
// 		d += 64;
// 		s += 64;
// 	}

// 	if (n & 32) {
// 		__builtin_memcpy_inline(d, s, 32);
// 		d += 32;
// 		s += 32;
// 	}

// 	if (n & 16) {
// 		__builtin_memcpy_inline(d, s, 16);
// 		d += 16;
// 		s += 16;
// 	}

// 	if (n & 8) {
// 		__builtin_memcpy_inline(d, s, 8);
// 		d += 8;
// 		s += 8;
// 	}

// 	if (n & 4) {
// 		__builtin_memcpy_inline(d, s, 4);
// 		d += 4;
// 		s += 4;
// 	}

// 	if (n & 2) {
// 		__builtin_memcpy_inline(d, s, 2);
// 		d += 2;
// 		s += 2;
// 	}

// 	if (n & 1) {
// 		__builtin_memcpy_inline(d, s, 1);
// 		d += 1;
// 		s += 1;
// 	}
// }

