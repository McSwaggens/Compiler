#include "general.h"
#include "print.h"
#include "linux.h"
#include "math.c"

// ------------------------------------ //

void exit_program(void) {
	system_call(LINUX_SYSCALL_EXIT, 0, 0, 0, 0, 0, 0);
}

// ------------------------------------ //

u64 count_cstring(const char* s) {
	const char* begin = s;
	while (*s) ++s;
	return s - begin;
}

// ------------------------------------ //

Linux_SysInfo linux_poll_sysinfo(void) {
	Linux_SysInfo info = { 0 };
	system_call(LINUX_SYSCALL_SYSINFO, (u64)&info, 0, 0, 0, 0, 0);
	return info;
}

u64 get_system_ram(void) {
	Linux_SysInfo info = linux_poll_sysinfo();
	return info.ram_total * info.memory_unit;
}

