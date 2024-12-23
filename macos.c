#include <sys/sysctl.h>

#define MACOS

static u64 get_system_ram(void) {
	s32 xxx[2] = { CTL_HW, HW_MEMSIZE };
	u64 memory_size;
	u64 out_size = 8;
	sysctl(xxx, 2, &memory_size, (unsigned long*)&out_size, 0, 0);
	return memory_size;
}

