#include <sys/sysinfo.h>

#define LINUX

static u64 get_system_ram(void) {
	struct sysinfo info;
	int x = sysinfo(&info);
	return info.totalram * info.mem_unit;
}

