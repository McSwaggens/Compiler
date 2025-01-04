#include <sys/sysinfo.h>

#define LINUX

#define OS_PAGE_SIZE (16 << 10)

static u64 get_system_ram(void) {
	struct sysinfo info;
	int x = sysinfo(&info);
	return info.totalram * info.mem_unit;
}

