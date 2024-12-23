#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define OS_PAGE_SIZE (16 << 10)

#include "macos.c"

static byte* unix_allocate_virtual_pages(u64 size) {
	size = round_to_nearest_mulpow2(size, OS_PAGE_SIZE);

	void* page = mmap(
		0,
		size,
		PROT_READ | PROT_WRITE,
		MAP_ANON  | MAP_PRIVATE | MAP_NORESERVE,
		-1,
		0
	);

	// assert(page != MAP_FAILED);
	return page;
}

static void unix_free_virtual_pages(byte* p, u64 size) {
	size = round_to_nearest_mulpow2(size, OS_PAGE_SIZE);
	int ret = munmap(p, size);
	assert(ret == 0);
}

bool does_file_exist(String path) {
	char cpath[path.length+1];
	copy(cpath, path.data, path.length);
	cpath[path.length] = 0;

	return access(cpath, F_OK) == 0;
}

static FileHandle32 open_file(String path, FileMode mode, FileAccessFlags access_flags) {
	if (path.length >= 4096)
		return FILE_HANDLE_INVALD;

	s64 flags = 0;

	switch (access_flags) {
		case FILE_ACCESS_FLAG_READ:                          flags = O_RDONLY; break;
		case FILE_ACCESS_FLAG_WRITE:                         flags = O_WRONLY; break;
		case FILE_ACCESS_FLAG_READ | FILE_ACCESS_FLAG_WRITE: flags = O_RDWR;   break;
	}

	switch (mode) {
		case FILE_MODE_OPEN:                                            break;
		case FILE_MODE_APPEND:             flags |= O_APPEND;           break;
		case FILE_MODE_TRUNCATE:           flags |= O_TRUNC;            break;
		case FILE_MODE_CREATE:             flags |= O_CREAT | O_EXCL;   break;
		case FILE_MODE_CREATE_OR_OPEN:     flags |= O_CREAT;            break;
		case FILE_MODE_CREATE_OR_APPEND:   flags |= O_CREAT | O_APPEND; break;
		case FILE_MODE_CREATE_OR_TRUNCATE: flags |= O_CREAT | O_TRUNC;  break;
	}

	char cpath[path.length+1];
	copy(cpath, path.data, path.length);
	cpath[path.length] = 0;

	s64 perm = 0x180;

	FileHandle32 handle = open(cpath, flags, perm);

	return handle;
}

static void close_file(FileHandle32 handle) {
	close(handle);
}

static u64 read_file(FileHandle32 handle, void* dest, u64 size) {
	return read(handle, dest, size);
}

static void write_file(FileHandle32 handle, const void* data, u64 length) {
	write(handle, data, length);
}

static s64 query_file_size(FileHandle32 handle) {
	struct stat st;
	fstat(handle, &st);
	return st.st_size;
}

static void exit_program(void) {
	_exit(0);
}

