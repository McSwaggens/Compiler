static void close_file(FileHandle32 handle) {
	system_call(LINUX_SYSCALL_CLOSE, handle, 0, 0, 0, 0, 0);
}

static void alloc_virtual_page(u64 size) {
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

static FileHandle32 open_file(String path, FileMode mode, FileAccessFlags access_flags) {
	if (path.length >= 4096)
		return FILE_HANDLE_INVALD;

	s64 flags = 0;

	switch (access_flags) {
		case FILE_ACCESS_FLAG_READ:                          flags = 0; break;
		case FILE_ACCESS_FLAG_WRITE:                         flags = 1; break;
		case FILE_ACCESS_FLAG_READ | FILE_ACCESS_FLAG_WRITE: flags = 2; break;
	}

	switch (mode) {
		case FILE_MODE_OPEN:                                                                  break;
		case FILE_MODE_DIRECTORY:          flags |= LINUX_OPEN_DIRECTORY;                     break;
		case FILE_MODE_APPEND:             flags |= LINUX_OPEN_APPEND;                        break;
		case FILE_MODE_TRUNCATE:           flags |= LINUX_OPEN_TRUNCATE;                      break;
		case FILE_MODE_CREATE:             flags |= LINUX_OPEN_CREATE | LINUX_OPEN_EXCLUSIVE; break;
		case FILE_MODE_CREATE_OR_OPEN:     flags |= LINUX_OPEN_CREATE;                        break;
		case FILE_MODE_CREATE_OR_APPEND:   flags |= LINUX_OPEN_CREATE | LINUX_OPEN_APPEND;    break;
		case FILE_MODE_CREATE_OR_TRUNCATE: flags |= LINUX_OPEN_CREATE | LINUX_OPEN_TRUNCATE;  break;
	}

	char cpath[path.length+1];
	copy(cpath, path.data, path.length);
	cpath[path.length] = 0;

	s64 perm = 0x180;

	FileHandle32 handle = system_call(LINUX_SYSCALL_OPEN, (s64)cpath, flags, perm, 0, 0, 0);

	return handle;
}

static bool does_file_exist(String path) {
	char cpath[path.length+1];
	copy(cpath, path.data, path.length);
	cpath[path.length] = 0;
	// F=0, X=1, W=2, R=4
	return system_call(LINUX_SYSCALL_ACCESS, (s64)cpath, path.length, 0, 0, 0, 0);
}

static u64 read_file(FileHandle32 handle, void* dest, u64 size) {
	return system_call(LINUX_SYSCALL_READ, handle, (s64)dest, size, 0, 0, 0);
}

static void write_file(FileHandle32 handle, const void* data, u64 length) {
	system_call(LINUX_SYSCALL_WRITE, handle, (s64)data, length, 0, 0, 0);
}

static s64 query_file_size(FileHandle32 handle) {
	struct Status {
		u64 dev;
		u64 ino;
		u64 nlink;

		u32 mode;
		u32 uid;
		u32 gid;
		u32 padding0;

		u64 rdev;
		s64 size;
		s64 block_size;
		s64 blocks;

		u64 atime;
		u64 atime_nsec;
		u64 mtime;
		u64 mtime_nsec;
		u64 ctime;
		u64 ctime_nsec;

		s64 padding1[3];
	} status;

	// print("sizeof(Status) = %\n", arg_u64(sizeof(struct Status)));

	system_call(LINUX_SYSCALL_FSTATUS, handle, (s64)&status, 0, 0, 0, 0);
	return status.size;
}

static void exit_program(void) {
	system_call(LINUX_SYSCALL_EXIT, 0, 0, 0, 0, 0, 0);
}

