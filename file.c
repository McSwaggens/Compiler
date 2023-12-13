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

static void close_file(FileHandle32 handle) {
	system_call(LINUX_SYSCALL_CLOSE, handle, 0, 0, 0, 0, 0);
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

static u64 read_file(FileHandle32 handle, void* dest, u64 size) {
	return system_call(LINUX_SYSCALL_READ, handle, (s64)dest, size, 0, 0, 0);
}

static void write_file(FileHandle32 handle, const void* data, u64 length) {
	system_call(LINUX_SYSCALL_WRITE, handle, (s64)data, length, 0, 0, 0);
}

static FileData load_file(FileHandle32 handle, u32 padding) {
	FileData result = { 0 };

	u64 file_size = query_file_size(handle);
	result.data = alloc_virtual_page(file_size + padding);
	result.size = read_file(handle, result.data, file_size);
	zero(result.data+result.size, 8);

	return result;
}

static bool does_file_exist(String path) {
	char cpath[path.length+1];
	copy(cpath, path.data, path.length);
	cpath[path.length] = 0;
	// F=0, X=1, W=2, R=4
	return system_call(LINUX_SYSCALL_ACCESS, (s64)cpath, path.length, 0, 0, 0, 0);
}

// ------------------------------------ //

typedef struct Linux_DirectoryEntry64 {
	u64  inode;
	s64  offset;
	u16  length;
	u8   type;
	char name[];
} Linux_DirectoryEntry64;

void experiment_virtual_pages(void) {
	u64* a = (u64*)system_call(LINUX_SYSCALL_MMAP, 0, 4096, 3, 0x21, -1, 0);
	*a = 1;
	print("*a = %, address = %\n", arg_u64(*a), arg_u64((u64)a));

	u64* b = (u64*)system_call(LINUX_SYSCALL_MMAP, 0, 4096, 3, 0x21, -1, 0);
	*b = 2;
	print("*b = %, address = %\n", arg_u64(*b), arg_u64((u64)b));

	u64* c = (u64*)system_call(LINUX_SYSCALL_MREMAP, (u64)b, 4096, 4096, 1|2|4, (u64)a, 0);

	print("*a = %, address = %\n", arg_u64(*a), arg_u64((u64)a));
	print("*b = %, address = %\n", arg_u64(*b), arg_u64((u64)b));

	*b = 3;
	print("*a = %, address = %\n", arg_u64(*a), arg_u64((u64)a));
	print("*b = %, address = %\n", arg_u64(*b), arg_u64((u64)b));
}

static void list_dir(char* dir) {
	u64 dirlen = count_cstring(dir);

	if (!dirlen) {
		dir = "./";
		dirlen = 2;
	}

	FileHandle32 dir_handle = open_file(tostr(dir), FILE_MODE_DIRECTORY, FILE_ACCESS_FLAG_READ);

	if (dir_handle == -1) {
		print("Invalid directory file handle.\n");
		return;
	}

	#define BUFFER_SIZE 8192
	char buffer[BUFFER_SIZE] = { 0 };
	char* p;

	u64 loc = 0;
	while (true) {
		p = buffer;
		s64 read = system_call(LINUX_SYSCALL_GETDENTS64, dir_handle, (u64)buffer, BUFFER_SIZE, 0, 0, 0);
		if (!read) break;

		if (read == -1) {
			print("Error reading dirs\n");
			return;
		}

		print("read = %\n", arg_s64(read));

		while (p < buffer+read) {
			Linux_DirectoryEntry64* entry = (Linux_DirectoryEntry64*)p;

			u64 name_len = count_cstring(entry->name);
			if (name_len >= 3 && compare(entry->name+name_len-2, ".c", 2)) {
				print("name = %, type = %, length = %, inode = %, offset = %\n",
					arg_cstring(entry->name),
					arg_u8(entry->type),
					arg_u16(entry->length),
					arg_u64(entry->inode),
					arg_s64(entry->offset)
				);

				char path[dirlen+name_len+2];
				char* g = path;
				copy(g, dir, dirlen);
				g += dirlen;

				if (g[-1] != '/') {
					*g = '/';
					g += 1;
				}

				copy(g, entry->name, name_len);
				g += name_len;
				*g = 0;
			}

			p += entry->length;
		}
	}
}

// ------------------------------------ //

static void flush_output_buffer(OutputBuffer* buffer) {
	if (!buffer->head)
		return;

	write_file(buffer->file_handle, buffer->buffer, buffer->head);
	buffer->flush_count++;
	buffer->flushed_bytes += buffer->head;
	buffer->head = 0;
}

static void write_output_buffer(OutputBuffer* buffer, const byte* data, u64 length) {
	buffer->written_bytes += length;

	if (buffer->head + length < OUTPUT_BUFFER_SIZE) {
		copy(buffer->buffer + buffer->head, data, length);
		buffer->head += length;
	}
	else if (buffer->head + length < OUTPUT_BUFFER_SIZE*2) {
		u64 precopy_size = OUTPUT_BUFFER_SIZE - buffer->head;
		copy(buffer->buffer + buffer->head, data, precopy_size);
		buffer->head = OUTPUT_BUFFER_SIZE;
		flush_output_buffer(buffer);

		u64 postcopy_size = length - precopy_size;
		copy(buffer->buffer, data+precopy_size, postcopy_size);
		buffer->head = postcopy_size;
	}
	else {
		flush_output_buffer(buffer);
		write_file(buffer->file_handle, data, length);
		buffer->flush_count++;
		buffer->flushed_bytes += length;
	}
}

static void write_output_buffer_b(OutputBuffer* buffer, byte b) {
	if (buffer->head >= OUTPUT_BUFFER_SIZE)
		flush_output_buffer(buffer);

	buffer->buffer[buffer->head] = b;
	buffer->head += 1;
	buffer->written_bytes += 1;
}

