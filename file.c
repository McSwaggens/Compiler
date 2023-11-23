FileHandle32 open_file(String path, FileMode mode, FileAccessFlags access_flags) {
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

void close_file(FileHandle32 handle) {
	system_call(LINUX_SYSCALL_CLOSE, handle, 0, 0, 0, 0, 0);
}

s64 query_file_size(FileHandle32 handle) {
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

	system_call(LINUX_SYSCALL_FSTATUS, handle, (s64)&status, 0, 0, 0, 0);
	return status.size;
}

u64 read_file(FileHandle32 handle, void* dest, u64 size) {
	return system_call(LINUX_SYSCALL_READ, handle, (s64)dest, size, 0, 0, 0);
}

void write_file(FileHandle32 handle, const void* data, u64 length) {
	system_call(LINUX_SYSCALL_WRITE, handle, (s64)data, length, 0, 0, 0);
}

FileData load_file(FileHandle32 handle, u32 padding) {
	FileData result = { 0 };

	u64 file_size = query_file_size(handle);
	result.data = alloc_virtual_page(file_size + padding);
	result.size = read_file(handle, result.data, file_size);
	zero(result.data+result.size, 8);

	return result;
}

bool does_file_exist(String path) {
	char cpath[path.length+1];
	copy(cpath, path.data, path.length);
	cpath[path.length] = 0;
	// F=0, X=1, W=2, R=4
	return system_call(LINUX_SYSCALL_ACCESS, (s64)cpath, path.length, 0, 0, 0, 0);
}

void flush_output_buffer(OutputBuffer* buffer) {
	if (!buffer->head)
		return;

	write_file(buffer->file_handle, buffer->buffer, buffer->head);
	buffer->flush_count++;
	buffer->flushed_bytes += buffer->head;
	buffer->head = 0;
}

void write_output_buffer(OutputBuffer* buffer, const byte* data, u64 length) {
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

void write_output_buffer_b(OutputBuffer* buffer, byte b) {
	if (buffer->head >= OUTPUT_BUFFER_SIZE)
		flush_output_buffer(buffer);

	buffer->buffer[buffer->head] = b;
	buffer->head += 1;
	buffer->written_bytes += 1;
}

