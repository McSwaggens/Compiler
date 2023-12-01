#ifndef FILE_H
#define FILE_H

#include "general.h"

typedef s64 FileAccessFlags;
#define FILE_ACCESS_FLAG_READ  0x1
#define FILE_ACCESS_FLAG_WRITE 0x2

typedef s32 FileHandle32;
#define FILE_HANDLE_INVALD               -1
#define FILE_HANDLE_UNIX_STANDARD_INPUT   0
#define FILE_HANDLE_UNIX_STANDARD_OUTPUT  1
#define FILE_HANDLE_UNIX_STANDARD_ERROR   2

typedef enum FileMode {
	FILE_MODE_OPEN,     // Open an existing file.
	FILE_MODE_APPEND,   // Open an existing file and go to the end.
	FILE_MODE_TRUNCATE, // Open and truncate an existing file.
	FILE_MODE_CREATE,   // Create a file that doesn't already exist.
						// ProTip: Believe it or not; there is an 'e' at the end of 'create'!

	FILE_MODE_CREATE_OR_OPEN,     // Open or create a file.
	FILE_MODE_CREATE_OR_APPEND,   // Create file if it doesn't already exist, otherwise go to the end.
	FILE_MODE_CREATE_OR_TRUNCATE, // Truncate an existing file, otherwise create one.
} FileMode;

#define OUTPUT_BUFFER_SIZE (4 << 12)

typedef struct OutputBuffer {
	u64 head;
	u64 flush_count;
	u64 flushed_bytes;
	u64 written_bytes;
	FileHandle32 file_handle;
	byte buffer[OUTPUT_BUFFER_SIZE];
} OutputBuffer;

typedef struct FileData {
	byte* data;
	u64 size;
} FileData;

static OutputBuffer standard_output_buffer = { .file_handle = FILE_HANDLE_UNIX_STANDARD_OUTPUT };

static FileHandle32 open_file(String path, FileMode mode, FileAccessFlags access_flags);
static FileData load_file(FileHandle32 handle, u32 padding);
static void close_file(FileHandle32 handle);
static s64  query_file_size(FileHandle32 handle);
static u64  read_file(FileHandle32 handle, void* dest, u64 size);
static void write_file(FileHandle32 handle, const void* data, u64 length);
static bool does_file_exist(String path);
static void flush_output_buffer(OutputBuffer* buffer);
static void write_output_buffer(OutputBuffer* buffer, const byte* data, u64 length);
static void write_output_buffer_b(OutputBuffer* buffer, byte b);

#endif // FILE_H

