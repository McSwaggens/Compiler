#ifndef GLOBAL_STRING_TABLE_H
#define GLOBAL_STRING_TABLE_H

// GlobalStringTable coalesces strings of the same value with the same address.
// This allows for string compares with just the ptr which is usefull later on in the compiler when identifiers are begin compared alot.
static char* gst_lookup(char* s, u64 length);

#endif // GLOBAL_STRING_TABLE_H

