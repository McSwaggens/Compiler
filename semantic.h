#ifndef SEMANTIC_H_INCLUDED
#define SEMANTIC_H_INCLUDED

typedef struct ScanHelper ScanHelper;

struct ScanHelper {
	struct Branch* loop; // Looping branch that the scanner is currently inside of. null if not in loop.
	struct Module* module;
};

static void scan_module(struct Module* module);

#endif // SEMANTIC_H_INCLUDED

