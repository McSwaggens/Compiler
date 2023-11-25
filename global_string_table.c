#include "global_string_table.h"

typedef struct GlobalStringTable_StringNode GlobalStringTable_StringNode;
typedef struct GlobalStringTable_LengthWise GlobalStringTable_LengthWise;
typedef struct GlobalStringTable_CharWise   GlobalStringTable_CharWise;
typedef struct GlobalStringTable            GlobalStringTable;

struct GlobalStringTable_LengthWise {
	char** strings;
	u32 count;
};

struct GlobalStringTable_StringNode {
	GlobalStringTable_StringNode* next;
	String string;
	// Hash?
};

struct GlobalStringTable_CharWise {
	GlobalStringTable_LengthWise lengthwise[32];
	GlobalStringTable_StringNode* large_strings_head;
};

struct GlobalStringTable {
	GlobalStringTable_CharWise charwise[256];
};

static GlobalStringTable global_string_table = { 0 }; // sizeof(GlobalStringTable) = 96k !!!

static const char ascii_string[256] = { // Yucky, however it gives a unique address for all strings with length 1
	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,
	33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,
	64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,
	96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,
	128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,
	160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,
	192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,
	224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,247,248,249,250,251,252,253,254,255
};

static char* gst_lookup(char* s, u64 length) {
	assert(is_alpha(*s));
	assert(length);

	if (length == 1) {
		// print("gst_lookup(%, %): ascii_string lut.\n",
		// 	arg_string((String){ s, length }),
		// 	arg_u64(length)
		// );
		return (char*)ascii_string + *s;
	}

	GlobalStringTable_CharWise* cw = &global_string_table.charwise[*s];

	if (length < 32+2) {
		GlobalStringTable_LengthWise* lw = &cw->lengthwise[length-2];

		for (u32 i = 0; i < lw->count; i++) {
			char* str = lw->strings[i];

			if (!compare(str, s, length))
				continue;

			// print("gst_lookup(%, %): Found string using lengthwise lookup. misses = %.\n",
			// 	arg_string((String){ s, length }),
			// 	arg_u64(length),
			// 	arg_u32(i)
			// );

			return str;
		}

		lw->strings = realloc(lw->strings, sizeof(char*)*(lw->count), sizeof(char*)*(lw->count+8));
		lw->strings[lw->count++] = s;

		// print("gst_lookup(%, %): Added new lengthwise string. misses = %.\n",
		// 	arg_string((String){ s, length }),
		// 	arg_u64(length),
		// 	arg_u32(lw->count-1)
		// );

		return s;
	}

	u64 n = 0;

	GlobalStringTable_StringNode** pp = &cw->large_strings_head;
	for (; *pp; pp = &(*pp)->next, n++) {
		GlobalStringTable_StringNode* node = *pp;

		// print("gst_lookup(%, %): Trying node: length = %.\n",
		// 	arg_string((String){ s, length }),
		// 	arg_u64(length),
		// 	arg_u64(node->string.length)
		// );

		if (node->string.length < length)
			continue;

		if (node->string.length > length)
			break;

		if (!compare(s, node->string.data, length))
			continue;

		// print("gst_lookup(%, %): Found string using linked list. misses = %.\n",
		// 	arg_string((String){ s, length }),
		// 	arg_u64(length),
		// 	arg_u32(n)
		// );

		return node->string.data;
	}

	GlobalStringTable_StringNode* next = *pp;
	*pp = alloc(sizeof(GlobalStringTable_StringNode));
	**pp = (GlobalStringTable_StringNode){
		.string = (String){ .data = s, .length = length },
		.next = next
	};

	// print("gst_lookup(%, %): Added new string to linked list. misses = %.\n",
	// 	arg_string((String){ s, length }),
	// 	arg_u64(length),
	// 	arg_u32(n)
	// );

	// print("Chain:\n");

	// for (GlobalStringTable_StringNode* node = cw->large_strings_head; node; node = node->next)
	// 	print("\t%, %\n", arg_string(node->string), arg_u64(node->string.length));

	return s;
}

