#ifndef TYPE_H
#define TYPE_H

#include "general.h"

typedef struct Type Type;
typedef enum TypeKind TypeKind;
typedef enum PrimitiveType PrimitiveType;
typedef u32 TypeID;

#define TYPEID_KIND_BITS 4
#define TYPEID_INDEX_BITS (sizeof(TypeID)*8 - TYPEID_KIND_BITS)

enum TypeKind {
	TYPE_KIND_PRIMITIVE = 0,
	TYPE_KIND_PTR       = 1,
	TYPE_KIND_REF       = 2,
	TYPE_KIND_ARRAY     = 3,
	TYPE_KIND_FIXED     = 4,
	TYPE_KIND_STRUCT    = 5,
	TYPE_KIND_ENUM      = 6,
	TYPE_KIND_TUPLE     = 7,
	TYPE_KIND_FUNCTION  = 8,
};

enum PrimitiveType {
	TYPE_BYTE     = 1,
	TYPE_BOOL     = 2,
	TYPE_TYPEID   = 3,

	TYPE_INT8     = 4,
	TYPE_INT16    = 5,
	TYPE_INT32    = 6,
	TYPE_INT64    = 7,

	TYPE_UINT8    = 8,
	TYPE_UINT16   = 9,
	TYPE_UINT32   = 10,
	TYPE_UINT64   = 11,

	TYPE_FLOAT32  = 12,
	TYPE_FLOAT64  = 13,
};

// Using macro here because it's used in enums.
#define MAKE_TYPEID(kind, index) ((TypeID)((kind << TYPEID_INDEX_BITS) | index))

enum {
	TYPE_EMPTY_TUPLE = MAKE_TYPEID(TYPE_KIND_TUPLE, 14),
};

#define LARGEST_CORE_TYPE_INDEX 14

typedef struct TypeExtent {
	TypeID type;

	// Length for tuple.
	// Element count for tuple.
	u64 length;

	// Output type for function.
	// Second element for tuple.
	// Subtype for fixed array.
	TypeID output;
} TypeExtent;

typedef struct XTable {
	TypeExtent* exts;
	u64 length;
} XTable;

struct Type {
	TypeID ptr;
	TypeID array;
	TypeID ref;
	TypeID subtype;
	TypeID input;
	TypeID output;
	TypeID* elements;
	struct Struct* ustruct;
	struct Enum* uenum;
	u64 length; // Length of fixed array and tuple.
	u64 size;
	XTable xtable;
};

static void init_type_system(void);
static inline TypeKind get_type_kind(TypeID id);
static inline u32 get_type_index(TypeID id);
static inline Type* get_type(TypeID id);
static inline u64 get_type_size(TypeID id);
static inline u64 get_type_tuple_length(TypeID type);
static inline u64 get_type_fixed_length(TypeID type);
static TypeID get_ptr_type(TypeID typeid);
static TypeID get_ref_type(TypeID subtype);
static TypeID get_array_type(TypeID typeid);
static TypeID get_func_type(TypeID input, TypeID output);
static TypeID get_tuple_type(TypeID* types, u64 count);
static TypeID get_fixed_type(TypeID typeid, u64 length);
static TypeID get_subtype(TypeID type);
static bool is_int(TypeID type);
static bool is_unsigned(TypeID type);
static bool is_signed(TypeID type);
static bool is_float(TypeID type);
static bool is_ref(TypeID type);
static bool is_specifier(TypeID type);
#endif // TYPE_H

