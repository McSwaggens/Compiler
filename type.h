#ifndef TYPE_H
#define TYPE_H

#include "general.h"

typedef struct Type Type;
typedef enum TypeKind TypeKind;
typedef enum PrimitiveType PrimitiveType;
typedef u32 TypeID;

#define TYPEID_KIND_BITS 3
#define TYPEID_INDEX_BITS (sizeof(TypeID)*8 - TYPEID_KIND_BITS)

enum TypeKind {
	TYPE_KIND_PRIMITIVE = 0,
	TYPE_KIND_PTR       = 1,
	TYPE_KIND_ARRAY     = 2,
	TYPE_KIND_FIXED     = 3,
	TYPE_KIND_STRUCT    = 4,
	TYPE_KIND_ENUM      = 5,
	TYPE_KIND_TUPLE     = 6,
	TYPE_KIND_FUNCTION  = 7,
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

static void ts_init(void);

static inline TypeKind ts_get_kind(TypeID id);
static inline u32      ts_get_index(TypeID id);
static inline Type*    ts_get_type(TypeID id);
static inline u64      ts_get_size(TypeID id);
static inline u64      ts_get_tuple_length(TypeID type);
static inline u64      ts_get_fixed_length(TypeID type);

static TypeID ts_get_ptr(TypeID typeid);
static TypeID ts_get_array(TypeID typeid);
static TypeID ts_get_func(TypeID input, TypeID output);
static TypeID ts_get_tuple(TypeID* types, u64 count);
static TypeID ts_get_fixed(TypeID typeid, u64 length);
static TypeID ts_get_subtype(TypeID type);
static TypeID ts_get_integral_type(TypeID type);
static TypeID ts_get_unsigned(TypeID type);
static TypeID ts_get_signed(TypeID type);

static bool ts_is_int(TypeID type);
static bool ts_is_unsigned(TypeID type);
static bool ts_is_signed(TypeID type);
static bool ts_is_float(TypeID type);
static bool ts_is_ptr(TypeID type);
static bool ts_is_integral_type(TypeID type);
static bool ts_is_specifier(TypeID type);

#endif // TYPE_H

