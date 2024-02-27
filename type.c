#include "type.h"

#define TYPE_TABLE_SIZE (1llu<<TYPEID_INDEX_BITS)
// @Note: ELF .zero pages are mapped but aren't initially backed by physical memory.
static Type ts_table[TYPE_TABLE_SIZE] = { 0 };
static u32 ts_table_head;

static inline TypeKind ts_get_kind(TypeID  id) { return id >> TYPEID_INDEX_BITS; }
static inline u32      ts_get_index(TypeID id) { return id & ((1llu<<TYPEID_INDEX_BITS)-1);  }
static inline Type*    ts_get_type(TypeID id)       { return ts_table + ts_get_index(id); }
static inline u64      ts_get_size(TypeID id)  { return ts_get_type(id)->size; }

static void ts_init(void) {
	ts_table_head= LARGEST_CORE_TYPE_INDEX+1;

	*ts_get_type(TYPE_BYTE)    = (Type){ .size = 1 };
	*ts_get_type(TYPE_BOOL)    = (Type){ .size = 1 };
	*ts_get_type(TYPE_TYPEID)  = (Type){ .size = 8 };
	*ts_get_type(TYPE_INT8)    = (Type){ .size = 1 };
	*ts_get_type(TYPE_INT16)   = (Type){ .size = 2 };
	*ts_get_type(TYPE_INT32)   = (Type){ .size = 4 };
	*ts_get_type(TYPE_INT64)   = (Type){ .size = 8 };
	*ts_get_type(TYPE_UINT8)   = (Type){ .size = 1 };
	*ts_get_type(TYPE_UINT16)  = (Type){ .size = 2 };
	*ts_get_type(TYPE_UINT32)  = (Type){ .size = 4 };
	*ts_get_type(TYPE_UINT64)  = (Type){ .size = 8 };
	*ts_get_type(TYPE_FLOAT32) = (Type){ .size = 4 };
	*ts_get_type(TYPE_FLOAT64) = (Type){ .size = 8 };

	*ts_get_type(ts_get_index(TYPE_EMPTY_TUPLE)) = (Type){ .size = 0, .length = 0 };
}

static inline u64 ts_get_tuple_length(TypeID type) {
	assert(type);
	assert(ts_get_kind(type) == TYPE_KIND_TUPLE);
	return ts_get_type(type)->length;
}

static inline u64 ts_get_fixed_length(TypeID type) {
	assert(type);
	assert(ts_get_kind(type) == TYPE_KIND_FIXED);
	return ts_get_type(type)->length;
}

static inline TypeID ts_create(TypeKind kind, Type value) {
	TypeID id = MAKE_TYPEID(kind, ts_table_head++);
	Type* type = ts_get_type(id);
	*type = value;
	return id;
}

static void xtable_push(XTable* table, TypeExtent ext) {
	if (is_pow2(table->length+1)) {
		table->exts = realloc(
			table->exts,
			sizeof(TypeExtent) * table->length,
			sizeof(TypeExtent) * ((table->length+1)*2)
		);
	}

	table->exts[table->length++] = ext;
}

static TypeID ts_get_ptr(TypeID subtype) {
	assert(subtype);

	Type* subinfo = ts_get_type(subtype);

	if (subinfo->ptr)
		return subinfo->ptr;

	TypeID newid = ts_create(TYPE_KIND_PTR, (Type){
		.subtype = subtype,
		.size = 8,
	});

	subinfo->ptr = newid;

	return newid;
}

static TypeID ts_get_array(TypeID subtype) {
	assert(subtype);

	Type* subinfo = ts_get_type(subtype);

	if (subinfo->array)
		return subinfo->array;

	TypeID newid = ts_create(TYPE_KIND_ARRAY, (Type){
		.subtype = subtype,
		.size = 16,
	});

	subinfo->array = newid;

	return newid;
}

// If get_function_type or get_tuple_type lookups are too slow we should start using binary search
static TypeID ts_get_func(TypeID input, TypeID output) {
	assert(input);
	assert(output);

	Type* input_info  = ts_get_type(input);
	Type* output_info = ts_get_type(output);

	XTable* xtable = &input_info->xtable;
	for (u64 i = 0; i < xtable->length; i++) {
		TypeExtent* ext = &xtable->exts[i];

		if (ext->output != output)
			continue;

		if (ts_get_kind(ext->type) != TYPE_KIND_FUNCTION)
			continue;

		return ext->type;
	}

	TypeID newid = ts_create(TYPE_KIND_FUNCTION, (Type){
		.input  = input,
		.output = output,
		.size   = 0,
	});

	xtable_push(xtable, (TypeExtent){
		.output = output,
		.type   = newid
	});

	return newid;
}

static TypeID ts_get_tuple(TypeID* types, u64 count) {
	if (!count)
		return TYPE_EMPTY_TUPLE;

	TypeID head_id = types[0];
	Type* head_info = ts_get_type(head_id);

	if (count == 1)
		return head_id;

	XTable* xtable = &head_info->xtable;

	for (u64 i = 0; i < xtable->length; i++) {
		TypeExtent* ext = &xtable->exts[i];

		if (ext->length != count)
			continue;

		if (ext->output != types[1])
			continue;

		if (ts_get_kind(ext->type) != TYPE_KIND_TUPLE)
			continue;

		if (count > 2 && !compare(types+2, ts_get_type(ext->type)->elements+2, (count-2)*sizeof(TypeID)))
			continue;

		return ext->type;
	}

	TypeID* elems = alloc(count*sizeof(TypeID));
	u64 size = 0;
	for (u64 i = 0; i < count; i++) {
		elems[i] = types[i];
		size += ts_get_size(types[i]);
	}

	TypeID newid = ts_create(TYPE_KIND_TUPLE, (Type){
		.size     = size,
		.length   = count,
		.elements = elems,
	});

	xtable_push(xtable, (TypeExtent){
		.length = count,
		.type   = newid,
		.output = types[1],
	});

	return newid;
}

static TypeID ts_get_fixed(TypeID subtype, u64 length) {
	assert(subtype);
	assert(length);

	Type* subinfo = ts_get_type(subtype);

	XTable* xtable = &subinfo->xtable;
	for (u64 i = 0; i < xtable->length; i++) {
		TypeExtent* ext = xtable->exts+i;

		if (ext->length != length)
			continue;

		if (ts_get_kind(ext->type) != TYPE_KIND_FIXED)
			continue;

		return ext->type;
	}

	u64 size = length * ts_get_size(subtype);

	TypeID newid = ts_create(TYPE_KIND_FIXED, (Type){
		.size     = size,
		.length   = length,
		.subtype  = subtype,
	});

	xtable_push(xtable, (TypeExtent){
		.length = length,
		.output = subtype,
		.type   = newid,
	});

	return newid;
}

static TypeID ts_get_subtype(TypeID type) {
	TypeKind kind = ts_get_kind(type);
	assert(ts_is_specifier(type));
	Type* p = ts_get_type(type);
	return p->subtype;
}

static bool ts_is_int(TypeID type) {
	return ts_is_signed(type) || ts_is_unsigned(type);
}

static bool ts_is_unsigned(TypeID type) {
	return type >= TYPE_UINT8 && type <= TYPE_UINT64;
}

static bool ts_is_signed(TypeID type) {
	return type >= TYPE_INT8 && type <= TYPE_INT64;
}

static bool ts_is_float(TypeID type) {
	return type >= TYPE_FLOAT32 && type <= TYPE_FLOAT64;
}

static bool ts_is_ptr(TypeID type) {
	return ts_get_kind(type) == TYPE_KIND_PTR;
}

static bool ts_is_specifier(TypeID type) {
	switch (ts_get_kind(type)) {
		default:
			// print("ts_is_specifier(%)\n", arg_type(type));
			assert_unreachable();

		case TYPE_KIND_PTR:
		case TYPE_KIND_ARRAY:
		case TYPE_KIND_FIXED:
			return true;

		case TYPE_KIND_STRUCT:
		case TYPE_KIND_ENUM:
		case TYPE_KIND_TUPLE:
		case TYPE_KIND_FUNCTION:
		case TYPE_KIND_PRIMITIVE:
			return false;
	}
}

