#include "type.h"

#define TYPE_TABLE_SIZE (1llu<<TYPEID_INDEX_BITS)
// @Note: ELF .zero pages are mapped but aren't backed by physical memory.
static Type type_table[TYPE_TABLE_SIZE] = { 0 };
static u32 type_table_head_index;

static inline TypeKind get_type_kind(TypeID  id) { return id >> TYPEID_INDEX_BITS; }
static inline u32      get_type_index(TypeID id) { return id & ((1llu<<TYPEID_INDEX_BITS)-1);  }
static inline Type*    get_type(TypeID id)       { return type_table + get_type_index(id); }
static inline u64      get_type_size(TypeID id)  { return get_type(id)->size; }

static void ts_init(void) {
	type_table_head_index = LARGEST_CORE_TYPE_INDEX+1;

	*get_type(TYPE_BYTE)    = (Type){ .size = 1 };
	*get_type(TYPE_BOOL)    = (Type){ .size = 1 };
	*get_type(TYPE_TYPEID)  = (Type){ .size = 8 };
	*get_type(TYPE_INT8)    = (Type){ .size = 1 };
	*get_type(TYPE_INT16)   = (Type){ .size = 2 };
	*get_type(TYPE_INT32)   = (Type){ .size = 4 };
	*get_type(TYPE_INT64)   = (Type){ .size = 8 };
	*get_type(TYPE_UINT8)   = (Type){ .size = 1 };
	*get_type(TYPE_UINT16)  = (Type){ .size = 2 };
	*get_type(TYPE_UINT32)  = (Type){ .size = 4 };
	*get_type(TYPE_UINT64)  = (Type){ .size = 8 };
	*get_type(TYPE_FLOAT32) = (Type){ .size = 4 };
	*get_type(TYPE_FLOAT64) = (Type){ .size = 8 };

	*get_type(get_type_index(TYPE_EMPTY_TUPLE)) = (Type){ .size = 0, .length = 0 };
}

static inline u64 get_tuple_length(TypeID type) {
	assert(type);
	assert(get_type_kind(type) == TYPE_KIND_TUPLE);
	return get_type(type)->length;
}

static inline u64 get_fixed_length(TypeID type) {
	assert(type);
	assert(get_type_kind(type) == TYPE_KIND_FIXED);
	return get_type(type)->length;
}

static inline TypeID create_type(TypeKind kind, Type value) {
	TypeID id = MAKE_TYPEID(kind, type_table_head_index++);
	Type* type = get_type(id);
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

static TypeID get_ptr_type(TypeID subtype) {
	assert(subtype);

	Type* subinfo = get_type(subtype);

	if (subinfo->ptr)
		return subinfo->ptr;

	TypeID newid = create_type(TYPE_KIND_PTR, (Type){
		.subtype = subtype,
		.size = 8,
	});

	subinfo->ptr = newid;

	return newid;
}

static TypeID get_array_type(TypeID subtype) {
	assert(subtype);

	Type* subinfo = get_type(subtype);

	if (subinfo->array)
		return subinfo->array;

	TypeID newid = create_type(TYPE_KIND_ARRAY, (Type){
		.subtype = subtype,
		.size = 16,
	});

	subinfo->array = newid;

	return newid;
}

// If get_function_type or get_tuple_type lookups are too slow we should start using binary search
static TypeID get_func_type(TypeID input, TypeID output) {
	assert(input);
	assert(output);

	Type* input_info  = get_type(input);
	Type* output_info = get_type(output);

	XTable* xtable = &input_info->xtable;
	for (u64 i = 0; i < xtable->length; i++) {
		TypeExtent* ext = &xtable->exts[i];

		if (ext->output != output)
			continue;

		if (get_type_kind(ext->type) != TYPE_KIND_FUNCTION)
			continue;

		return ext->type;
	}

	TypeID newid = create_type(TYPE_KIND_FUNCTION, (Type){
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

static TypeID get_tuple_type(TypeID* types, u64 count) {
	if (!count)
		return TYPE_EMPTY_TUPLE;

	TypeID head_id = types[0];
	Type* head_info = get_type(head_id);

	if (count == 1)
		return head_id;

	XTable* xtable = &head_info->xtable;

	for (u64 i = 0; i < xtable->length; i++) {
		TypeExtent* ext = &xtable->exts[i];

		if (ext->length != count)
			continue;

		if (ext->output != types[1])
			continue;

		if (get_type_kind(ext->type) != TYPE_KIND_TUPLE)
			continue;

		if (count > 2 && !compare(types+2, get_type(ext->type)->elements+2, (count-2)*sizeof(TypeID)))
			continue;

		return ext->type;
	}

	TypeID* elems = alloc(count*sizeof(TypeID));
	u64 size = 0;
	for (u64 i = 0; i < count; i++) {
		elems[i] = types[i];
		size += get_type_size(types[i]);
	}

	TypeID newid = create_type(TYPE_KIND_TUPLE, (Type){
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

static TypeID get_fixed_type(TypeID subtype, u64 length) {
	assert(subtype);
	assert(length);

	Type* subinfo = get_type(subtype);

	XTable* xtable = &subinfo->xtable;
	for (u64 i = 0; i < xtable->length; i++) {
		TypeExtent* ext = xtable->exts+i;

		if (ext->length != length)
			continue;

		if (get_type_kind(ext->type) != TYPE_KIND_FIXED)
			continue;

		return ext->type;
	}

	u64 size = length * get_type_size(subtype);

	TypeID newid = create_type(TYPE_KIND_FIXED, (Type){
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

static TypeID get_subtype(TypeID type) {
	TypeKind kind = get_type_kind(type);
	assert(is_specifier(type));
	Type* p = get_type(type);
	return p->subtype;
}

static bool is_int(TypeID type) {
	return is_signed(type) || is_unsigned(type);
}

static bool is_unsigned(TypeID type) {
	return type >= TYPE_UINT8 && type <= TYPE_UINT64;
}

static bool is_signed(TypeID type) {
	return type >= TYPE_INT8 && type <= TYPE_INT64;
}

static bool is_float(TypeID type) {
	return type >= TYPE_FLOAT32 && type <= TYPE_FLOAT64;
}

static bool is_ptr(TypeID type) {
	return get_type_kind(type) == TYPE_KIND_PTR;
}

static bool is_specifier(TypeID type) {
	switch (get_type_kind(type)) {
		default:
			// print("is_specifier(%)\n", arg_type(type));
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

