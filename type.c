#include "type.h"

Type* types;
Type* types_end;
u64 type_count;

TypeKind get_type_kind(TypeID id)  { return id & 0xFllu; }
u64      get_type_index(TypeID id) { return id >> 4llu;  }

// @Warning: Pointer may be invalidated by new_type function. Use carefully!
Type* get_type_ptr(TypeID id) { return types+get_type_index(id);  }
Type  get_type(TypeID id)     { return types[get_type_index(id)]; }

void update_type(TypeID id, Type type) { types[get_type_index(id)] = type; }
u64  get_type_size(TypeID id) { return get_type(id).size; }

u64 get_tuple_type_length(TypeID type) {
	assert(type);
	assert(get_type_kind(type) == TYPE_KIND_TUPLE);
	return get_type(type).length;
}

u64 get_fixed_type_length(TypeID type) {
	assert(type);
	assert(get_type_kind(type) == TYPE_KIND_FIXED);
	return get_type(type).length;
}

void init_type_system(void) {
	types      = alloc(1<<24);
	types_end  = (Type*)(((char*)types) + (1<<24));
	type_count = LARGEST_CORE_TYPE_INDEX;

	types[TYPE_BYTE]    = (Type){ .size = 1 };
	types[TYPE_BOOL]    = (Type){ .size = 1 };
	types[TYPE_TYPEID]  = (Type){ .size = 8 };
	types[TYPE_INT8]    = (Type){ .size = 1 };
	types[TYPE_INT16]   = (Type){ .size = 2 };
	types[TYPE_INT32]   = (Type){ .size = 4 };
	types[TYPE_INT64]   = (Type){ .size = 8 };
	types[TYPE_UINT8]   = (Type){ .size = 1 };
	types[TYPE_UINT16]  = (Type){ .size = 2 };
	types[TYPE_UINT32]  = (Type){ .size = 4 };
	types[TYPE_UINT64]  = (Type){ .size = 8 };
	types[TYPE_FLOAT32] = (Type){ .size = 4 };
	types[TYPE_FLOAT64] = (Type){ .size = 8 };

	types[get_type_index(TYPE_EMPTY_TUPLE)] = (Type){ .size = 0, .length = 0 };
}

TypeID new_type(TypeKind kind) {
	u64 index = type_count;
	TypeID id = MAKE_TYPEID(kind, index);

	if (types+index+1 <= types_end)
		return id;

	u64 current_alloc_size = (char*)types_end-(char*)types;
	u64 new_alloc_size = current_alloc_size*4;
	types = realloc(types, current_alloc_size, new_alloc_size);

	return id;
}

void xtable_push(TypeID typeid, TypeExtent ext) {
	XTable* table = &get_type_ptr(typeid)->xtable;
	if (is_pow2(table->length+1)) {
		table->exts = realloc(
			table->exts,
			sizeof(TypeExtent) * (table->length+1),
			sizeof(TypeExtent) * ((table->length+1)*2)
		);
	}

	table->exts[table->length] = ext;
	table->length++;
}

TypeID get_ref_type(TypeID subtype) {
	assert(subtype);

	if (get_type(subtype).ptr)
		return get_type(subtype).ref;

	TypeID ntid = new_type(TYPE_KIND_REF);
	update_type(ntid, (Type){
		.subtype = subtype,
		.size = 8,
	});

	Type* sub = get_type_ptr(subtype);
	sub->ptr = ntid;

	return ntid;
}

TypeID get_pointer_type(TypeID subtype) {
	assert(subtype);

	if (get_type(subtype).ptr)
		return get_type(subtype).ptr;

	TypeID ntid = new_type(TYPE_KIND_PTR);
	update_type(ntid, (Type){
		.subtype = subtype,
		.size = 8,
	});

	Type* sub = get_type_ptr(subtype);
	sub->ptr = ntid;

	return ntid;
}

TypeID get_array_type(TypeID subtype) {
	assert(subtype);

	if (get_type(subtype).array)
		return get_type(subtype).array;

	TypeID ntid = new_type(TYPE_KIND_ARRAY);
	update_type(ntid, (Type){
		.subtype = subtype,
		.size = 16,
	});

	Type* sub = get_type_ptr(subtype);
	sub->array = ntid;

	return ntid;
}

// If get_function_type or get_tuple_type lookups are too slow we should start using binary search

TypeID get_function_type(TypeID input, TypeID output) {
	assert(input);
	assert(output);

	XTable table = get_type(input).xtable;
	for (u64 i = 0; i < table.length; i++) {
		TypeExtent* ext = &table.exts[i];

		if (ext->output != output)
			continue;

		if (get_type_kind(ext->type) != TYPE_KIND_FUNCTION)
			continue;

		return ext->type;
	}

	TypeID ntid = new_type(TYPE_KIND_FUNCTION);
	update_type(ntid, (Type){
		.input  = input,
		.output = output,
		.size   = 0,
	});

	xtable_push(ntid, (TypeExtent){
		.output = output,
		.type   = ntid
	});

	return ntid;
}

TypeID get_tuple_type(TypeID* types, u64 count) {
	if (!count)
		return TYPE_EMPTY_TUPLE;

	if (count == 1)
		return types[0];

	XTable table = get_type(types[0]).xtable;

	for (u64 i = 0; i < table.length; i++) {
		TypeExtent* ext = &table.exts[i];

		if (ext->length != count)
			continue;

		if (ext->output != types[1])
			continue;

		if (get_type_kind(ext->type) != TYPE_KIND_TUPLE)
			continue;

		if (!compare(types+1, get_type(ext->type).elements+1, (count-1)*sizeof(TypeID)))
			continue;

		return ext->type;
	}

	TypeID* elems = alloc(count*sizeof(TypeID));
	u64 size = 0;
	for (u64 i = 0; i < count; i++) {
		elems[i] = types[i];
		size += get_type_size(types[i]);
	}

	TypeID ntid = new_type(TYPE_KIND_TUPLE);
	update_type(ntid, (Type){
		.size = size,
		.length = count,
		.elements = elems,
	});

	xtable_push(types[0], (TypeExtent){
		.length = count,
		.type   = ntid,
		.output = types[1],
	});

	return ntid;
}

TypeID get_fixed_type(TypeID subtype, u64 length) {
	assert(subtype);
	assert(length);

	XTable table = get_type(subtype).xtable;
	for (u64 i = 0; i < table.length; i++) {
		TypeExtent* ext = table.exts+i;

		if (ext->length != length)
			continue;

		if (get_type_kind(ext->type) != TYPE_KIND_FIXED)
			continue;

		return ext->type;
	}

	u64 size = length * get_type_size(subtype);

	TypeID ntid = new_type(TYPE_KIND_FIXED);
	update_type(ntid, (Type){
		.size     = size,
		.length   = length,
		.subtype  = subtype,
	});

	xtable_push(subtype, (TypeExtent){
		.length = length,
		.output = subtype,
		.type   = ntid,
	});

	return ntid;
}

bool is_int(TypeID type) {
	switch (type) {
		case TYPE_INT8:
		case TYPE_INT16:
		case TYPE_INT32:
		case TYPE_INT64:
		case TYPE_UINT8:
		case TYPE_UINT16:
		case TYPE_UINT32:
		case TYPE_UINT64:
			return true;
		default:
			return false;
	}
}

bool is_unsigned(TypeID type) {
	switch (type) {
		case TYPE_UINT8:
		case TYPE_UINT16:
		case TYPE_UINT32:
		case TYPE_UINT64:
			return true;
		default:
			return false;
	}
}

bool is_signed(TypeID type) {
	switch (type) {
		case TYPE_INT8:
		case TYPE_INT16:
		case TYPE_INT32:
		case TYPE_INT64:
			return true;
		default:
			return false;
	}
}

bool is_float(TypeID type) {
	switch (type) {
		case TYPE_FLOAT32:
		case TYPE_FLOAT64:
			return true;
		default:
			return false;
	}
}

bool is_ref(TypeID type) {
	return get_type_kind(type) == TYPE_KIND_REF;
}

