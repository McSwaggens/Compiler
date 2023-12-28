#include "general.c"
#include "alloc.c"
#include "global_string_table.c"
#include "print.c"
#include "lexer.c"
#include "parser.c"
#include "semantic.c"
#include "ir.c"
#include "mm.c"
#include "type.c"
#include "file.c"

typedef u16 ModuleIndex16;

// Module store allows you to reference a specific module
//     with a small 16bit id instead of a 64bit pointer.
struct ModuleStore {
	Module** modules;
	ModuleIndex16 count;
	ModuleIndex16 capacity;
} static module_store;

static Module* get_module(ModuleIndex16 module_index) {
	return module_store.modules[module_index];
}

static Module* make_module(String file) {
	if (module_store.count == module_store.capacity) {
		module_store.capacity <<= 2;
		module_store.modules = realloc(
			module_store.modules,
			sizeof(Module*) * module_store.count,
			sizeof(Module*) * module_store.capacity
		);
	}

	Module* module = stack_alloc(sizeof(Module));
	*module = (Module){
		.file = file,
	};

	module_store.modules[module_store.count++] = module;

	return module;
}

// @WARNING: VERY SLOW FUNCTION.
// @WARNING: ONLY USE IN COLD PATH (Error)
static Module* find_module(Token* token) {
	for (u32 i = 0; i < module_store.count; i++) {
		Module* module = module_store.modules[i];
		if (token >= module->tokens && token < module->tokens_end)
			return module;
	}

	assert_unreachable();
	return null;
}

static TokenAux* get_aux(Module* module, Token* token) {
	u32 index = token - module->tokens;
	return &module->auxs[index];
}

static Position get_pos(Module* module, Token* token) {
	return get_aux(module, token)->pos;
}

static void init_module_store(void) {
	module_store.capacity = 64;
	module_store.count    = 0;
	module_store.modules  = alloc(sizeof(Module*)*module_store.capacity);
}

int main(int argc, char* argv[]) {
	init_global_allocator();
	init_type_system();
	init_module_store();
	init_ir();
	init_mm();

	list_dir(".");

	// @Todo: Load all files in directory

	Module* module = make_module(tostr("test.q"));
	lex(module);
	parse_module(module);

	print("Compiler finished.\n");
	flush_output_buffer(&standard_output_buffer);
	return 0;
}

