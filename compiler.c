#include "general.c"
#include "alloc.c"
#include "global_string_table.c"
#include "print.c"
#include "lexer.c"
#include "parser.c"
#include "semantic.c"
#include "ir.c"
#include "type.c"
#include "file.c"

int main(int argc, char* argv[]) {
	init_global_allocator();
	init_type_system();
	init_ir();
	TokenStore tokens = generate_tokens(get_string("test.q"));
	flush_output_buffer(&standard_output_buffer);
	// @Todo: Load all files in directory
	Module* module = parse_module(tokens);
	scan_module(module);
	preconvert_module(module);
	convert_module(module);
	print("Compiler finished.\n");
	return 0;
}

