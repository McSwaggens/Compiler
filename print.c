#include "print.h"
#include "ast.h"
#include "lexer.h"

static void write_char(OutputBuffer* buffer, char c) {
	write_output_buffer_b(buffer, c);
}

static void write_u64(OutputBuffer* buffer, u64 n) {
	char tmp[20];
	s64 i = 0;
	do tmp[19-(i++)] = '0' + (n % 10); while (n /= 10);
	write_output_buffer(buffer, tmp+20-i, i);
}

static void write_s64(OutputBuffer* buffer, s64 n) {
	if (n < 0) {
		write_char(buffer, '-');
		write_u64(buffer, (u64)-n);
		return;
	}

	write_u64(buffer, (u64)n);
}

static f64 get_lowest_digit_f64(f64 f, f64 base) {
	f64 result = f / base;
	result -= floor_f64(result);
	result *= base;
	return result;
}

static void write_f64(OutputBuffer* buffer, f64 f) {
	if (is_nan_f64(f)) {
		write_output_buffer(buffer, "NaN", 3);
		return;
	}

	if (f < 0.0f) {
		write_char(buffer, '-');
		f = -f;
	}

	if (f == FLOAT64_INFINITY) {
		write_output_buffer(buffer, "INFINITY", 8);
		return;
	}

	char tmp[64];

	f64 fu = floor_f64(f);
	f64 fl = f-fu;

	s64 i = 0;
	do {
		// @Bug: 1536 gets printed as 1535
		f64 digit = get_lowest_digit_f64(fu, 10.0);
		fu /= 10;
		tmp[64-1-(i++)] = '0' + (u8)digit;
	} while (floor_f64(fu) && i < 64);

	write_output_buffer(buffer, tmp+64-i, i);

	if (!fl)
		return;

	write_char(buffer, '.');

	i = 0;

	do {
		fl *= 10;
		f64 digit = floor_f64(fl);
		tmp[i++] = '0' + (u8)digit;
		fl -= digit;
	} while (i < 16 && fl >= 0.01);

	write_output_buffer(buffer, tmp, i);
}

static void write_f32(OutputBuffer* buffer, f32 f) {
	write_f64(buffer, (f64)f);
}

static void write_string(OutputBuffer* buffer, String string) {
	if (string.data)
		write_output_buffer(buffer, string.data, string.length);
}

static void write_cstring(OutputBuffer* buffer, char* s) {
	write_string(buffer, tostr(s));
}

static void write_bool(OutputBuffer* buffer, bool b) {
	write_string(buffer, tostr(b ? "true" : "false"));
}

static void write_token_kind(OutputBuffer* buffer, TokenKind kind) {
	char* str;
	switch (kind) {
		case TOKEN_EOF:                 str = "TOKEN_EOF"; break;

		case TOKEN_IDENTIFIER_VARIABLE: str = "TOKEN_IDENTIFIER_VARIABLE"; break;
		case TOKEN_IDENTIFIER_FORMAL:   str = "TOKEN_IDENTIFIER_FORMAL";   break;
		case TOKEN_IDENTIFIER_CONSTANT: str = "TOKEN_IDENTIFIER_CONSTANT"; break;

		case TOKEN_LITERAL_INT8:        str = "TOKEN_LITERAL_INT8";    break;
		case TOKEN_LITERAL_INT16:       str = "TOKEN_LITERAL_INT16";   break;
		case TOKEN_LITERAL_INT32:       str = "TOKEN_LITERAL_INT32";   break;
		case TOKEN_LITERAL_INT64:       str = "TOKEN_LITERAL_INT64";   break;
		case TOKEN_LITERAL_UINT8:       str = "TOKEN_LITERAL_UINT8";   break;
		case TOKEN_LITERAL_UINT16:      str = "TOKEN_LITERAL_UINT16";  break;
		case TOKEN_LITERAL_UINT32:      str = "TOKEN_LITERAL_UINT32";  break;
		case TOKEN_LITERAL_UINT64:      str = "TOKEN_LITERAL_UINT64";  break;
		case TOKEN_LITERAL_FLOAT32:     str = "TOKEN_LITERAL_FLOAT32"; break;
		case TOKEN_LITERAL_FLOAT64:     str = "TOKEN_LITERAL_FLOAT64"; break;
		case TOKEN_LITERAL_STRING:      str = "TOKEN_LITERAL_STRING";  break;

		case TOKEN_IF:                  str = "if";       break;
		case TOKEN_ELSE:                str = "else";     break;
		case TOKEN_THEN:                str = "then";     break;
		case TOKEN_FOR:                 str = "for";      break;
		case TOKEN_WHILE:               str = "while";    break;
		case TOKEN_MATCH:               str = "match";    break;
		case TOKEN_BREAK:               str = "break";    break;
		case TOKEN_RETURN:              str = "return";   break;
		case TOKEN_CONTINUE:            str = "continue"; break;
		case TOKEN_TRUE:                str = "true";     break;
		case TOKEN_FALSE:               str = "false";    break;
		case TOKEN_NULL:                str = "null";     break;
		case TOKEN_INC:                 str = "inc";      break;
		case TOKEN_DEC:                 str = "dec";      break;

		case TOKEN_BYTE:                str = "byte";     break;
		case TOKEN_BOOL:                str = "bool";     break;
		case TOKEN_INT:                 str = "int";      break;
		case TOKEN_INT8:                str = "int8";     break;
		case TOKEN_INT16:               str = "int16";    break;
		case TOKEN_INT32:               str = "int32";    break;
		case TOKEN_INT64:               str = "int64";    break;
		case TOKEN_UINT:                str = "uint";     break;
		case TOKEN_UINT8:               str = "uint8";    break;
		case TOKEN_UINT16:              str = "uint16";   break;
		case TOKEN_UINT32:              str = "uint32";   break;
		case TOKEN_UINT64:              str = "uint64";   break;
		case TOKEN_FLOAT32:             str = "float32";  break;
		case TOKEN_FLOAT64:             str = "float64";  break;
		case TOKEN_TYPE_ID:             str = "type_id";  break;

		case TOKEN_STRUCT:              str = "struct";   break;
		case TOKEN_ENUM:                str = "enum";     break;

		case TOKEN_OPEN_BRACE:          str = "{";  break;
		case TOKEN_CLOSE_BRACE:         str = "}";  break;
		case TOKEN_OPEN_BRACKET:        str = "[";  break;
		case TOKEN_CLOSE_BRACKET:       str = "]";  break;
		case TOKEN_OPEN_PAREN:          str = "(";  break;
		case TOKEN_CLOSE_PAREN:         str = ")";  break;

		case TOKEN_EQUAL:               str = "=";  break;
		case TOKEN_NOT_EQUAL:           str = "!="; break;
		case TOKEN_LESS:                str = "<";  break;
		case TOKEN_GREATER:             str = ">";  break;
		case TOKEN_LESS_OR_EQUAL:       str = "<="; break;
		case TOKEN_GREATER_OR_EQUAL:    str = ">="; break;

		case TOKEN_BACK_SLASH:          str = "\\"; break;
		case TOKEN_PIKE:                str = "|";  break;
		case TOKEN_COLON:               str = ":";  break;
		case TOKEN_SEMICOLON:           str = ";";  break;
		case TOKEN_DOT:                 str = ".";  break;
		case TOKEN_DOT_DOT:             str = ".."; break;
		case TOKEN_ARROW:               str = "->"; break;
		case TOKEN_COMMA:               str = ",";  break;
		case TOKEN_PLUS:                str = "+";  break;
		case TOKEN_MINUS:               str = "-";  break;

		case TOKEN_AND:                 str = "&&"; break;
		case TOKEN_OR:                  str = "||"; break;

		case TOKEN_BACKTICK:            str = "`";  break;
		case TOKEN_TILDA:               str = "~";  break;
		case TOKEN_ASTERISK:            str = "*";  break;
		case TOKEN_SLASH:               str = "/";  break;
		case TOKEN_EXCLAMATION:         str = "!";  break;
		case TOKEN_AT:                  str = "@";  break;
		case TOKEN_HASH:                str = "#";  break;
		case TOKEN_DOLLAR:              str = "$";  break;
		case TOKEN_PERCENT:             str = "%";  break;
		case TOKEN_CARET:               str = "^";  break;
		case TOKEN_AMPERSAND:           str = "&";  break;
		case TOKEN_QUESTION:            str = "?";  break;

		case TOKEN_LEFT_SHIFT:          str = "<<";  break;
		case TOKEN_RIGHT_SHIFT:         str = ">>";  break;
		case TOKEN_LEFT_SHIFT_EQUAL:    str = "<<="; break;
		case TOKEN_RIGHT_SHIFT_EQUAL:   str = ">>="; break;

		case TOKEN_PLUS_EQUAL:          str = "+="; break;
		case TOKEN_MINUS_EQUAL:         str = "-="; break;
		case TOKEN_ASTERISK_EQUAL:      str = "*="; break;
		case TOKEN_SLASH_EQUAL:         str = "/="; break;
		case TOKEN_PERCENT_EQUAL:       str = "%="; break;
		case TOKEN_AMPERSAND_EQUAL:     str = "&="; break;
		case TOKEN_CARET_EQUAL:         str = "^="; break;
		case TOKEN_TILDA_EQUAL:         str = "~="; break;
		case TOKEN_PIKE_EQUAL:          str = "|="; break;
	}

	write_string(buffer, tostr(str));
}

static void write_token(OutputBuffer* buffer, Token* token) {
	switch (token->kind) {
		case TOKEN_IDENTIFIER_VARIABLE:
		case TOKEN_IDENTIFIER_FORMAL:
		case TOKEN_IDENTIFIER_CONSTANT:
			write_string(buffer, token->identifier);
			return;

		case TOKEN_LITERAL_INT8:
		case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:
		case TOKEN_LITERAL_INT64:
			write_s64(buffer, token->i);
			return;

		case TOKEN_LITERAL_UINT8:
		case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32:
		case TOKEN_LITERAL_UINT64:
			write_u64(buffer, token->i);
			return;

		case TOKEN_LITERAL_FLOAT32:
			write_f32(buffer, token->f);
			return;

		case TOKEN_LITERAL_FLOAT64:
			write_f64(buffer, token->d);
			return;

		case TOKEN_LITERAL_STRING:
			write_char(buffer, '"');
			write_string(buffer, token->string);
			write_char(buffer, '"');
			return;

		default:
			write_token_kind(buffer, token->kind);
			return;
	}
}

static void write_type(OutputBuffer* buffer, TypeID type) {
	if (!type) {
		write_cstring(buffer, "TYPE_NULL");
		return;
	}

	Type* info = ts_get_info(type);

	switch (ts_get_kind(type)) {
		case TYPE_KIND_PRIMITIVE: {
			switch ((PrimitiveType)type) {
				case TYPE_BYTE:    write_cstring(buffer, "byte");    break;
				case TYPE_BOOL:    write_cstring(buffer, "bool");    break;
				case TYPE_TYPEID:  write_cstring(buffer, "typeid");  break;

				case TYPE_INT8:    write_cstring(buffer, "int8");    break;
				case TYPE_INT16:   write_cstring(buffer, "int16");   break;
				case TYPE_INT32:   write_cstring(buffer, "int32");   break;
				case TYPE_INT64:   write_cstring(buffer, "int64");   break;

				case TYPE_UINT8:   write_cstring(buffer, "uint8");   break;
				case TYPE_UINT16:  write_cstring(buffer, "uint16");  break;
				case TYPE_UINT32:  write_cstring(buffer, "uint32");  break;
				case TYPE_UINT64:  write_cstring(buffer, "uint64");  break;

				case TYPE_FLOAT32: write_cstring(buffer, "float32"); break;
				case TYPE_FLOAT64: write_cstring(buffer, "float64"); break;
			}
		} return;

		case TYPE_KIND_PTR: {
			write_char(buffer, '*');
			write_type(buffer, info->subtype);
		} break;

		case TYPE_KIND_ARRAY: {
			write_cstring(buffer, "[]");
			write_type(buffer, info->subtype);
		} break;

		case TYPE_KIND_FIXED: {
			write_char(buffer, '[');
			write_u64(buffer, info->length);
			write_char(buffer, ']');
			write_type(buffer, info->subtype);
		} break;

		case TYPE_KIND_STRUCT: {
			write_cstring(buffer, "struct ");
			write_token(buffer, info->ustruct->name);
			write_string(buffer, info->ustruct ? info->ustruct->name->string : tostr("null"));
		} break;

		case TYPE_KIND_ENUM: {
			write_cstring(buffer, "enum ");
			write_string(buffer, info->uenum ? info->uenum->name->string : tostr("null"));
		} break;

		case TYPE_KIND_TUPLE: {
			write_char(buffer, '(');
			for (u32 i = 0; i < info->length; i++) {
				if (i) write_cstring(buffer, ", ");
				write_type(buffer, info->elements[i]);
			}
			write_char(buffer, ')');
		} break;

		case TYPE_KIND_FUNCTION: {
			bool is_input_tuple = ts_get_kind(info->input) == TYPE_KIND_TUPLE;
			if (!is_input_tuple) write_char(buffer, '(');
			write_type(buffer, info->input);
			if (!is_input_tuple) write_char(buffer, ')');
			write_cstring(buffer, " -> ");
			write_type(buffer, info->output);
		} break;
	}
}

static void write_expression(OutputBuffer* buffer, Expression* expr) {
	if (!expr) {
		write_cstring(buffer, "null");
		return;
	}

	// print("(kind = %)", arg_s32(expr->kind));

	switch (expr->kind) {
		default: assert_unreachable();

		case EXPR_TRUE:
		case EXPR_FALSE:
		case EXPR_NULL:
		case EXPR_BASETYPE_PRIMITIVE:
		case EXPR_LITERAL: {
			write_token(buffer, expr->term.token);
		} break;

		case EXPR_BASETYPE_STRUCT: {
			write_string(buffer, expr->term.user_struct->name->identifier);
		} break;

		case EXPR_BASETYPE_ENUM: {
			write_string(buffer, expr->term.user_enum->name->identifier);
		} break;

		case EXPR_FUNCTION: {
			if (!expr->term.func)
				write_cstring(buffer, "null");
			else
				write_token(buffer, expr->term.func->name);
		} break;

		case EXPR_IDENTIFIER_CONSTANT:
		case EXPR_IDENTIFIER_FORMAL:
		case EXPR_IDENTIFIER_VARIABLE: {
			write_string(buffer, expr->term.token->identifier);
		} break;

		case EXPR_SPEC_FIXED: {
			write_char(buffer, '[');
			write_expression(buffer, expr->specifier.length);
			write_char(buffer, ']');
			write_expression(buffer, expr->specifier.sub);
		} break;

		case EXPR_SPEC_PTR: {
			write_char(buffer, '*');
			write_expression(buffer, expr->specifier.sub);
		} break;

		case EXPR_SPEC_ARRAY: {
			write_cstring(buffer, "[]");
			write_expression(buffer, expr->specifier.sub);
		} break;

		case EXPR_UNARY_IMPLICIT_CAST: {
			write_cstring(buffer, "(");
			write_expression(buffer, expr->left);
			write_cstring(buffer, " IMPLICIT_CAST ");
			// write_cstring(buffer, " TODO_TYPE_VALUE_PRINTING");
			write_type(buffer, expr->type);
			write_cstring(buffer, ")");
		} break;

		case EXPR_UNARY_PTR:
		case EXPR_UNARY_REF:
		case EXPR_UNARY_ABS:
		case EXPR_UNARY_INVERSE:
		case EXPR_UNARY_NOT:
		case EXPR_UNARY_BIT_NOT: {
			// write_char(buffer, '(');
			write_token(buffer, expr->optoken);
			write_expression(buffer, expr->left);
			// write_char(buffer, ')');
		} break;

		case EXPR_BINARY_ADD:
		case EXPR_BINARY_SUB:
		case EXPR_BINARY_MUL:
		case EXPR_BINARY_DIV:
		case EXPR_BINARY_MOD:
		case EXPR_BINARY_BIT_XOR:
		case EXPR_BINARY_BIT_AND:
		case EXPR_BINARY_BIT_OR:
		case EXPR_BINARY_OR:
		case EXPR_BINARY_AND:
		case EXPR_BINARY_EQUAL:
		case EXPR_BINARY_NOT_EQUAL:
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_LESS_OR_EQUAL:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_GREATER_OR_EQUAL:
		case EXPR_BINARY_LSHIFT:
		case EXPR_BINARY_RSHIFT:
		case EXPR_BINARY_DOT:
		case EXPR_BINARY_DOT_DOT: {
			write_char(buffer, '(');
			write_expression(buffer, expr->left);
			write_char(buffer, ' ');
			write_token(buffer, expr->optoken);
			write_char(buffer, ' ');
			write_expression(buffer, expr->right);
			write_char(buffer, ')');
		} break;

		case EXPR_INDEX: {
			write_expression(buffer, expr->subscript.base);
			write_char(buffer, '[');
			write_expression(buffer, expr->subscript.index);
			write_char(buffer, ']');
		} break;

		case EXPR_CALL: {
			write_expression(buffer, expr->call.function);
			write_char(buffer, '(');

			Expression** args = expr_table_get(&expr->call.args);
			for (u64 i = 0; i < expr->call.args.count; i++) {
				if (i) write_cstring(buffer, ", ");
				write_expression(buffer, args[i]);
			}

			write_char(buffer, ')');
		} break;

		case EXPR_BINARY_SPAN: {
			write_char(buffer, '[');
			write_expression(buffer, expr->left);
			write_cstring(buffer, " .. ");
			write_expression(buffer, expr->right);
			write_char(buffer, ']');
		} break;


		case EXPR_TERNARY_IF_ELSE: {
			write_char(buffer, '(');
			write_expression(buffer, expr->left);
			write_cstring(buffer, " if ");
			write_expression(buffer, expr->middle);
			write_cstring(buffer, " else ");
			write_expression(buffer, expr->right);
			write_char(buffer, ')');
		} break;

		case EXPR_ARRAY: {
			write_cstring(buffer, "{ARRAY: ");

			Expression** args = expr_table_get(&expr->array.elems);
			for (u64 i = 0; i < expr->array.elems.count; i++) {
				if (i) write_cstring(buffer, ", ");
				write_expression(buffer, args[i]);
			}

			if (expr->array.elems.count)
				write_char(buffer, ' ');

			write_cstring(buffer, "}");
		} break;

		case EXPR_TUPLE: {
			write_cstring(buffer, "(TUPLE: ");

			Expression** args = expr_table_get(&expr->array.elems);
			for (u64 i = 0; i < expr->tuple.elems.count; i++) {
				if (i) write_cstring(buffer, ", ");
				write_expression(buffer, args[i]);
			}

			write_cstring(buffer, ")");
		} break;
	}
}

static void write_statement(OutputBuffer* buffer, Statement* statement) {
	if (!statement)
		write_cstring(buffer, "null");

	const char* s = null;
	switch (statement->kind) {
		case STATEMENT_ASSIGNMENT:         s = "STATEMENT_ASSIGNMENT";         break;
		case STATEMENT_ASSIGNMENT_LSH:     s = "STATEMENT_ASSIGNMENT_LSH";     break;
		case STATEMENT_ASSIGNMENT_RSH:     s = "STATEMENT_ASSIGNMENT_RSH";     break;
		case STATEMENT_ASSIGNMENT_DIV:     s = "STATEMENT_ASSIGNMENT_DIV";     break;
		case STATEMENT_ASSIGNMENT_MOD:     s = "STATEMENT_ASSIGNMENT_MOD";     break;
		case STATEMENT_ASSIGNMENT_ADD:     s = "STATEMENT_ASSIGNMENT_ADD";     break;
		case STATEMENT_ASSIGNMENT_SUB:     s = "STATEMENT_ASSIGNMENT_SUB";     break;
		case STATEMENT_ASSIGNMENT_MUL:     s = "STATEMENT_ASSIGNMENT_MUL";     break;
		case STATEMENT_ASSIGNMENT_BIT_XOR: s = "STATEMENT_ASSIGNMENT_BIT_XOR"; break;
		case STATEMENT_ASSIGNMENT_BIT_AND: s = "STATEMENT_ASSIGNMENT_BIT_AND"; break;
		case STATEMENT_ASSIGNMENT_BIT_OR:  s = "STATEMENT_ASSIGNMENT_BIT_OR";  break;
		case STATEMENT_EXPRESSION:         s = "STATEMENT_EXPRESSION";         break;
		case STATEMENT_CONTROLFLOW:        s = "STATEMENT_CONTROLFLOW";        break;
		case STATEMENT_VARDECL:            s = "STATEMENT_VARDECL";            break;
		case STATEMENT_RETURN:             s = "STATEMENT_RETURN";             break;
		case STATEMENT_BREAK:              s = "STATEMENT_BREAK";              break;
		case STATEMENT_CONTINUE:           s = "STATEMENT_CONTINUE";           break;
		case STATEMENT_INC:                s = "STATEMENT_INC";                break;
		case STATEMENT_DEC:                s = "STATEMENT_DEC";                break;
	}
	print(s);

	switch (statement->kind) {
		default: assert_unreachable();

		case STATEMENT_ASSIGNMENT_LSH:
		case STATEMENT_ASSIGNMENT_RSH:
		case STATEMENT_ASSIGNMENT_DIV:
		case STATEMENT_ASSIGNMENT_MOD:
		case STATEMENT_ASSIGNMENT_ADD:
		case STATEMENT_ASSIGNMENT_SUB:
		case STATEMENT_ASSIGNMENT_MUL:
		case STATEMENT_ASSIGNMENT_BIT_XOR:
		case STATEMENT_ASSIGNMENT_BIT_AND:
		case STATEMENT_ASSIGNMENT_BIT_OR:
		case STATEMENT_ASSIGNMENT: {
			printbuff(
				buffer,
				":\n\tleft  = %\n\tright = %",
				arg_expression(statement->assign.left),
				arg_expression(statement->assign.right)
			);
		} break;

		case STATEMENT_EXPRESSION: {
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
		} break;

		case STATEMENT_CONTROLFLOW: {
			write_cstring(buffer, "");
		} break;

		case STATEMENT_VARDECL: {
			printbuff(
				buffer,
				":\n\tname = %\n\ttype = %\n\tinit = %",
				arg_token(statement->var->name),
				arg_expression(statement->var->type_expr),
				arg_expression(statement->var->init_expr)
			);
		} break;

		case STATEMENT_RETURN: {
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
		} break;

		case STATEMENT_CONTINUE: {
		} break;

		case STATEMENT_BREAK: {
		} break;

		case STATEMENT_INC: {
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
		} break;

		case STATEMENT_DEC: {
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
		} break;
	}
}

static void print_argument(OutputBuffer* buffer, FormatArg arg) {
	switch (arg.kind) {
		case PRINT_ARG_BOOL:       write_bool(buffer, arg.b); return;

		case PRINT_ARG_INT8:       write_s64(buffer, arg.s8);  return;
		case PRINT_ARG_INT16:      write_s64(buffer, arg.s16); return;
		case PRINT_ARG_INT32:      write_s64(buffer, arg.s32); return;
		case PRINT_ARG_INT64:      write_s64(buffer, arg.s64); return;

		case PRINT_ARG_UINT8:      write_u64(buffer, arg.u8);  return;
		case PRINT_ARG_UINT16:     write_u64(buffer, arg.u16); return;
		case PRINT_ARG_UINT32:     write_u64(buffer, arg.u32); return;
		case PRINT_ARG_UINT64:     write_u64(buffer, arg.u64); return;

		case PRINT_ARG_F32:        write_f32(buffer, arg.f); return;
		case PRINT_ARG_F64:        write_f64(buffer, arg.d); return;
		case PRINT_ARG_CHAR:       write_char(buffer, arg.c);  return;
		case PRINT_ARG_STRING:     write_string(buffer, arg.str); return;
		case PRINT_ARG_CSTRING:    write_cstring(buffer, arg.s);  return;

		case PRINT_ARG_TOKEN:      write_token(buffer, arg.token); return;
		case PRINT_ARG_TOKEN_KIND: write_token_kind(buffer, arg.token_kind); return;

		case PRINT_ARG_EXPRESSION: write_expression(buffer, arg.expr); return;
		case PRINT_ARG_STATEMENT:  write_statement(buffer, arg.statement); return;
		case PRINT_ARG_TYPE:       write_type(buffer, arg.type); return;
	}
}

static void internal_print(OutputBuffer* buffer, const char* format, __builtin_va_list args) {
	// const char* format_begin = format;
	const char* left_cursor = format;
	const char* cursor = format;

	while (*cursor) {
		if (cursor[0] == '\\') {
			switch (cursor[1]) {
				case '\\':
				case '%': {
					if (cursor-left_cursor)
						write_output_buffer(buffer, left_cursor, cursor-left_cursor);
					left_cursor = cursor+1;
					cursor += 2;
				}
			}
		}
		else if (cursor[0] == '%') {
			if (cursor-left_cursor)
				write_output_buffer(buffer, left_cursor, cursor-left_cursor);

			FormatArg arg = __builtin_va_arg(args, FormatArg);
			print_argument(buffer, arg);

			cursor += 1;
			left_cursor = cursor;
		}
		else {
			cursor++;
		}
	}

	if (cursor-left_cursor)
		write_output_buffer(buffer, left_cursor, cursor-left_cursor);

	flush_output_buffer(buffer);
}

static void print(const char* format, ...) {
	__builtin_va_list args;
	__builtin_va_start(args, format);
	internal_print(&standard_output_buffer, format, args);
	__builtin_va_end(args);
}

static void printbuff(OutputBuffer* buffer, const char* format, ...) {
	__builtin_va_list args;
	__builtin_va_start(args, format);
	internal_print(buffer, format, args);
	__builtin_va_end(args);
}

static void error(String file, Position pos, const char* format, ...) {
	print("%:%:%: error: ",
		arg_string(file),
		arg_u64(pos.line+1),
		arg_u64(pos.column+1)
	);

	__builtin_va_list args;
	__builtin_va_start(args, format);
	internal_print(&standard_output_buffer, format, args);
	__builtin_va_end(args);
	flush_output_buffer(&standard_output_buffer);
	exit_program();
}

static void errort(Token* token, const char* format, ...) {
	Module* module = find_module(token);
	Position pos = get_pos(module, token);

	print("%:%:%: error: ",
		arg_string(module->file),
		arg_u64(pos.line+1),
		arg_u64(pos.column+1)
	);

	__builtin_va_list args;
	__builtin_va_start(args, format);
	internal_print(&standard_output_buffer, format, args);
	__builtin_va_end(args);
	flush_output_buffer(&standard_output_buffer);
	exit_program();
}

static void write_fill_char(OutputBuffer* buffer, char c, u64 n) {
	while (n--)
		write_char(buffer, c);
}

static void errore(Expression* expr, u64 extra_lines, const char* format, ...) {
	OutputBuffer* buffer = &standard_output_buffer;

	Module* module = find_module(expr->begin);

	TokenAux* aux_begin = get_aux(module, expr->begin);
	TokenAux* aux_end   = get_aux(module, expr->end);

	u64 line_begin = aux_begin->pos.line;
	u64 line_end   = aux_end[-1].pos.line+1;

	assert(line_begin < line_end);

	u64 expr_lines = line_end-line_begin;
	u64 lines_till_eof = module->line_count - line_begin;
	u64 lines = min_u64(lines_till_eof, expr_lines + extra_lines);

	line_end = line_begin + lines;

	Position pos = aux_begin->pos;

	print("%:%:%: error: ",
		arg_string(module->file),
		arg_u64(pos.line+1),
		arg_u64(pos.column+1)
	);

	__builtin_va_list args;
	__builtin_va_start(args, format);
	internal_print(buffer, format, args);
	__builtin_va_end(args);

	// Filter out blank lines
	for (u64 i = line_end-1; i > line_begin+1; i--) {
		Line* line = module->lines+i;
		if (line->token_begin != line->token_end) {
			break;
		}

		line_end--;
	}

	for (u64 i = line_begin; i < line_end; i++) {
		Line* line = &module->lines[i];
		write_string(buffer, line->string);

		if (i == line_begin && expr_lines == 1) {

			write_fill_char(buffer, '\t', expr->begin->indent);
			write_fill_char(buffer, ' ', aux_begin->pos.column-expr->begin->indent);
			write_char(buffer, '^');

			u64 width = (aux_end[-1].pos.column - aux_begin->pos.column) + aux_end[-1].width;
			write_fill_char(buffer, '~', width-1);
			write_char(buffer, '\n');
		}
	}

	flush_output_buffer(&standard_output_buffer);
	exit_program();
}

