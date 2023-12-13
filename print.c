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
		case TOKEN_FLOAT32:             str = "f32";  break;
		case TOKEN_FLOAT64:             str = "f64";  break;
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

	Type* info = get_type(type);

	switch (get_type_kind(type)) {
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

				case TYPE_FLOAT32: write_cstring(buffer, "f32"); break;
				case TYPE_FLOAT64: write_cstring(buffer, "f64"); break;
			}
		} return;

		case TYPE_KIND_PTR: {
			write_char(buffer, '*');
			write_type(buffer, info->subtype);
		} break;

		case TYPE_KIND_REF: {
			write_cstring(buffer, "*");
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
			bool is_input_tuple = get_type_kind(info->input) == TYPE_KIND_TUPLE;
			if (!is_input_tuple) write_char(buffer, '(');
			write_type(buffer, info->input);
			if (!is_input_tuple) write_char(buffer, ')');
			write_cstring(buffer, " -> ");
			write_type(buffer, info->input);
		} break;
	}
}

static void write_expression(OutputBuffer* buffer, Expression* expr) {
	if (!expr) {
		write_cstring(buffer, "null");
		return;
	}

	switch (expr->kind) {
		default: assert_unreachable();

		case AST_EXPR_TRUE:
		case AST_EXPR_FALSE:
		case AST_EXPR_NULL:
		case AST_EXPR_BASETYPE_PRIMITIVE:
		case AST_EXPR_BASETYPE_IDENTIFIER:
		case AST_EXPR_LITERAL: {
			write_token(buffer, expr->term.token);
		} break;

		case AST_EXPR_FUNCTION: {
			if (!expr->term.func)
				write_cstring(buffer, "null");
			else
				write_token(buffer, expr->term.func->name);
		} break;

		case AST_EXPR_IDENTIFIER_CONSTANT:
		case AST_EXPR_IDENTIFIER_FORMAL:
		case AST_EXPR_IDENTIFIER_VARIABLE: {
			write_string(buffer, expr->term.token->identifier);
		} break;

		case AST_EXPR_SPEC_FIXED: {
			write_char(buffer, '[');
			write_expression(buffer, expr->specifier.length);
			write_char(buffer, ']');
			write_expression(buffer, expr->specifier.sub);
		} break;

		case AST_EXPR_SPEC_PTR: {
			write_char(buffer, '*');
			write_expression(buffer, expr->specifier.sub);
		} break;

		case AST_EXPR_SPEC_ARRAY: {
			write_cstring(buffer, "[]");
			write_expression(buffer, expr->specifier.sub);
		} break;

		case AST_EXPR_UNARY_IMPLICIT_CAST:
			write_cstring(buffer, "(");
			write_expression(buffer, expr->unary.sub);
			write_cstring(buffer, " IMPLICIT_CAST ");
			write_type(buffer, expr->type);
			write_cstring(buffer, ")");

		case AST_EXPR_UNARY_PTR:
		case AST_EXPR_UNARY_REF:
		case AST_EXPR_UNARY_ABS:
		case AST_EXPR_UNARY_INVERSE:
		case AST_EXPR_UNARY_NOT:
		case AST_EXPR_UNARY_BIT_NOT: {
			write_char(buffer, '(');
			write_token(buffer, expr->unary.optoken);
			write_expression(buffer, expr->unary.sub);
			write_char(buffer, ')');
		} break;

		case AST_EXPR_BINARY_ADD:
		case AST_EXPR_BINARY_SUB:
		case AST_EXPR_BINARY_MUL:
		case AST_EXPR_BINARY_DIV:
		case AST_EXPR_BINARY_MOD:
		case AST_EXPR_BINARY_BIT_XOR:
		case AST_EXPR_BINARY_BIT_AND:
		case AST_EXPR_BINARY_BIT_OR:
		case AST_EXPR_BINARY_OR:
		case AST_EXPR_BINARY_AND:
		case AST_EXPR_BINARY_EQUAL:
		case AST_EXPR_BINARY_NOT_EQUAL:
		case AST_EXPR_BINARY_LESS:
		case AST_EXPR_BINARY_LESS_OR_EQUAL:
		case AST_EXPR_BINARY_GREATER:
		case AST_EXPR_BINARY_GREATER_OR_EQUAL:
		case AST_EXPR_BINARY_LSHIFT:
		case AST_EXPR_BINARY_RSHIFT:
		case AST_EXPR_BINARY_DOT:
		case AST_EXPR_BINARY_DOT_DOT: {
			write_char(buffer, '(');
			write_expression(buffer, expr->binary.left);
			write_char(buffer, ' ');
			write_token(buffer, expr->binary.optoken);
			write_char(buffer, ' ');
			write_expression(buffer, expr->binary.right);
			write_char(buffer, ')');
		} break;

		case AST_EXPR_INDEX: {
			write_expression(buffer, expr->subscript.base);
			write_char(buffer, '[');
			write_expression(buffer, expr->subscript.index);
			write_char(buffer, ']');
		} break;

		case AST_EXPR_CALL: {
			write_expression(buffer, expr->call.function);
			write_char(buffer, '(');
			for (u64 i = 0; i < expr->call.arg_count; i++) {
				if (i) write_cstring(buffer, ", ");
				Expression* arg = expr->call.args[i];
				write_expression(buffer, arg);
			}
			write_char(buffer, ')');
		} break;

		case AST_EXPR_BINARY_SPAN: {
			write_char(buffer, '[');
			write_expression(buffer, expr->span.left);
			write_cstring(buffer, " .. ");
			write_expression(buffer, expr->span.right);
			write_char(buffer, ']');
		} break;


		case AST_EXPR_TERNARY_IF_ELSE: {
			write_char(buffer, '(');
			write_expression(buffer, expr->ternary.left);
			write_cstring(buffer, " if ");
			write_expression(buffer, expr->ternary.middle);
			write_cstring(buffer, " else ");
			write_expression(buffer, expr->ternary.right);
			write_char(buffer, ')');
		} break;

		case AST_EXPR_ARRAY: {
			write_cstring(buffer, "{ARRAY: ");

			for (u64 i = 0; i < expr->array.elem_count; i++) {
				if (i) write_cstring(buffer, ", ");
				Expression* elem = expr->array.elems[i];
				write_expression(buffer, elem);
			}

			if (expr->array.elem_count)
				write_char(buffer, ' ');

			write_cstring(buffer, "}");
		} break;

		case AST_EXPR_TUPLE: {
			write_cstring(buffer, "(TUPLE: ");
			for (u64 i = 0; i < expr->tuple.elem_count; i++) {
				if (i) write_cstring(buffer, ", ");
				Expression* elem = expr->tuple.elems[i];
				write_expression(buffer, elem);
			}

			write_cstring(buffer, ")");
		} break;
	}
}

static void write_ast_kind(OutputBuffer* buffer, AstKind kind) {
	char* s = 0;
	switch (kind) {
		case AST_INVALID:                      s = "AST_INVALID";                       break;

		// -------------------------------------------------- //

		case AST_FUNCTION:                     s = "AST_FUNCTION";                     break;
		case AST_VARIABLE:                     s = "AST_VARIABLE";                     break;
		case AST_BRANCH:                       s = "AST_BRANCH";                       break;
		case AST_CONTROLFLOW:                  s = "AST_CONTROLFLOW";                  break;

		// -------------------------------------------------- //

		case AST_EXPR_NULL:                    s = "AST_EXPR_NULL";                    break;
		case AST_EXPR_TRUE:                    s = "AST_EXPR_TRUE";                    break;
		case AST_EXPR_FALSE:                   s = "AST_EXPR_FALSE";                   break;
		case AST_EXPR_LITERAL:                 s = "AST_EXPR_LITERAL";                 break;
		case AST_EXPR_FUNCTION:                s = "AST_EXPR_FUNCTION";                break;
		case AST_EXPR_BASETYPE_PRIMITIVE:      s = "AST_EXPR_BASETYPE_PRIMITIVE";      break;
		case AST_EXPR_BASETYPE_IDENTIFIER:     s = "AST_EXPR_BASETYPE_IDENTIFIER";     break;
		case AST_EXPR_IDENTIFIER_CONSTANT:     s = "AST_EXPR_IDENTIFIER_CONSTANT";     break;
		case AST_EXPR_IDENTIFIER_FORMAL:       s = "AST_EXPR_IDENTIFIER_FORMAL";       break;
		case AST_EXPR_IDENTIFIER_VARIABLE:     s = "AST_EXPR_IDENTIFIER_VARIABLE";     break;

		case AST_EXPR_ARRAY:                   s = "AST_EXPR_ARRAY";                   break;
		case AST_EXPR_TUPLE:                   s = "AST_EXPR_TUPLE";                   break;

		case AST_EXPR_SPEC_PTR:                s = "AST_EXPR_SPEC_PTR";                break;
		case AST_EXPR_SPEC_ARRAY:              s = "AST_EXPR_SPEC_ARRAY";              break;
		case AST_EXPR_SPEC_FIXED:              s = "AST_EXPR_SPEC_FIXED";              break;

		case AST_EXPR_UNARY_PTR:               s = "AST_EXPR_UNARY_PTR";               break;
		case AST_EXPR_UNARY_REF:               s = "AST_EXPR_UNARY_REF";               break;
		case AST_EXPR_UNARY_ABS:               s = "AST_EXPR_UNARY_ABS";               break;
		case AST_EXPR_UNARY_INVERSE:           s = "AST_EXPR_UNARY_INVERSE";           break;
		case AST_EXPR_UNARY_NOT:               s = "AST_EXPR_UNARY_NOT";               break;
		case AST_EXPR_UNARY_BIT_NOT:           s = "AST_EXPR_UNARY_BIT_NOT";           break;

		case AST_EXPR_UNARY_IMPLICIT_CAST:     s = "AST_EXPR_UNARY_IMPLICIT_CAST";     break;

		case AST_EXPR_BINARY_ADD:              s = "AST_EXPR_BINARY_ADD";              break;
		case AST_EXPR_BINARY_SUB:              s = "AST_EXPR_BINARY_SUB";              break;
		case AST_EXPR_BINARY_MUL:              s = "AST_EXPR_BINARY_MUL";              break;
		case AST_EXPR_BINARY_DIV:              s = "AST_EXPR_BINARY_DIV";              break;
		case AST_EXPR_BINARY_MOD:              s = "AST_EXPR_BINARY_MOD";              break;
		case AST_EXPR_BINARY_BIT_XOR:          s = "AST_EXPR_BINARY_BIT_XOR";          break;
		case AST_EXPR_BINARY_BIT_AND:          s = "AST_EXPR_BINARY_BIT_AND";          break;
		case AST_EXPR_BINARY_BIT_OR:           s = "AST_EXPR_BINARY_BIT_OR";           break;
		case AST_EXPR_BINARY_OR:               s = "AST_EXPR_BINARY_OR";               break;
		case AST_EXPR_BINARY_AND:              s = "AST_EXPR_BINARY_AND";              break;
		case AST_EXPR_BINARY_EQUAL:            s = "AST_EXPR_BINARY_EQUAL";            break;
		case AST_EXPR_BINARY_NOT_EQUAL:        s = "AST_EXPR_BINARY_NOT_EQUAL";        break;
		case AST_EXPR_BINARY_LESS:             s = "AST_EXPR_BINARY_LESS";             break;
		case AST_EXPR_BINARY_LESS_OR_EQUAL:    s = "AST_EXPR_BINARY_LESS_OR_EQUAL";    break;
		case AST_EXPR_BINARY_GREATER:          s = "AST_EXPR_BINARY_GREATER";          break;
		case AST_EXPR_BINARY_GREATER_OR_EQUAL: s = "AST_EXPR_BINARY_GREATER_OR_EQUAL"; break;
		case AST_EXPR_BINARY_DOT:              s = "AST_EXPR_BINARY_DOT";              break;
		case AST_EXPR_BINARY_DOT_DOT:          s = "AST_EXPR_BINARY_DOT_DOT";          break;
		case AST_EXPR_BINARY_LSHIFT:           s = "AST_EXPR_BINARY_LSHIFT";           break;
		case AST_EXPR_BINARY_RSHIFT:           s = "AST_EXPR_BINARY_RSHIFT";           break;
		case AST_EXPR_BINARY_SPAN:             s = "AST_EXPR_BINARY_SPAN";             break;
		case AST_EXPR_CALL:                    s = "AST_EXPR_CALL";                    break;
		case AST_EXPR_INDEX:                   s = "AST_EXPR_INDEX";                   break;

		case AST_EXPR_TERNARY_IF_ELSE:         s = "AST_EXPR_TERNARY_IF_ELSE";         break;

		// -------------------------------------------------- //

		case AST_STATEMENT_ASSIGNMENT:         s = "AST_STATEMENT_ASSIGNMENT";         break;

		case AST_STATEMENT_ASSIGNMENT_LSH:     s = "AST_STATEMENT_ASSIGNMENT_LSH";     break;
		case AST_STATEMENT_ASSIGNMENT_RSH:     s = "AST_STATEMENT_ASSIGNMENT_RSH";     break;
		case AST_STATEMENT_ASSIGNMENT_DIV:     s = "AST_STATEMENT_ASSIGNMENT_DIV";     break;
		case AST_STATEMENT_ASSIGNMENT_MOD:     s = "AST_STATEMENT_ASSIGNMENT_MOD";     break;
		case AST_STATEMENT_ASSIGNMENT_ADD:     s = "AST_STATEMENT_ASSIGNMENT_ADD";     break;
		case AST_STATEMENT_ASSIGNMENT_SUB:     s = "AST_STATEMENT_ASSIGNMENT_SUB";     break;
		case AST_STATEMENT_ASSIGNMENT_MUL:     s = "AST_STATEMENT_ASSIGNMENT_MUL";     break;
		case AST_STATEMENT_ASSIGNMENT_BIT_XOR: s = "AST_STATEMENT_ASSIGNMENT_BIT_XOR"; break;
		case AST_STATEMENT_ASSIGNMENT_BIT_AND: s = "AST_STATEMENT_ASSIGNMENT_BIT_AND"; break;
		case AST_STATEMENT_ASSIGNMENT_BIT_OR:  s = "AST_STATEMENT_ASSIGNMENT_BIT_OR";  break;

		case AST_STATEMENT_EXPRESSION:         s = "AST_STATEMENT_EXPRESSION";         break;
		case AST_STATEMENT_CONTROLFLOW:        s = "AST_STATEMENT_CONTROLFLOW";        break;
		case AST_STATEMENT_VARDECL:            s = "AST_STATEMENT_VARDECL";            break;
		case AST_STATEMENT_RETURN:             s = "AST_STATEMENT_RETURN";             break;
		case AST_STATEMENT_BREAK:              s = "AST_STATEMENT_BREAK";              break;
		case AST_STATEMENT_CONTINUE:           s = "AST_STATEMENT_CONTINUE";           break;
		case AST_STATEMENT_INC:                s = "AST_STATEMENT_INC";                break;
		case AST_STATEMENT_DEC:                s = "AST_STATEMENT_DEC";                break;

		// -------------------------------------------------- //

		case AST_STRUCT:                       s = "AST_STRUCT";                       break;
		case AST_STRUCT_MEMBER:                s = "AST_STRUCT_MEMBER";                break;

		// -------------------------------------------------- //

		case AST_ENUM:                         s = "AST_ENUM";                         break;
		case AST_ENUM_MEMBER:                  s = "AST_ENUM_MEMBER";                  break;
	}

	write_cstring(buffer, s);
}

static void write_statement(OutputBuffer* buffer, Statement* statement) {
	if (!statement)
		write_cstring(buffer, "null");

	write_ast_kind(buffer, statement->kind);

	switch (statement->kind) {
		default: assert_unreachable();

		case AST_STATEMENT_ASSIGNMENT_LSH:
		case AST_STATEMENT_ASSIGNMENT_RSH:
		case AST_STATEMENT_ASSIGNMENT_DIV:
		case AST_STATEMENT_ASSIGNMENT_MOD:
		case AST_STATEMENT_ASSIGNMENT_ADD:
		case AST_STATEMENT_ASSIGNMENT_SUB:
		case AST_STATEMENT_ASSIGNMENT_MUL:
		case AST_STATEMENT_ASSIGNMENT_BIT_XOR:
		case AST_STATEMENT_ASSIGNMENT_BIT_AND:
		case AST_STATEMENT_ASSIGNMENT_BIT_OR:

		case AST_STATEMENT_ASSIGNMENT:
			printbuff(
				buffer,
				":\n\tleft  = %\n\tright = %",
				arg_expression(statement->assign.left),
				arg_expression(statement->assign.right)
			);
			break;

		case AST_STATEMENT_EXPRESSION:
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
			break;

		case AST_STATEMENT_CONTROLFLOW:
			write_cstring(buffer, "");
			break;

		case AST_STATEMENT_VARDECL:
			printbuff(
				buffer,
				":\n\tname = %\n\ttype = %\n\tinit = %",
				arg_token(statement->var->name),
				arg_expression(statement->var->type_expr),
				arg_expression(statement->var->init_expr)
			);
			break;

		case AST_STATEMENT_RETURN:
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
			break;

		case AST_STATEMENT_CONTINUE:
			break;

		case AST_STATEMENT_BREAK:
			break;

		case AST_STATEMENT_INC:
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
			break;

		case AST_STATEMENT_DEC:
			printbuff(
				buffer,
				":\n\texpr = %",
				arg_expression(statement->expr)
			);
			break;
	}
}

static void write_ast(OutputBuffer* buffer, Ast* ast) {
	if (!ast) {
		write_cstring(buffer, "null");
		return;
	}

	write_ast_kind(buffer, ast->kind);

	if (ast->kind != AST_INVALID) {
	 	write_cstring(buffer, " ");
	}

	switch (ast->kind) {
		case AST_INVALID:
			return;

		case AST_BRANCH:
			write_cstring(buffer, "branch");
			return;

		case AST_CONTROLFLOW:
			write_cstring(buffer, "controlflow");
			return;

		case AST_FUNCTION:
			write_token(buffer, ast->func.name);
			return;

		case AST_VARIABLE:
			write_token(buffer, ast->var.name);
			return;

		case AST_STRUCT:
			write_token(buffer, ast->ustruct.name);
			return;

		case AST_STRUCT_MEMBER:
			write_token(buffer, ast->ustruct_field.name);
			return;

		case AST_ENUM:
			write_token(buffer, ast->uenum.name);
			return;

		case AST_ENUM_MEMBER:
			write_token(buffer, ast->uenum_field.name);
			return;

		case AST_EXPR_NULL:
		case AST_EXPR_TRUE:
		case AST_EXPR_FALSE:
		case AST_EXPR_LITERAL:
		case AST_EXPR_FUNCTION:
		case AST_EXPR_BASETYPE_PRIMITIVE:
		case AST_EXPR_BASETYPE_IDENTIFIER:
		case AST_EXPR_IDENTIFIER_CONSTANT:
		case AST_EXPR_IDENTIFIER_FORMAL:
		case AST_EXPR_IDENTIFIER_VARIABLE:
		case AST_EXPR_ARRAY:
		case AST_EXPR_TUPLE:
		case AST_EXPR_SPEC_PTR:
		case AST_EXPR_SPEC_ARRAY:
		case AST_EXPR_SPEC_FIXED:
		case AST_EXPR_UNARY_PTR:
		case AST_EXPR_UNARY_REF:
		case AST_EXPR_UNARY_ABS:
		case AST_EXPR_UNARY_INVERSE:
		case AST_EXPR_UNARY_NOT:
		case AST_EXPR_UNARY_BIT_NOT:
		case AST_EXPR_UNARY_IMPLICIT_CAST:
		case AST_EXPR_BINARY_ADD:
		case AST_EXPR_BINARY_SUB:
		case AST_EXPR_BINARY_MUL:
		case AST_EXPR_BINARY_DIV:
		case AST_EXPR_BINARY_MOD:
		case AST_EXPR_BINARY_BIT_XOR:
		case AST_EXPR_BINARY_BIT_AND:
		case AST_EXPR_BINARY_BIT_OR:
		case AST_EXPR_BINARY_OR:
		case AST_EXPR_BINARY_AND:
		case AST_EXPR_BINARY_EQUAL:
		case AST_EXPR_BINARY_NOT_EQUAL:
		case AST_EXPR_BINARY_LESS:
		case AST_EXPR_BINARY_LESS_OR_EQUAL:
		case AST_EXPR_BINARY_GREATER:
		case AST_EXPR_BINARY_GREATER_OR_EQUAL:
		case AST_EXPR_BINARY_DOT:
		case AST_EXPR_BINARY_DOT_DOT:
		case AST_EXPR_BINARY_LSHIFT:
		case AST_EXPR_BINARY_RSHIFT:
		case AST_EXPR_BINARY_SPAN:
		case AST_EXPR_CALL:
		case AST_EXPR_INDEX:
		case AST_EXPR_TERNARY_IF_ELSE:
			write_expression(buffer, (Expression*)ast);
			return;

		case AST_STATEMENT_ASSIGNMENT:
		case AST_STATEMENT_ASSIGNMENT_LSH:
		case AST_STATEMENT_ASSIGNMENT_RSH:
		case AST_STATEMENT_ASSIGNMENT_DIV:
		case AST_STATEMENT_ASSIGNMENT_MOD:
		case AST_STATEMENT_ASSIGNMENT_ADD:
		case AST_STATEMENT_ASSIGNMENT_SUB:
		case AST_STATEMENT_ASSIGNMENT_MUL:
		case AST_STATEMENT_ASSIGNMENT_BIT_XOR:
		case AST_STATEMENT_ASSIGNMENT_BIT_AND:
		case AST_STATEMENT_ASSIGNMENT_BIT_OR:
		case AST_STATEMENT_EXPRESSION:
		case AST_STATEMENT_CONTROLFLOW:
		case AST_STATEMENT_VARDECL:
		case AST_STATEMENT_RETURN:
		case AST_STATEMENT_BREAK:
		case AST_STATEMENT_CONTINUE:
		case AST_STATEMENT_INC:
		case AST_STATEMENT_DEC:
			write_statement(buffer, (Statement*)ast);
			return;
	}
}

static void print_argument(OutputBuffer* buffer, FormatArg arg) {
	switch (arg.kind) {
		case PRINT_ARG_BOOL:            write_bool(buffer, arg.b); return;

		case PRINT_ARG_INT8:            write_s64(buffer, arg.s8);  return;
		case PRINT_ARG_INT16:           write_s64(buffer, arg.s16); return;
		case PRINT_ARG_INT32:           write_s64(buffer, arg.s32); return;
		case PRINT_ARG_INT64:           write_s64(buffer, arg.s64); return;

		case PRINT_ARG_UINT8:           write_u64(buffer, arg.u8);  return;
		case PRINT_ARG_UINT16:          write_u64(buffer, arg.u16); return;
		case PRINT_ARG_UINT32:          write_u64(buffer, arg.u32); return;
		case PRINT_ARG_UINT64:          write_u64(buffer, arg.u64); return;

		case PRINT_ARG_F32:             write_f32(buffer, arg.f); return;
		case PRINT_ARG_F64:             write_f64(buffer, arg.d); return;
		case PRINT_ARG_CHAR:            write_char(buffer, arg.c);  return;
		case PRINT_ARG_STRING:          write_string(buffer, arg.str); return;
		case PRINT_ARG_CSTRING:         write_cstring(buffer, arg.s);  return;

		case PRINT_ARG_TOKEN:           write_token(buffer, arg.token); return;
		case PRINT_ARG_TOKEN_KIND:      write_token_kind(buffer, arg.token_kind); return;

		case PRINT_ARG_AST_KIND:        write_ast_kind(buffer, arg.ast_kind); return;
		case PRINT_ARG_AST:             write_ast(buffer, arg.ast); return;
		case PRINT_ARG_EXPRESSION:      write_expression(buffer, arg.expr); return;
		case PRINT_ARG_STATEMENT:       write_statement(buffer, arg.statement); return;
		case PRINT_ARG_TYPE:            write_type(buffer, arg.type); return;
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

