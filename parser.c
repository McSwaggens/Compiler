#include "ast.h"
#include "print.h"
#include "alloc.h"

static
bool is_correct_indent(Token* token, Indent16 indent) {
	return !is_newline(token) || token->indent == indent;
}

static inline
Expression* alloc_expression(void) {
	return stack_alloc(sizeof(Expression));
}

static
void scope_push_var(Scope* scope, Variable* var) {
	if (scope->variable_count == scope->variable_capacity) {
		scope->variable_capacity = next_pow2(scope->variable_capacity+7);
		scope->variables = realloc(
			scope->variables,
			sizeof(Variable*)*scope->variable_count,
			sizeof(Variable*)*scope->variable_capacity
		);
	}

	scope->variables[scope->variable_count++] = var;
}

static
void push_statement(Code* code, Statement statement) {
	if (code->statement_count == code->statement_capacity) {
		code->statement_capacity = next_pow2(code->statement_capacity+15);
		code->statements = realloc(
			code->statements,
			sizeof(Statement)*code->statement_count,
			sizeof(Statement)*code->statement_capacity
		);
	}

	code->statements[code->statement_count++] = statement;
}

// @Note: Don't use 15, it's reserved for parsing a single expression.
static
u8 unary_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_PLUS:
		case TOKEN_MINUS:
		case TOKEN_ASTERISK:
		case TOKEN_EXCLAMATION:
		case TOKEN_TILDA:
		case TOKEN_AT:
		case TOKEN_PIKE:
		case TOKEN_BACK_SLASH:
		case TOKEN_LESS:
		case TOKEN_OPEN_BRACKET:     return 13;
		default:                     return 0;
	}
}

static
u8 postfix_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_OPEN_BRACKET:
		case TOKEN_OPEN_PAREN:       return 14;
		default:                     return 0;
	}
}

static
u8 binary_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_DOT:              return 14;

		case TOKEN_ARROW:            return 8;

		case TOKEN_ASTERISK:
		case TOKEN_SLASH:
		case TOKEN_BACK_SLASH:
		case TOKEN_LEFT_SHIFT:
		case TOKEN_RIGHT_SHIFT:      return 7;

		case TOKEN_PIKE:
		case TOKEN_AMPERSAND:
		case TOKEN_CARET:
		case TOKEN_PLUS:
		case TOKEN_MINUS:            return 6;

		case TOKEN_EQUAL:
		case TOKEN_NOT_EQUAL:
		case TOKEN_LESS:
		case TOKEN_LESS_OR_EQUAL:
		case TOKEN_GREATER:
		case TOKEN_GREATER_OR_EQUAL: return 5;

		case TOKEN_AND:              return 2;
		case TOKEN_OR:               return 1;

		default:                     return 0;
	}
}

static
u8 ternary_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_IF:               return 3;
		default:                     return 0;
	}
}

static
bool is_term(TokenKind kind) {
	switch (kind) {
		case TOKEN_IDENTIFIER_CONSTANT:
		case TOKEN_IDENTIFIER_FORMAL:
		case TOKEN_IDENTIFIER_VARIABLE:
		case TOKEN_TRUE:
		case TOKEN_FALSE:
		case TOKEN_NULL:
		case TOKEN_LITERAL_INT8:
		case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:
		case TOKEN_LITERAL_INT64:
		case TOKEN_LITERAL_UINT8:
		case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32:
		case TOKEN_LITERAL_UINT64:
		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64:
		case TOKEN_LITERAL_STRING:
		case TOKEN_BYTE:
		case TOKEN_BOOL:
		case TOKEN_INT:
		case TOKEN_INT8:
		case TOKEN_INT16:
		case TOKEN_INT32:
		case TOKEN_INT64:
		case TOKEN_UINT:
		case TOKEN_UINT8:
		case TOKEN_UINT16:
		case TOKEN_UINT32:
		case TOKEN_UINT64:
		case TOKEN_FLOAT32:
		case TOKEN_FLOAT64:
		case TOKEN_TYPE_ID:
		case TOKEN_OPEN_BRACE:
		case TOKEN_OPEN_PAREN:
		case TOKEN_OPEN_BRACKET:
			return true;
		default:
			return false;
	}
}

static
bool is_scope_terminator(TokenKind kind) {
	switch (kind) {
		case TOKEN_SEMICOLON:
		case TOKEN_ELSE:
		case TOKEN_THEN:
			return true;
		default:
			return false;
	}
}

static
bool is_expression_starter(TokenKind kind) {
	return unary_precedence(kind) || is_term(kind);
}

static
bool is_branch(TokenKind kind) {
	switch (kind) {
		case TOKEN_IF:
		case TOKEN_WHILE:
		case TOKEN_FOR:
		case TOKEN_MATCH:
			return true;
		default:
			return false;
	}
}

static
bool is_identifier(TokenKind kind) {
	switch (kind) {
		case TOKEN_IDENTIFIER_CONSTANT:
		case TOKEN_IDENTIFIER_FORMAL:
		case TOKEN_IDENTIFIER_VARIABLE:
			return true;
		default:
			return false;
	}
}

static
void check_indent(Token* token, Indent16 indent) {
	if (token->kind == TOKEN_EOF)
		return;

	if (!is_correct_indent(token, indent)) {
		errort(token,
			"Incorrect indentation, expected indent of %, not: %\n",
			arg_u16(indent),
			arg_u16(token->indent)
		);
	}
}

static
u8 correct_unary_precedence(u8 precedence, Token* token) {
	if (!is_rspace(token))
		precedence <<= 4;

	return precedence;
}

static
u8 correct_binary_precedence(u8 precedence, Token* token) {
	if (!is_lspace(token) && !is_rspace(token))
		precedence <<= 4;

	return precedence;
}

static Token* parse_code(Module* module, Token* token, Indent16 indent, Code* code);
static Token* parse_expression(Module* module, Token* token, Indent16 indent, bool allow_equals, Expression** out);

static
Token* parse_struct(Module* module, Token* token, Indent16 indent) {
	assert(token->kind == TOKEN_STRUCT);

	Struct* ast = &module->structs[module->struct_count++];
	*ast = (Struct){ 0 };

	token++;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
		errort(token, "Expected formal name after 'struct', not: %\n", arg_token(token));
	check_indent(token, indent);

	ast->name = token;
	token++;

	// print("Parsing struct: %\n", arg_token(ast->name));

	if (token->kind != TOKEN_COLON)
		errort(token, "Expected ':', not: %\n", arg_token(token));
	check_indent(token, indent);
	token++;

	while (is_correct_indent(token, indent+1)) {
		StructField field = { 0 };

		if (token->kind == TOKEN_IDENTIFIER_CONSTANT || token->kind == TOKEN_IDENTIFIER_FORMAL)
			errort(token, "Struct field with constant name\n");

		if (token->kind != TOKEN_IDENTIFIER_VARIABLE)
			errort(token, "Expected field name, not: %\n", arg_token(token));

		field.name = token;
		token++;

		if (token->kind != TOKEN_COLON)
			errort(token, "Expected ':', not: %\n", arg_token(token));
		check_indent(token, indent+2);
		token++;

		if (!is_expression_starter(token->kind))
			errort(token, "Expected type after ':', not: %\n", arg_token(token));

		token = parse_expression(module, token, indent+2, true, &field.type);

		ast->fields = realloc(ast->fields, sizeof(StructField)*(ast->field_count), sizeof(StructField)*(ast->field_count+1));
		ast->fields[ast->field_count++] = field;

		if (token->kind == TOKEN_SEMICOLON && is_correct_indent(token, indent+1)) {
			token++;
			continue;
		}

		if (!is_newline(token) && token->kind == TOKEN_IDENTIFIER_VARIABLE)
			errort(token, "Missing ';' before next field: %\n", arg_token(token));

		if (!is_newline(token))
			errort(token, "Unexpected token: %\n", arg_token(token));
	}

	return token;
}

static
Token* parse_enum(Module* module, Token* token, Indent16 indent) {
	assert(token->kind == TOKEN_ENUM);

	Enum* ast = &module->enums[module->enum_count++];
	*ast = (Enum){ 0 };

	token++;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
		errort(token, "Expected formal name after 'enum', not: %\n", arg_token(token));
	check_indent(token, indent);

	ast->name = token;
	token++;

	if (token->kind == TOKEN_ARROW) {
		check_indent(token, indent+1);
		token++;

		if (!is_expression_starter(token->kind))
			errort(token, "Expected type after '->', not: %\n", arg_token(token));

		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, true, &ast->type);
	}

	if (token->kind != TOKEN_COLON)
		errort(token, "Expected ':', not: %\n", arg_token(token));
	check_indent(token, indent);
	token++;

	while (is_correct_indent(token, indent+1)) {
		EnumField field = { 0 };

		if (token->kind == TOKEN_IDENTIFIER_VARIABLE)
			errort(token, "Enum field with variable name is not allowed: %\n", arg_token(token));

		if (token->kind != TOKEN_IDENTIFIER_CONSTANT)
			errort(token, "Expected field name, not: %\n", arg_token(token));

		field.name = token;
		token++;

		if (token->kind == TOKEN_EQUAL && is_correct_indent(token, indent+2)) {
			check_indent(token, indent+2);
			token++;

			check_indent(token, indent+2);
			if (!is_expression_starter(token->kind))
				errort(token, "Expected expression after '=', not: %\n", arg_token(token));

			token = parse_expression(module, token, indent+2, true, &field.value);
		}

		ast->fields = realloc(ast->fields, sizeof(EnumField)*(ast->field_count), sizeof(EnumField)*(ast->field_count+1));
		ast->fields[ast->field_count++] = field;

		if (token->kind == TOKEN_SEMICOLON && is_correct_indent(token, indent+1)) {
			token++;
			continue;
		}

		if (!is_newline(token) && token->kind == TOKEN_IDENTIFIER_FORMAL)
			errort(token, "Missing ';' before next field: %\n", arg_token(token));

		if (!is_newline(token))
			errort(token, "Unexpected token: %\n", arg_token(token));
	}

	return token;
}

typedef struct IndentHelper {
	u64 bracket_level;
	Indent16 indent;
	bool locked;
	s8 adjustment;
} IndentHelper;

static inline
void ih_enter(IndentHelper* helper) {
	helper->bracket_level++;
}

static inline
void ih_leave(IndentHelper* helper) {
	helper->bracket_level--;
}

static
void ih_check(Token* token, IndentHelper* helper, s32 adjustment) {
	if (token->kind == TOKEN_EOF)
		return;

	if (!is_newline(token))
		return;

	Indent16 expected_indent = helper->indent + helper->bracket_level + helper->adjustment + adjustment;

	if (token->indent == expected_indent) {
		helper->locked = true;
		return;
	}

	if (!helper->locked && helper->bracket_level && token->indent == expected_indent-1) {
		helper->locked = true;
		helper->adjustment = -1;
		return;
	}

	errort(token, "Invalid indent\n");
}

static
bool ih_test(Token* token, IndentHelper* helper, s32 adjustment) {
	if (token->kind == TOKEN_EOF)
		return false;

	if (!is_newline(token))
		return true;

	Indent16 expected_indent = helper->indent + helper->bracket_level + helper->adjustment + adjustment;

	return token->indent == expected_indent || token->indent == expected_indent-1;
}

static
Token* internal_parse_expression(Module* module, Token* token, bool allow_equals, u8 parent_precedence, IndentHelper* helper, Expression** out) {
	// Make sure to call ih_check before internal_parse_expression
	Expression* left = null;
	Token* begin = token;

	u8 unaryprec = correct_unary_precedence(unary_precedence(token->kind), token);

	switch (token->kind) {
		ExpressionKind kind;

		case TOKEN_IDENTIFIER_CONSTANT: kind = EXPR_IDENTIFIER_CONSTANT; goto GOTO_TERM;
		case TOKEN_IDENTIFIER_FORMAL:   kind = EXPR_IDENTIFIER_FORMAL;   goto GOTO_TERM;
		case TOKEN_IDENTIFIER_VARIABLE: kind = EXPR_IDENTIFIER_VARIABLE; goto GOTO_TERM;

		case TOKEN_TRUE:  kind = EXPR_TRUE;  goto GOTO_TERM;
		case TOKEN_FALSE: kind = EXPR_FALSE; goto GOTO_TERM;
		case TOKEN_NULL:  kind = EXPR_NULL;  goto GOTO_TERM;

		case TOKEN_LITERAL_INT8:
		case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:
		case TOKEN_LITERAL_INT64:
		case TOKEN_LITERAL_UINT8:
		case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32:
		case TOKEN_LITERAL_UINT64:
			kind = EXPR_LITERAL;
			goto GOTO_TERM;

		case TOKEN_LITERAL_FLOAT32:
		case TOKEN_LITERAL_FLOAT64:
		case TOKEN_LITERAL_STRING:
			kind = EXPR_LITERAL;
			goto GOTO_TERM;

		case TOKEN_BYTE:   case TOKEN_BOOL:   case TOKEN_INT:     case TOKEN_INT8:    case TOKEN_INT16:
		case TOKEN_INT32:  case TOKEN_INT64:  case TOKEN_UINT:    case TOKEN_UINT8:   case TOKEN_UINT16:
		case TOKEN_UINT32: case TOKEN_UINT64: case TOKEN_FLOAT32: case TOKEN_FLOAT64: case TOKEN_TYPE_ID:
			kind = EXPR_BASETYPE_PRIMITIVE;
			goto GOTO_TERM;

		GOTO_TERM: {
			left = alloc_expression();
			*left = (Expression){
				.kind = kind,
				.term.token = token,
			};

			token++;
		} break;

		case TOKEN_ASTERISK:    kind = EXPR_UNARY_PTR;     goto GOTO_UNARY;
		case TOKEN_AT:          kind = EXPR_UNARY_REF;     goto GOTO_UNARY;
		case TOKEN_EXCLAMATION: kind = EXPR_UNARY_NOT;     goto GOTO_UNARY;
		case TOKEN_TILDA:       kind = EXPR_UNARY_BIT_NOT; goto GOTO_UNARY;
		case TOKEN_MINUS:       kind = EXPR_UNARY_INVERSE; goto GOTO_UNARY;
		case TOKEN_PLUS:        kind = EXPR_UNARY_ABS;     goto GOTO_UNARY;
		GOTO_UNARY: {
			left = alloc_expression();
			*left = (Expression){
				.kind = kind,
				.unary.optoken = token,
			};

			token++;

			ih_check(token, helper, 0);
			token = internal_parse_expression(module, token, allow_equals, unaryprec, helper, &left->unary.sub);

			if (!left->unary.sub)
				errort(token, "Expected expression after unary '%' operator\n", arg_token(begin));
		} break;

		case TOKEN_OPEN_BRACE: {
			left = alloc_expression();
			*left = (Expression){
				.kind = EXPR_ARRAY,
			};

			Token* open = token;

			if (!open->closure)
				errort(open, "Fixed array literal missing closing '}'\n");

			// ih_check(token, helper, 0);
			ih_enter(helper);
			token++; // {

			Expression** elems = null;
			u64 count = 0;

			if (token->kind != TOKEN_CLOSE_BRACE) {
				count = open->comma_count+1;
				elems = alloc(sizeof(Expression*)*count);

				Expression** elem = elems;

				while (true) {
					if (token->kind == TOKEN_COMMA)
						errort(token, "Missing expression before ','\n");

					ih_check(token, helper, 0);
					token = internal_parse_expression(module, token, true, 0, helper, elem);

					if (!*elem)
						errort(token, "Expected expression in array literal, not: '%'\n", arg_token(token));

					elem++;

					if (token->kind == TOKEN_CLOSE_BRACE)
						break;

					if (token->kind != TOKEN_COMMA) {
						if (is_expression_starter(token->kind) && ih_test(token, helper, 0))
							errort(token, "Missing ',' before expression\n");

						errort(token, "Unexpected token %, expected ',' or '}'\n", arg_token(token));
					}

					ih_check(token, helper, -1); // ,
					token++;
				}
			}

			ih_leave(helper);
			ih_check(token, helper, 0);
			token++; // }

			left->array.elems = elems;
			left->array.elem_count = count;

		} break;

		case TOKEN_OPEN_PAREN: {
			left = alloc_expression();
			*left = (Expression){
				.kind = EXPR_TUPLE,
			};

			// ih_check(token, helper, 0);
			ih_enter(helper);
			token++; // {

			// print("Starting tuple parse...\n");

			Expression** elems = null;
			u64 count = 0;


			if (token->kind != TOKEN_CLOSE_PAREN)
			while (true) {
				if (token->kind == TOKEN_COMMA)
					errort(token, "Missing expression before ','\n");

				Expression* elem = null;
				ih_check(token, helper, 0);
				token = internal_parse_expression(module, token, true, 0, helper, &elem);

				if (!elem)
					errort(token, "Expected expression in tuple, not: '%'\n", arg_token(token));

				elems = realloc(elems, sizeof(Expression*) * (count), sizeof(Expression*) * (count+1));
				// print("elems = %\n", arg_u64((u64)elems));
				elems[count++] = elem;

				if (token->kind == TOKEN_CLOSE_PAREN)
					break;

				if (token->kind != TOKEN_COMMA)
					errort(token, "Unexpected token %, expected ',' or ')'\n",
						arg_token(token)
					);

				ih_check(token, helper, -1);
				token++;

				if (token->kind == TOKEN_CLOSE_PAREN)
					errort(token, "Missing expression after ','\n");
			}

			ih_leave(helper);
			ih_check(token, helper, 0);
			token++;

			left->tuple.elems = elems;
			left->tuple.elem_count = count;
			// print("Tuple parse finished.\n");

		} break;

		case TOKEN_OPEN_BRACKET: {
			left = alloc_expression();

			ih_enter(helper);
			token++; // [

			if (token->kind == TOKEN_CLOSE_BRACKET) {
				left->kind = EXPR_SPEC_ARRAY; // []e

				// Take subexpression
				ih_check(token+1, helper, 0); // e
				token = internal_parse_expression(module, token+1, allow_equals, unaryprec, helper, &left->specifier.sub);

				if (!left->specifier.sub) {
					errort(token, "Fixed array specifier missing specifier\n");
				}

				break;
			}

			Expression* lexpr = null;

			left->kind = EXPR_SPEC_FIXED; // [n]e
			ih_check(token, helper, 0); // n
			token = internal_parse_expression(module, token, allow_equals, 0, helper, &lexpr);

			if (!lexpr)
				errort(token, "Expected expression after brackets\n");


			if (token->kind == TOKEN_DOT_DOT) {
				left->kind = EXPR_BINARY_SPAN; // [a..b]
				left->span.left = lexpr;
				ih_check(token, helper, -1);  // ..
				ih_check(token+1, helper, 0); // b
				token = internal_parse_expression(module, token+1, allow_equals, 0, helper, &left->span.right);

				if (!left->span.right)
					errort(token, "Span extent expression missing\n");
			}

			if (token->kind != TOKEN_CLOSE_BRACKET)
				errort(token, "Expected ']', not: %\n", arg_token(token));

			ih_leave(helper);
			ih_check(token, helper, 0);
			token++; // ]

			if (left->kind == EXPR_SPEC_FIXED) {
				// [N]e
				left->specifier.length = lexpr;
				ih_check(token, helper, 0); // e
				token = internal_parse_expression(module, token, allow_equals, unaryprec, helper, &left->specifier.sub);

				if (!left->specifier.sub)
					errort(token, "Invalid bracketed expression, valid syntaxes: e[e], [e]e\n");
			}

		} break;

		default:
			break;
	}

	if (left) {
		left->begin = begin;
		left->end = token;
	}

	while (true) {
		if (token->kind == TOKEN_EQUAL && !allow_equals)
			break;

		u8 precedence = binary_precedence(token->kind) | postfix_precedence(token->kind) | ternary_precedence(token->kind);

		if (!precedence)
			break;

		if (is_newline(token) && !helper->bracket_level && token->indent <= helper->indent-1)
			break;

		ih_check(token, helper, 0);

		precedence = correct_binary_precedence(precedence, token);

		if (precedence <= parent_precedence)
			break;

		Expression* expr = alloc_expression();

		switch (token->kind) {
			default:
				errort(token, "parse_expression didn't handle binary operator %\n", arg_token(token));

			case TOKEN_DOT:              expr->kind = EXPR_BINARY_DOT;              goto GOTO_BINARY_OP;

			case TOKEN_ASTERISK:         expr->kind = EXPR_BINARY_MUL;              goto GOTO_BINARY_OP;
			case TOKEN_SLASH:            expr->kind = EXPR_BINARY_DIV;              goto GOTO_BINARY_OP;
			case TOKEN_BACK_SLASH:       expr->kind = EXPR_BINARY_MOD;              goto GOTO_BINARY_OP;
			case TOKEN_LEFT_SHIFT:       expr->kind = EXPR_BINARY_LSHIFT;           goto GOTO_BINARY_OP;
			case TOKEN_RIGHT_SHIFT:      expr->kind = EXPR_BINARY_RSHIFT;           goto GOTO_BINARY_OP;

			case TOKEN_PIKE:             expr->kind = EXPR_BINARY_BIT_OR;           goto GOTO_BINARY_OP;
			case TOKEN_AMPERSAND:        expr->kind = EXPR_BINARY_BIT_AND;          goto GOTO_BINARY_OP;
			case TOKEN_CARET:            expr->kind = EXPR_BINARY_BIT_XOR;          goto GOTO_BINARY_OP;
			case TOKEN_PLUS:             expr->kind = EXPR_BINARY_ADD;              goto GOTO_BINARY_OP;
			case TOKEN_MINUS:            expr->kind = EXPR_BINARY_SUB;              goto GOTO_BINARY_OP;

			case TOKEN_EQUAL:            expr->kind = EXPR_BINARY_EQUAL;            goto GOTO_BINARY_OP;
			case TOKEN_NOT_EQUAL:        expr->kind = EXPR_BINARY_NOT_EQUAL;        goto GOTO_BINARY_OP;
			case TOKEN_LESS:             expr->kind = EXPR_BINARY_LESS;             goto GOTO_BINARY_OP;
			case TOKEN_LESS_OR_EQUAL:    expr->kind = EXPR_BINARY_LESS_OR_EQUAL;    goto GOTO_BINARY_OP;
			case TOKEN_GREATER:          expr->kind = EXPR_BINARY_GREATER;          goto GOTO_BINARY_OP;
			case TOKEN_GREATER_OR_EQUAL: expr->kind = EXPR_BINARY_GREATER_OR_EQUAL; goto GOTO_BINARY_OP;

			case TOKEN_AND:              expr->kind = EXPR_BINARY_AND;              goto GOTO_BINARY_OP;
			case TOKEN_OR:               expr->kind = EXPR_BINARY_OR;               goto GOTO_BINARY_OP;
			GOTO_BINARY_OP: {
				expr->binary.optoken = token;
				expr->binary.left = left;

				token++;

				ih_check(token, helper, 0);
				token = internal_parse_expression(module, token, allow_equals, precedence, helper, &expr->binary.right);

				if (!expr->binary.right) {
					errort(token, "Expected expression after binary '%' operator\n", arg_token(expr->binary.optoken));
				}
			} break;

			case TOKEN_OPEN_BRACKET: {
				expr->kind = EXPR_INDEX;
				expr->subscript.base = left;
				// expr->subscript.token = token;
				token++;

				ih_enter(helper);
				ih_check(token, helper, 0);
				token = internal_parse_expression(module, token, allow_equals, 0, helper, &expr->subscript.index);

				if (!expr->subscript.index) {
					errort(token, "Expression missing in subscript\n");
				}

				if (token->kind != TOKEN_CLOSE_BRACKET) {
					errort(token, "Subscript expression not closed. Expected ']' after index expression, not: %\n", arg_token(token));
				}

				ih_check(token, helper, -1);
				ih_leave(helper);
				token++;
			} break;

			case TOKEN_OPEN_PAREN: {
				*expr = (Expression){
					.kind           = EXPR_CALL,
					.call.function  = left,
					.call.arg_count = 0,
					.call.args      = null,
				};

				ih_enter(helper);
				token++;

				if (token->kind != TOKEN_CLOSE_PAREN)
				while (true) {
					// @Todo: Precompute size
					expr->call.args = realloc(expr->call.args, sizeof(Expression*)*expr->call.arg_count, sizeof(Expression*)*expr->call.arg_count+1);
					ih_check(token, helper, 0);

					Expression* arg = null;
					token = internal_parse_expression(module, token, allow_equals, 0, helper, &arg);
					expr->call.args[expr->call.arg_count] = arg;

					if (!arg)
						errort(token, "Invalid function argument, expected expression, not: '%'\n", arg_token(token));

					expr->call.arg_count++;

					if (token->kind == TOKEN_COMMA) {
						ih_check(token, helper, -1);
						token++;
						continue;
					}

					if (token->kind == TOKEN_CLOSE_PAREN)
						break;

					errort(token, "Unexpected token in function arguments: %\n", arg_token(token));
				}

				ih_check(token, helper, -1);
				ih_leave(helper);
				token++;

			} break;

			case TOKEN_IF: {
				*expr = (Expression){
					.kind = EXPR_TERNARY_IF_ELSE,
					.ternary.left = left,
				};

				ih_check(token+1, helper, 0);
				token = internal_parse_expression(module, token+1, true, precedence, helper, &expr->ternary.middle);

				if (!expr->ternary.middle)
					errort(token, "Condition expression missing from 'if else' operator, unexpected: '%'\n", arg_token(token));

				if (token->kind != TOKEN_ELSE)
					errort(token, "Expected 'else' not: %\n", arg_token(token));

				ih_check(token+1, helper, 0);
				token = internal_parse_expression(module, token+1, allow_equals, precedence, helper, &expr->ternary.right);

				if (!expr->ternary.right)
					errort(token, "Expected expression after 'else', not: '%'\n", arg_token(token));
			} break;
		}

		expr->begin = begin;
		expr->end = token;
		left = expr;
	}

	*out = left;
	return token;
}

static
Token* parse_expression(Module* module, Token* token, Indent16 indent, bool allow_equals, Expression** out) {
	// if (token->kind == TOKEN_EOF)
	// 	return token;

	// assert(is_expression_starter(token->kind));

	IndentHelper helper = {
		.locked = false,
		.adjustment = false,
		.indent = indent,
		.bracket_level = 0,
	};

	token = internal_parse_expression(module, token, allow_equals, 0, &helper, out);
	// print("Parsed expression: %\n", arg_expression(*out));

	return token;
}

static
Token* parse_variable_declaration(Module* module, Token* token, Indent16 indent, Variable* out) {
	assert(is_identifier(token->kind));

	switch (token->kind) {
		case TOKEN_IDENTIFIER_CONSTANT: out->flags |= VAR_CONSTANT; break;
		case TOKEN_IDENTIFIER_VARIABLE: out->flags |= VAR_VARIABLE; break;
		default: assert_unreachable();
	}

	bool is_const = (out->flags & VAR_CONSTANT);
	out->name = token;
	token++;

	if (token->kind != TOKEN_COLON)
		errort(token, "Variable declaration expected ':', not: %\n", arg_token(token));
	check_indent(token, indent+1);
	token++;

	if (is_expression_starter(token->kind)) {
		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, false, &out->type_expr);
	}

	if (!out->type_expr && token->kind != TOKEN_EQUAL)
		errort(token, "Invalid variable declaration, expected type or '=', not: %\n", arg_token(token));

	if (token->kind == TOKEN_EQUAL) {
		check_indent(token, indent+1);
		token++;

		if (!is_expression_starter(token->kind))
			errort(token, "Expected expression after '='\n", arg_token(token));

		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, false, &out->init_expr);
	}

	if (is_const && !out->init_expr)
		errort(token, "Constant declared without a value.\n");

	return token;
}

static
Token* parse_if(Module* module, Token* token, Indent16 indent, Branch* branch) {
	branch->kind = BRANCH_IF;
	token++;

	if (!is_expression_starter(token->kind)) {
		errort(token, "Missing condition expression in 'if' branch\n");
	}

	check_indent(token, indent+1);
	token = parse_expression(module, token, indent+1, true, &branch->cond);

	if (token->kind != TOKEN_COLON)
		errort(token, "Expected ':', not: %\n", arg_token(token));
	check_indent(token, indent+1);
	token++; // :

	token = parse_code(module, token, indent+1, &branch->code);

	return token;
}

static
Token* parse_while(Module* module, Token* token, Indent16 indent, Branch* branch) {
	branch->kind = BRANCH_WHILE;
	token++;

	if (!is_expression_starter(token->kind)) {
		errort(token, "Missing condition expression in 'while' branch\n");
	}

	check_indent(token, indent+1);
	token = parse_expression(module, token, indent+1, true, &branch->cond);

	if (token->kind != TOKEN_COLON)
		errort(token, "Expected ':', not: '%'\n", arg_token(token));
	check_indent(token, indent+1);
	token++; // :

	token = parse_code(module, token, indent+1, &branch->code);

	return token;
}

static
Token* parse_for(Module* module, Token* token, Indent16 indent, Branch* branch) {
	branch->kind = BRANCH_FOR;
	token++;

	if (token->kind != TOKEN_IDENTIFIER_VARIABLE) {
		errort(token, "Expected variable declaration after 'for', not: '%'\n", arg_token(token));
	}
	check_indent(token, indent+1);

	branch->var = stack_alloc(sizeof(Variable));
	token = parse_variable_declaration(module, token, indent+1, branch->var);

	if (token->kind != TOKEN_COMMA)
		errort(token, "Expected ',' after initial expression, not: '%'\n", arg_token(token));
	check_indent(token, indent+1);
	token++;

	if (!is_expression_starter(token->kind))
		errort(token, "Expected condition expression not: '%'\n", arg_token(token));

	check_indent(token, indent+1);
	token = parse_expression(module, token, indent+1, true, &branch->cond);

	if (token->kind == TOKEN_COMMA) {
		token++;

		if (!is_expression_starter(token->kind))
			errort(token, "Expected expression after ','\n");

		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, true, &branch->new);
	}

	if (token->kind != TOKEN_COLON)
		errort(token, "Expected ':', not: '%'\n", arg_token(token));
	check_indent(token, indent+1);
	token++; // :

	token = parse_code(module, token, indent+1, &branch->code);

	return token;
}

static
Token* parse_match(Module* module, Token* token, Indent16 indent, Branch* branch) {
	branch->kind = BRANCH_MATCH;
	token++; // match

	if (!is_expression_starter(token->kind))
		errort(token, "Expected expression not: '%'\n", arg_token(token+1));

	token = parse_expression(module, token, indent+1, true, &branch->cond);

	if (token->kind != TOKEN_COLON)
		errort(token, "Expected ':', not: '%'\n", arg_token(token));
	check_indent(token, indent+1);
	token++; // :

	while (is_correct_indent(token, indent+1)) {
		MatchGroup group = { 0 };

		if (token->kind == TOKEN_THEN) {
			check_indent(token,   indent+1); // then
			token++;

			if (token->kind != TOKEN_COLON)
				errort(token, "Expected ':' after 'then', not: %\n", arg_token(token));
			check_indent(token, indent+1);
			token++; // :

			if (!is_correct_indent(token, indent+2))
				errort(token, "Code missing for 'then' clause in match statement\n");

			token = parse_code(module, token, indent+2, &branch->code);
		}
		else {
			if (token->kind == TOKEN_ELSE)
				errort(token, "'else' clause not allowed in match statement\n");

			while (true) {
				if (!is_expression_starter(token->kind))
					errort(token, "Expected match expression, not: %\n", arg_token(token));

				Expression* expr = null;
				token = parse_expression(module, token, indent+2, true, &expr);

				group.exprs = realloc(group.exprs, sizeof(Expression*) * (group.expr_count), sizeof(Expression*) * (group.expr_count+1));
				group.exprs[group.expr_count++] = expr;

				if (token->kind != TOKEN_COMMA)
					break;

				check_indent(token, indent+1); // ,
				token++;
			}

			if (token->kind != TOKEN_COLON)
				errort(token, "Expected ':', not: %\n", arg_token(token));

			token = parse_code(module, token+1, indent+2, &group.code);

			realloc(branch->match.groups, branch->match.group_count+sizeof(MatchGroup), (branch->match.group_count+4)*sizeof(MatchGroup));
			branch->match.groups[branch->match.group_count++] = group;
		}
	}

	MatchGroup* then = null;

   	for (u64 i = 0; i < branch->match.group_count; i++) {
		MatchGroup* g = &branch->match.groups[i];

		g->clause_then = then;

		if (g->kind == CLAUSE_THEN)
			then = g;
	}

	return token;
}

static
Token* parse_branch(Module* module, Token* token, Indent16 indent, Branch* branch) {
	*branch = (Branch){ 0 };

	switch (token->kind) {
		case TOKEN_COLON: {
			branch->kind = BRANCH_NAKED;
			token = parse_code(module, token+1, indent+1, &branch->code);
		} break;

		case TOKEN_IF:    token = parse_if(module, token, indent, branch); break;
		case TOKEN_WHILE: token = parse_while(module, token, indent, branch); break;
		case TOKEN_FOR:   token = parse_for(module, token, indent, branch); break;
		case TOKEN_MATCH: token = parse_match(module, token, indent, branch); break;

		default: {
		} break;
	}

	return token;
}

static
Token* parse_controlflow(Module* module, Token* token, Indent16 indent, ControlFlow* out) {
	ClauseKind clause = CLAUSE_INIT;
	*out = (ControlFlow){ 0 };

	while (true) {
		out->branches = realloc(out->branches, sizeof(Branch)*(out->branch_count), sizeof(Branch)*(out->branch_count+1));
		token = parse_branch(module, token, indent, &out->branches[out->branch_count]);
		out->branch_count++;

		if (!is_correct_indent(token, indent))
			break;

		if (token->kind != TOKEN_ELSE && token->kind != TOKEN_THEN)
			break;

		if (token->kind == TOKEN_ELSE)
			clause = CLAUSE_ELSE;

		if (token->kind == TOKEN_THEN)
			clause = CLAUSE_THEN;

		token++; // else | then
	}

	Branch* belse = null;
	Branch* bthen = null;
	for (s64 i = out->branch_count-1; i >= 0; i--) {
		Branch* branch = &out->branches[i];

		branch->belse = belse;
		branch->bthen = bthen;

		if (branch->clause == CLAUSE_ELSE)
			belse = branch;

		if (branch->clause == CLAUSE_THEN)
			bthen = branch;
	}

	return token;
}

static
Token* parse_statement(Module* module, Token* token, Indent16 indent, Code* code) {
	Statement statement = { 0 };

	switch (token->kind) {
		case TOKEN_RETURN: {
			statement.kind = STATEMENT_RETURN;
			token++;

			if (is_expression_starter(token->kind) && is_correct_indent(token, indent+1))
				token = parse_expression(module, token, indent+1, true, &statement.ret.expr);
		} break;

		case TOKEN_BREAK:
			statement.kind = STATEMENT_BREAK;
			token++;
			break;

		case TOKEN_CONTINUE:
			statement.kind = STATEMENT_CONTINUE;
			token++;
			break;

		case TOKEN_INC:
		case TOKEN_DEC: {
			bool is_inc = token->kind == TOKEN_INC;
			statement.kind = is_inc ? STATEMENT_INC : STATEMENT_DEC;

			if (!is_expression_starter(token[1].kind) || !is_correct_indent(token+1, indent+1))
				errort(token, is_inc ? "Expected expression after inc\n" : "Expected expression after dec\n");

			token = parse_expression(module, token+1, indent+1, true, &statement.expr);
		} break;

		case TOKEN_IF:
		case TOKEN_FOR:
		case TOKEN_WHILE:
		case TOKEN_MATCH:
			statement.kind = STATEMENT_CONTROLFLOW;
			token = parse_controlflow(module, token, indent, &statement.controlflow);
			break;

		case TOKEN_IDENTIFIER_VARIABLE: {
			if (token[1].kind != TOKEN_COLON)
				goto GOTO_PARSE_EXPRESSION_STATEMENT;

			statement.kind = STATEMENT_VARDECL;
			Variable* var = stack_alloc(sizeof(Variable));
			zero(var, sizeof(Variable));
			statement.var = var;
			token = parse_variable_declaration(module, token, indent, var);
			scope_push_var(&code->scope, var);
		} break;

		GOTO_PARSE_EXPRESSION_STATEMENT:
		default: {
			if (!is_expression_starter(token->kind))
				errort(token, "Unexpected token at start of statement: %\n", arg_token(token));

			Expression* expr = null;
			token = parse_expression(module, token, indent+1, false, &expr);

			switch (token->kind) {
				default:
					statement.kind = STATEMENT_EXPRESSION;
					statement.expr = expr;
					break;

				case TOKEN_EQUAL:             statement.kind = STATEMENT_ASSIGNMENT;         break;
				case TOKEN_LEFT_SHIFT_EQUAL:  statement.kind = STATEMENT_ASSIGNMENT_LSH;     break;
				case TOKEN_RIGHT_SHIFT_EQUAL: statement.kind = STATEMENT_ASSIGNMENT_RSH;     break;
				case TOKEN_SLASH_EQUAL:       statement.kind = STATEMENT_ASSIGNMENT_DIV;     break;
				case TOKEN_PERCENT_EQUAL:     statement.kind = STATEMENT_ASSIGNMENT_MOD;     break;
				case TOKEN_PLUS_EQUAL:        statement.kind = STATEMENT_ASSIGNMENT_ADD;     break;
				case TOKEN_MINUS_EQUAL:       statement.kind = STATEMENT_ASSIGNMENT_SUB;     break;
				case TOKEN_ASTERISK_EQUAL:    statement.kind = STATEMENT_ASSIGNMENT_MUL;     break;
				case TOKEN_AMPERSAND_EQUAL:   statement.kind = STATEMENT_ASSIGNMENT_BIT_AND; break;
				case TOKEN_CARET_EQUAL:       statement.kind = STATEMENT_ASSIGNMENT_BIT_XOR; break;
				case TOKEN_PIKE_EQUAL:        statement.kind = STATEMENT_ASSIGNMENT_BIT_OR;  break;
			}

			if (statement.kind != STATEMENT_EXPRESSION) {
				statement.assign.left = expr;
				check_indent(token,   indent+1);
				check_indent(token+1, indent+1);
				token = parse_expression(module, token+1, indent+1, true, &statement.assign.right);
			}
		} break;
	}

	print("Parsed statement: %\n", arg_statement(&statement));

	if (!is_newline(token) && !is_scope_terminator(token->kind))
		errort(token, "Expected ';' after statement, not: %\n", arg_token(token));

	if (token->kind == TOKEN_SEMICOLON)
		token++;

	push_statement(code, statement);
	return token;
}

static
Token* parse_code(Module* module, Token* token, Indent16 indent, Code* code) {
	if (token->indent > indent)
		errort(token, "Invalid indent for statement\n");

	while (is_correct_indent(token, indent)) {
		token = parse_statement(module, token, indent, code);

		if (is_scope_terminator(token->kind))
			break;
	}

	return token;
}

static
Token* parse_params(Module* module, Function* func, Token* token, Indent16 indent) {
	Token* open = token;

	if (!open->closure)
		errort(open, "Function parameters missing closing ')'\n");

	check_indent(token, indent);
	token++; // (

	if (token->kind != TOKEN_CLOSE_PAREN) {
		func->param_count = open->comma_count+1;
		func->params = alloc(sizeof(Variable)*func->param_count);
		Variable* param = func->params;

		while (true) {
			if (token->kind == TOKEN_EOF)
				errort(token, "Missing ')'\n");

			token = parse_variable_declaration(module, token, indent+1, param);
			param++;

			if (token->kind == TOKEN_CLOSE_PAREN)
				break;

			if (token->kind != TOKEN_COMMA)
				errort(token, "Expected ')' or ',', not: %\n", arg_token(token));

			check_indent(token, indent);
			token++;
		}
	}

	check_indent(token, indent);
	token++; // )

	return token;
}

static
Token* parse_function(Module* module, Token* token, Indent16 indent) {
	Function func = { 0 };

	check_indent(token, indent);
	func.name = token++;

	token = parse_params(module, &func, token, indent);

	if (token->kind == TOKEN_ARROW) {
		check_indent(token, indent);
		token++; // ->

		token = parse_expression(module, token, indent+1, true, &func.return_type_expr);
	}

	if (token->kind != TOKEN_COLON)
		errort(token, "Function declaration expected ':', not: %\n", arg_token(token));
	check_indent(token, indent);
	token++; // :

	token = parse_code(module, token, indent+1, &func.code);

	// print("Parsed function: %\n", arg_token(func->name));
	module->functions[module->function_count++] = func;

	return token;
}

static
void parse_module(Module* module) {
	Token* token = module->tokens;

	// PreAllocate functions
	module->functions = alloc(sizeof(Function)*module->function_count);
	zero(module->functions, sizeof(Function)*module->function_count);
	module->function_count = 0;

	// PreAllocate structs
	module->structs = alloc(sizeof(Struct)*module->struct_count);
	zero(module->structs, sizeof(Struct)*module->struct_count);
	module->struct_count = 0;

	// PreAllocate enums
	module->enums = alloc(sizeof(Enum)*module->enum_count);
	zero(module->enums, sizeof(Enum)*module->enum_count);
	module->enum_count = 0;

	while (token->kind != TOKEN_EOF) {
		switch (token->kind) {
			case TOKEN_IDENTIFIER_VARIABLE:
			case TOKEN_IDENTIFIER_CONSTANT: {
				Variable* var = stack_alloc(sizeof(Variable));
				zero(var, sizeof(Variable));
				token = parse_variable_declaration(module, token, 0, var);
				scope_push_var(&module->scope, var);
			} break;

			case TOKEN_IDENTIFIER_FORMAL: {
				if (token[1].kind == TOKEN_OPEN_PAREN) {
					token = parse_function(module, token, 0);
					break;
				}

				if (token[1].kind == TOKEN_COLON) {
					errort(token, "Variables must start with a lowercase letter or all uppercase (constant).\n");
				}

				errort(token, "Function declaration expected '(' after name, not: %\n",
					arg_token(token)
				);
			} break;

			case TOKEN_STRUCT:
				token = parse_struct(module, token, 0);
				break;

			case TOKEN_ENUM:
				token = parse_enum(module, token, 0);
				break;

			default:
				errort(token, "Unexpected token when parsing global scope: %\n",
					arg_token(token)
				);
		}
	}
}

