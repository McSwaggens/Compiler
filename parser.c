#include "ast.h"
#include "alloc.h"

bool is_correct_indent(Token* token, Indent16 indent) {
	return !token->newline || token->indent == indent;
}

Expression* alloc_expression(Module* module) {
	return stack_alloc(sizeof(Expression));
}

void scope_push_var(Scope* scope, Variable* var) {
	scope->variables = realloc(scope->variables, scope->variable_count*sizeof(Variable*), next_pow2(scope->variable_count+7)*sizeof(Variable*));
	scope->variables[scope->variable_count++] = var;
}

void push_statement(Code* code, Statement statement) {
	code->statements = realloc(code->statements, code->statement_count*sizeof(Statement), next_pow2(code->statement_count+15)*sizeof(Statement));
	code->statements[code->statement_count++] = statement;
}

void push_function(Module* module, Function func) {
	module->functions = realloc(module->functions, module->function_count*sizeof(Function), next_pow2(module->function_count+31)*sizeof(Function));
	module->functions[module->function_count++] = func;
}

// @Note: Don't use 15, it's reserved for parsing a single expression.
u8 unary_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_PLUS:             return 13;
		case TOKEN_MINUS:            return 13;
		case TOKEN_ASTERISK:         return 13;
		case TOKEN_EXCLAMATION:      return 13;
		case TOKEN_TILDA:            return 13;
		case TOKEN_AT:               return 13;
		case TOKEN_PIKE:             return 13;
		case TOKEN_BACK_SLASH:       return 13;
		case TOKEN_LESS:             return 13;
		case TOKEN_OPEN_BRACKET:     return 13;
		default:                     return 0;
	}
}

u8 postfix_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_OPEN_BRACKET:     return 14;
		case TOKEN_OPEN_PAREN:       return 14;
		default:                     return 0;
	}
}

u8 binary_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_DOT:              return 14;

		case TOKEN_ARROW:            return 8;

		case TOKEN_ASTERISK:         return 7;
		case TOKEN_SLASH:            return 7;
		case TOKEN_BACK_SLASH:       return 7;
		case TOKEN_LEFT_SHIFT:       return 7;
		case TOKEN_RIGHT_SHIFT:      return 7;

		case TOKEN_PIKE:             return 6;
		case TOKEN_AMPERSAND:        return 6;
		case TOKEN_CARET:            return 6;
		case TOKEN_PLUS:             return 6;
		case TOKEN_MINUS:            return 6;

		case TOKEN_EQUAL:            return 5;
		case TOKEN_NOT_EQUAL:        return 5;
		case TOKEN_LESS:             return 5;
		case TOKEN_LESS_OR_EQUAL:    return 5;
		case TOKEN_GREATER:          return 5;
		case TOKEN_GREATER_OR_EQUAL: return 5;

		case TOKEN_AND:              return 2;
		case TOKEN_OR:               return 1;

		default:                     return 0;
	}
}

u8 ternary_precedence(TokenKind kind) {
	switch (kind) {
		case TOKEN_IF:               return 3;
		default:                     return 0;
	}
}

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

bool is_expression_starter(TokenKind kind) {
	return unary_precedence(kind) || is_term(kind);
}

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

void check_indent(Token* token, Indent16 indent) {
	if (!is_correct_indent(token, indent))
		error("Incorrect indentation\n");
}

u8 correct_unary_precedence(u8 precedence, Token* token) {
	if (!token->rspace)
		precedence <<= 4;

	return precedence;
}

u8 correct_binary_precedence(u8 precedence, Token* token) {
	if (!token->lspace && !token->rspace)
		precedence <<= 4;

	return precedence;
}

Token* parse_code(Module* module, Token* token, Indent16 indent, Code* code);
Token* parse_expression(Module* module, Token* token, Indent16 indent, bool allow_equals, Expression** out);

Token* parse_enum(Module* module, Token* token, Indent16 indent, Enum* out) {
	assert(token->kind == TOKEN_ENUM);
	*out = (Enum){ 0 };

	token++;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
		error("Expected formal name after 'enum', not: %\n", arg_token(token));
	check_indent(token, indent);

	out->name = token;
	token++;

	if (token->kind == TOKEN_ARROW) {
		check_indent(token, indent+1);
		token++;

		if (!is_expression_starter(token->kind))
			error("Expected type after '->', not: %\n", arg_token(token));

		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, true, &out->type);
	}

	if (token->kind != TOKEN_COLON)
		error("Expected ':', not: %\n", arg_token(token));
	check_indent(token, indent);
	token++;

	while (is_correct_indent(token, indent+1)) {
		EnumField field = { 0 };

		if (token->kind == TOKEN_IDENTIFIER_VARIABLE)
			error("Enum field with variable name is not allowed: %\n", arg_token(token));

		if (token->kind != TOKEN_IDENTIFIER_CONSTANT)
			error("Expected field name, not: %\n", arg_token(token));

		field.name = token;
		token++;

		if (token->kind == TOKEN_EQUAL && is_correct_indent(token, indent+2)) {
			check_indent(token, indent+2);
			token++;

			check_indent(token, indent+2);
			if (!is_expression_starter(token->kind))
				error("Expected expression after '=', not: %\n", arg_token(token));

			token = parse_expression(module, token, indent+2, true, &field.value);
		}

		out->fields = realloc(out->fields, sizeof(EnumField)*(out->field_count), sizeof(EnumField)*(out->field_count+1));
		out->fields[out->field_count++] = field;

		if (token->kind == TOKEN_SEMICOLON && is_correct_indent(token, indent+1)) {
			token++;
			continue;
		}

		if (!token->newline && token->kind == TOKEN_IDENTIFIER_FORMAL)
			error("Missing ';' before next field: %\n", arg_token(token));

		if (!token->newline)
			error("Unexpected token: %\n", arg_token(token));
	}

	return token;
}

Token* ParseStruct(Module* module, Token* token, Indent16 indent, Struct* out) {
	assert(token->kind == TOKEN_STRUCT);
	*out = (Struct){ 0 };

	token++;

	if (token->kind != TOKEN_IDENTIFIER_FORMAL)
		error("Expected formal name after 'struct', not: %\n", arg_token(token));
	check_indent(token, indent);

	out->name = token;
	token++;

	// print("Parsing struct: %\n", arg_token(out->name));

	if (token->kind != TOKEN_COLON)
		error("Expected ':', not: %\n", arg_token(token));
	check_indent(token, indent);
	token++;

	while (is_correct_indent(token, indent+1)) {
		StructField field = { 0 };

		if (token->kind == TOKEN_IDENTIFIER_CONSTANT || token->kind == TOKEN_IDENTIFIER_FORMAL)
			error("Struct field with constant name\n");

		if (token->kind != TOKEN_IDENTIFIER_VARIABLE)
			error("Expected field name, not: %\n", arg_token(token));

		field.name = token;
		token++;

		if (token->kind != TOKEN_COLON)
			error("Expected ':', not: %\n", arg_token(token));
		check_indent(token, indent+2);
		token++;

		if (!is_expression_starter(token->kind))
			error("Expected type after ':', not: %\n", arg_token(token));

		token = parse_expression(module, token, indent+2, true, &field.type);

		out->fields = realloc(out->fields, sizeof(StructField)*(out->field_count), sizeof(StructField)*(out->field_count+1));
		out->fields[out->field_count++] = field;

		if (token->kind == TOKEN_SEMICOLON && is_correct_indent(token, indent+1)) {
			token++;
			continue;
		}

		if (!token->newline && token->kind == TOKEN_IDENTIFIER_VARIABLE)
			error("Missing ';' before next field: %\n", arg_token(token));

		if (!token->newline)
			error("Unexpected token: %\n", arg_token(token));
	}

	return token;
}

typedef struct ExpressionParseHelper {
	Indent16 indent;
	u64 bracket_level;
	bool locked;
	s8 adjustment;
} ExpressionParseHelper;

bool test_expression_indent(Token* token, ExpressionParseHelper* helper, s32 adjustment, bool error_if_invalid) {
	if (!token->newline)
		return true;

	bool was_locked = helper->locked;
	Indent16 indent = helper->indent + helper->bracket_level - helper->adjustment + adjustment;
	helper->locked = true;

	// print("token = %, lvl = %, ti = %, ci = %\n",
	// 	arg_token(token),
	// 	arg_u64(helper->bracket_level),
	// 	arg_u16(token->indent),
	// 	arg_u16(indent)
	// );

	if (token->indent == indent)
		return true;

	if (was_locked || !helper->bracket_level || token->indent > indent || token->indent < indent-1) {
		if (!error_if_invalid)
			return false;

		error("Invalid indent\n");
	}

	helper->adjustment = 1;
	return true;
}

Token* internal_parse_expression(Module* module, Token* token, bool allow_equals, u8 parent_precedence, ExpressionParseHelper* helper, Expression** out) {
	// Make sure to call test_expression_indent before you call internal_parse_expression
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

		case TOKEN_LITERAL_INT8:   case TOKEN_LITERAL_INT16:
		case TOKEN_LITERAL_INT32:  case TOKEN_LITERAL_INT64:
		case TOKEN_LITERAL_UINT8:  case TOKEN_LITERAL_UINT16:
		case TOKEN_LITERAL_UINT32: case TOKEN_LITERAL_UINT64:
			kind = EXPR_LITERAL;
			goto GOTO_TERM;

		case TOKEN_LITERAL_FLOAT32:     kind = EXPR_LITERAL; goto GOTO_TERM;
		case TOKEN_LITERAL_FLOAT64:     kind = EXPR_LITERAL; goto GOTO_TERM;
		case TOKEN_LITERAL_STRING:      kind = EXPR_LITERAL; goto GOTO_TERM;
		GOTO_TERM:
			left = alloc_expression(module);
			left->kind = kind;
			left->token = token;
			token++;
			break;

		case TOKEN_BYTE:   case TOKEN_BOOL:   case TOKEN_INT:     case TOKEN_INT8:    case TOKEN_INT16:
		case TOKEN_INT32:  case TOKEN_INT64:  case TOKEN_UINT:    case TOKEN_UINT8:   case TOKEN_UINT16:
		case TOKEN_UINT32: case TOKEN_UINT64: case TOKEN_FLOAT32: case TOKEN_FLOAT64: case TOKEN_TYPE_ID:
			left = alloc_expression(module);
			left->kind = EXPR_BASETYPE_PRIMITIVE;
			left->token = token;
			token++;
			break;

		case TOKEN_ASTERISK:    kind = EXPR_UNARY_PTR;     goto GOTO_UNARY;
		case TOKEN_AT:          kind = EXPR_UNARY_REF;     goto GOTO_UNARY;
		case TOKEN_EXCLAMATION: kind = EXPR_UNARY_NOT;     goto GOTO_UNARY;
		case TOKEN_TILDA:       kind = EXPR_UNARY_BIT_NOT; goto GOTO_UNARY;
		case TOKEN_MINUS:       kind = EXPR_UNARY_INVERSE; goto GOTO_UNARY;
		case TOKEN_PLUS:        kind = EXPR_UNARY_ABS;     goto GOTO_UNARY;
		GOTO_UNARY:
			left = alloc_expression(module);
			left->kind = kind;
			left->token = token;
			test_expression_indent(token+1, helper, 0, true);
			token = internal_parse_expression(module, token+1, allow_equals, unaryprec, helper, &left->right);
			break;

		case TOKEN_OPEN_BRACE: {
			left = alloc_expression(module);
			left->kind = EXPR_ARRAY;
			left->token = token;
			test_expression_indent(token, helper, 0, true);
			token++;

			Expression** elems = null;
			u64 count = 0;

			helper->bracket_level++;

			if (token->kind != TOKEN_CLOSE_BRACE)
			while (true) {
				if (token->kind == TOKEN_COMMA)
					error("Missing expression before ','\n");

				Expression* elem = null;
				test_expression_indent(token, helper, 0, true);
				token = internal_parse_expression(module, token, true, 0, helper, &elem);

				elems = realloc(elems, sizeof(Expression*) * (count), sizeof(Expression*) * (count+1));
				elems[count++] = elem;

				if (token->kind == TOKEN_CLOSE_BRACE)
					break;

				if (token->kind != TOKEN_COMMA)
					error("Unexpected token %, expected ',' or '}'\n",
						arg_token(token)
					);

				test_expression_indent(token, helper, -1, true);
				token++;
			}

			helper->bracket_level--;
			test_expression_indent(token, helper, 0, true);
			token++;

			left->elems = elems;
			left->elem_count = count;

		} break;

		case TOKEN_OPEN_PAREN: {
			left = alloc_expression(module);
			left->kind = EXPR_TUPLE;
			left->token = token;
			test_expression_indent(token, helper, 0, true);
			token++;

			// print("Starting tuple parse...\n");

			Expression** elems = null;
			u64 count = 0;

			helper->bracket_level++;

			if (token->kind != TOKEN_CLOSE_PAREN)
			while (true) {
				if (token->kind == TOKEN_COMMA)
					error("Missing expression before ','\n");

				Expression* elem = null;
				test_expression_indent(token, helper, 0, true);
				token = internal_parse_expression(module, token, true, 0, helper, &elem);

				elems = realloc(elems, sizeof(Expression*) * (count), sizeof(Expression*) * (count+1));
				// print("elems = %\n", arg_u64((u64)elems));
				elems[count++] = elem;

				if (token->kind == TOKEN_CLOSE_PAREN)
					break;

				if (token->kind != TOKEN_COMMA)
					error("Unexpected token %, expected ',' or ')'\n",
						arg_token(token)
					);

				test_expression_indent(token, helper, -1, true);
				token++;

				if (token->kind == TOKEN_CLOSE_PAREN)
					error("Missing expression after ','\n");
			}

			helper->bracket_level--;
			test_expression_indent(token, helper, 0, true);
			token++;

			left->elems = elems;
			left->elem_count = count;
			// print("Tuple parse finished.\n");

		} break;

		case TOKEN_OPEN_BRACKET: {
			left = alloc_expression(module);
			left->kind = EXPR_UNARY_SPEC_FIXED;
			left->token = token;

			helper->bracket_level++;

			if (token[1].kind == TOKEN_CLOSE_BRACKET) {
				left->kind = EXPR_UNARY_SPEC_ARRAY; // []e

				// Take subexpression
				test_expression_indent(token+2, helper, 0, true); // e
				token = internal_parse_expression(module, token+2, allow_equals, unaryprec, helper, &left->right);
				break;
			}

			left->kind = EXPR_UNARY_SPEC_FIXED; // [N]e
			test_expression_indent(token+1, helper, 0, true); // e
			token = internal_parse_expression(module, token+1, allow_equals, 0, helper, &left->left);

			if (token->kind == TOKEN_DOT_DOT) {
				test_expression_indent(token, helper, -1, true); // ..
				left->kind = EXPR_BINARY_SPAN; // [a..b]
				test_expression_indent(token+1, helper, 0, true); // b
				token = internal_parse_expression(module, token+1, allow_equals, 0, helper, &left->right);
			}

			if (token->kind != TOKEN_CLOSE_BRACKET)
				error("Expected ']', not: %\n", arg_token(token));

			helper->bracket_level--;
			test_expression_indent(token, helper, 0, true); // ]
			token++;

			if (left->kind == EXPR_UNARY_SPEC_FIXED) { // [N]e
				// Take subexpression
				test_expression_indent(token, helper, 0, true); // e
				token = internal_parse_expression(module, token, allow_equals, unaryprec, helper, &left->right);
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

		if (token->newline && !helper->bracket_level && token->indent <= helper->indent-1)
			break;

		test_expression_indent(token, helper, 0, true);

		precedence = correct_binary_precedence(precedence, token);

		if (precedence <= parent_precedence)
			break;

		Expression* expr = alloc_expression(module);
		expr->left = left;
		left = expr;

		expr->token = token;

		switch (token->kind) {
			default:
				error("parse_expression didn't handle binary operator %\n", arg_token(token));

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
			GOTO_BINARY_OP:
				test_expression_indent(token+1, helper, 0, true);
				token = internal_parse_expression(module, token+1, allow_equals, precedence, helper, &expr->right);
				break;

			case TOKEN_OPEN_BRACKET:
				expr->kind = EXPR_INDEX;
				expr->token = token;
				token++;

				helper->bracket_level++;
				test_expression_indent(token, helper, 0, true);
				token = internal_parse_expression(module, token, allow_equals, precedence, helper, &expr->right);
				helper->bracket_level--;
				test_expression_indent(token, helper, 0, true);
				token++;

				break;

			case TOKEN_OPEN_PAREN:
				expr->kind = EXPR_CALL;
				expr->token = token;
				token++;

				helper->bracket_level++;

				if (token->kind != TOKEN_CLOSE_PAREN)
				while (true) {
					expr->elems = realloc(expr->elems, sizeof(Expression*)*expr->elem_count, sizeof(Expression*)*expr->elem_count+1);
					test_expression_indent(token, helper, 0, true);
					token = internal_parse_expression(module, token, allow_equals, precedence, helper, &expr->elems[expr->elem_count]);
					expr->elem_count++;

					if (token->kind == TOKEN_COMMA) {
						test_expression_indent(token, helper, -1, true);
						token++;
						continue;
					}

					if (token->kind == TOKEN_CLOSE_PAREN)
						break;

					error("Unexpected token in function parameters: %\n", arg_token(token));
				}

				helper->bracket_level--;
				test_expression_indent(token, helper, 0, true);
				token++;

				break;

			case TOKEN_IF:
				expr->kind = EXPR_TERNARY_IF_ELSE;
				test_expression_indent(token+1, helper, 0, true);
				token = internal_parse_expression(module, token+1, true, precedence, helper, &expr->middle);

				if (token->kind != TOKEN_ELSE)
					error("Expected 'else' not: %\n", arg_token(token));

				test_expression_indent(token+1, helper, 0, true);
				token = internal_parse_expression(module, token+1, allow_equals, precedence, helper, &expr->right);
				break;
		}

		expr->begin = begin;
		expr->end = token;
	}

	*out = left;
	return token;
}

Token* parse_expression(Module* module, Token* token, Indent16 indent, bool allow_equals, Expression** out) {
	assert(is_expression_starter(token->kind));

	ExpressionParseHelper helper = {
		.locked = false,
		.adjustment = false,
		.indent = indent,
		.bracket_level = 0,
	};

	token = internal_parse_expression(module, token, allow_equals, 0, &helper, out);
	// print("Parsed expression: %\n", arg_expression(*out));

	return token;
}

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
		error("Variable declaration expected ':', not: %\n", arg_token(token));
	check_indent(token, indent+1);
	token++;

	if (is_expression_starter(token->kind)) {
		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, false, &out->type_expr);
	}

	if (!out->type_expr && token->kind != TOKEN_EQUAL)
		error("Invalid variable declaration, expected type or '=', not: %\n", arg_token(token));

	if (token->kind == TOKEN_EQUAL) {
		check_indent(token, indent+1);
		token++;

		if (!is_expression_starter(token->kind))
			error("Expected expression after '='\n", arg_token(token));

		check_indent(token, indent+1);
		token = parse_expression(module, token, indent+1, false, &out->init_expr);
	}

	if (is_const && !out->init_expr)
		error("Constant declared without a value.\n");

	return token;
}

Token* parse_match(Module* module, Token* token, Indent16 indent, Match* out) {
	while (is_correct_indent(token, indent)) {
		MatchGroup group = { 0 };

		if (token->kind == TOKEN_THEN) {
			if (token[1].kind != TOKEN_COLON)
				error("Expected ':' after 'then', not: %\n", arg_token(token+1));

			check_indent(token,   indent);
			check_indent(token+1, indent);

			if (!is_correct_indent(token, indent))
				error("Code missing for 'then' clause in match statement\n");
		}
		else {
			if (token->kind == TOKEN_ELSE)
				error("'else' clause not allowed in match statement\n");

			while (true) {
				if (!is_expression_starter(token->kind))
					error("Expected expression, not: %\n", arg_token(token));

				Expression* expr = null;
				token = parse_expression(module, token, indent+1, true, &expr);

				group.exprs = realloc(group.exprs, sizeof(Expression*) * (group.expr_count), sizeof(Expression*) * (group.expr_count+1));
				group.exprs[group.expr_count++] = expr;

				if (token->kind != TOKEN_COMMA)
					break;

				check_indent(token, indent);
				token++;
			}

			if (token->kind != TOKEN_COLON)
				error("Expected ':', not: %\n", arg_token(token));

			check_indent(token, indent);

			token = parse_code(module, token+1, indent+1, &group.code);

			realloc(out->groups, (out->group_count)+sizeof(MatchGroup), (out->group_count+4)*sizeof(MatchGroup));
			out->groups[out->group_count++] = group;
		}
	}

	MatchGroup* then = null;

	for (u64 i = 0; i < out->group_count; i++) {
		MatchGroup* g = out->groups+i;

		g->clause_then = then;

		if (g->kind == CLAUSE_THEN)
			then = g;
	}

	return token;
}

Token* parse_branch(Module* module, Token* token, Indent16 indent, Branch* branch) {
	// print("Parsing new branch... token = %\n", arg_token(token));

	*branch = (Branch){ 0 };

	switch (token->kind) {
		case TOKEN_COLON: {
			branch->kind = BRANCH_NAKED;
			check_indent(token, indent+1);
			token = parse_code(module, token+1, indent+1, &branch->code);
		} break;

		case TOKEN_IF: {
			branch->kind = BRANCH_IF;
			check_indent(token+1, indent+1);
			token = parse_expression(module, token+1, indent+1, true, &branch->cond);

			if (token->kind != TOKEN_COLON)
				error("Expected ':', not: %\n", arg_token(token));
			check_indent(token, indent+1);

			token = parse_code(module, token+1, indent+1, &branch->code);
		} break;

		case TOKEN_WHILE: {
			branch->kind = BRANCH_WHILE;
			check_indent(token+1, indent+1);
			token = parse_expression(module, token+1, indent+1, true, &branch->cond);

			if (token->kind != TOKEN_COLON)
				error("Expected ':', not: %\n", arg_token(token));
			check_indent(token, indent+1);

			token = parse_code(module, token+1, indent+1, &branch->code);
		} break;

		case TOKEN_FOR: {
			branch->kind = BRANCH_FOR;

			if (!is_expression_starter(token[1].kind))
				error("Expected expression not: %\n", arg_token(token+1));

			check_indent(token+1, indent+1);
			token = parse_expression(module, token+1, indent+1, true, &branch->init);

			if (token->kind != TOKEN_COMMA)
				error("Expected ',' after initial expression, not: %\n", arg_token(token));
			check_indent(token, indent+1);

			if (!is_expression_starter(token[1].kind))
				error("Expected condition expression not: %\n", arg_token(token+1));

			check_indent(token+1, indent+1);
			token = parse_expression(module, token+1, indent+1, true, &branch->cond);

			if (token->kind == TOKEN_COMMA) {
				check_indent(token+1, indent+1);
				token = parse_expression(module, token+1, indent+1, true, &branch->inc);
			}

			if (token->kind != TOKEN_COLON)
				error("Expected ':', not: %\n", arg_token(token));
			check_indent(token, indent+1);

			token = parse_code(module, token+1, indent+1, &branch->code);
		} break;

		case TOKEN_MATCH: {
			branch->kind = BRANCH_MATCH;

			if (!is_expression_starter(token[1].kind))
				error("Expected expression not: %\n", arg_token(token+1));

			token = parse_expression(module, token+1, indent+1, true, &branch->init);

			if (token->kind != TOKEN_COLON)
				error("Expected ':', not: %\n", arg_token(token));
			check_indent(token, indent+1);

			token = parse_match(module, token, indent+1, &branch->match);
		} break;

		default: {
		} break;
	}

	return token;
}

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

		token++;
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
				error(is_inc ? "Expected expression after inc\n" : "Expected expression after dec\n");

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
				error("Unexpected token at start of statement: %\n", arg_token(token));

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

	if (!token->newline && !is_scope_terminator(token->kind))
		error("Expected ';' after statement, not: %\n", arg_token(token));

	if (token->kind == TOKEN_SEMICOLON)
		token++;

	push_statement(code, statement);
	return token;
}

Token* parse_code(Module* module, Token* token, Indent16 indent, Code* code) {
	while (is_correct_indent(token, indent)) {
		token = parse_statement(module, token, indent, code);

		if (is_scope_terminator(token->kind))
			break;
	}

	return token;
}

Token* parse_params(Module* module, Function* func, Token* token, Indent16 indent) {
	check_indent(token, indent);
	token++;

	if (token->kind != TOKEN_CLOSE_PAREN)
	while (true) {
		Variable var = { 0 };

		if (token->kind == TOKEN_EOF)
			error("Missing ')'\n");

		token = parse_variable_declaration(module, token, indent+1, &var);
		func->params = realloc(func->params, func->param_count*sizeof(Variable), (func->param_count+1)*sizeof(Variable));
		func->params[func->param_count++] = var;

		if (token->kind == TOKEN_CLOSE_PAREN)
			break;

		if (token->kind != TOKEN_COMMA)
			error("Expected ')' or ',', not: %\n", arg_token(token));

		check_indent(token, indent);
		token++;
	}

	check_indent(token, indent);
	token++;

	return token;
}

Token* parse_function(Module* module, Token* token, Indent16 indent) {
	Function func = { 0 };

	check_indent(token, indent);
	func.name = token++;

	token = parse_params(module, &func, token, indent);

	if (token->kind == TOKEN_ARROW) {
		check_indent(token, indent);
		token = parse_expression(module, token+1, indent+1, true, &func.return_type_expr);
	}

	if (token->kind != TOKEN_COLON)
		error("Function declaration expected ':', not: %\n", arg_token(token));
	check_indent(token, indent);

	token = parse_code(module, token+1, indent+1, &func.code);

	// print("Parsed function: %\n", arg_token(func->name));
	push_function(module, func);

	return token;
}

Module* parse_module(TokenStore ts) {
	if (!ts.count)
		return null;

	Module* module = stack_alloc(sizeof(Module));
	zero(module, sizeof(Module));

	Token* token = ts.tokens;

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
					error("Variables must start with a lowercase letter or all uppercase (constant).\n");
				}

				error("Function declaration expected '(' after name, not: %\n",
					arg_token(token)
				);
			} break;

			case TOKEN_STRUCT:
				module->structs = realloc(module->structs, sizeof(Struct)*(module->struct_count), sizeof(Struct)*(module->struct_count+1));
				token = ParseStruct(module, token, 0, &module->structs[module->struct_count]);
				module->struct_count++;
				break;

			case TOKEN_ENUM:
				module->enums = realloc(module->enums, sizeof(Enum)*(module->enum_count), sizeof(Enum)*(module->enum_count+1));
				token = parse_enum(module, token, 0, &module->enums[module->enum_count]);
				module->enum_count++;
				break;

			default:
				error("Unexpected token when parsing global scope: %\n",
					arg_token(token)
				);
		}
	}

	return module;
}

