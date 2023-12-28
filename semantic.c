#include "semantic.h"
#include "ast.h"

static void complete(Ast* ast) {
}

static void scan_ast(Ast* ast, Context context) {
	Expression* expr = (Expression*)ast;

	switch (ast->kind) {
		case AST_INVALID:

		case AST_FUNCTION:
		case AST_VARIABLE:

		case AST_STRUCT:
		case AST_STRUCT_MEMBER:

		case AST_ENUM:
		case AST_ENUM_MEMBER:

		case AST_BRANCH:
		case AST_CONTROLFLOW:

		case AST_EXPR_NULL:
		case AST_EXPR_TRUE:
		case AST_EXPR_FALSE:
		case AST_EXPR_LITERAL:
			return;

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
			return;

		case AST_EXPR_UNARY_PTR:
		case AST_EXPR_UNARY_REF:
		case AST_EXPR_UNARY_ABS:
		case AST_EXPR_UNARY_INVERSE:
		case AST_EXPR_UNARY_NOT:
		case AST_EXPR_UNARY_BIT_NOT:
		case AST_EXPR_UNARY_IMPLICIT_CAST:
			return;

		case AST_EXPR_BINARY_ADD: {
			if (expr->flags & AST_FLAG_COMPLETE) {
				return;
			}

			Expression* left  = expr->binary.left;
			Expression* right = expr->binary.right;

			// if (!known(left->type_value)) {
			// 	error("left type is unknown.\n");
			// 	return;
			// }

			// TypeID ltid = resolve(left->type);
			// TypeID rtid = resolve(right->type);

			// if (!ltid || !rtid) {
			// 	return;
			// }

			// expr->flags |= (left->flags & right->flags) & (AST_FLAG_CONSTANT);

			// // ptr + int
			// if (is_ptr(ltid) && is_int(rtid)) {
			// 	expr->type = ltid;
			// 	complete(ast);
			// 	return;
			// }

			// if (is_int(ltid) && is_int(rtid)) {
			// 	expr->type = ltid;
			// 	complete(ast);
			// }

		} return;

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
			return;

		case AST_EXPR_CALL:
		case AST_EXPR_INDEX:
		case AST_EXPR_TERNARY_IF_ELSE:

		case AST_STATEMENT_ASSIGNMENT: {
		} return;

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
			return;

		case AST_STATEMENT_EXPRESSION:
		case AST_STATEMENT_CONTROLFLOW:
		case AST_STATEMENT_VARDECL:
		case AST_STATEMENT_RETURN:
		case AST_STATEMENT_BREAK:
		case AST_STATEMENT_CONTINUE:
		case AST_STATEMENT_INC:
		case AST_STATEMENT_DEC:
			return;
	}
}

static void scan_expression(ScanHelper* helper,  Expression* expr, Scope* scope);
static void scan_statement(ScanHelper* helper,   Statement* statement, Code* code);
static void scan_assignment(ScanHelper* helper,  Statement* statement, Scope* scope);
static void scan_controlflow(ScanHelper* helper, ControlFlow* controlflow, Scope* scope);
static void scan_code(ScanHelper* helper,        Code* code);
static void scan_function(ScanHelper* helper,    Function* func) {
}

static void scan_module(Module* module) {
	ScanHelper helper = (ScanHelper){
	};

	for (u32 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		print("Scanning function '%'\n", arg_token(func->name));
		scan_function(&helper, func);
	}
}

