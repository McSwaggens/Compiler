#include "semantic.h"
#include "ast.h"

void scan_assignment(Module* module, Statement* statement, Scope* scope) {
	Assignment* assign = &statement->assign;

	scan_expression(module, assign->left, scope);
	scan_expression(module, assign->right, scope);

	switch (statement->kind) {
		default: assert_unreachable();
		case STATEMENT_ASSIGNMENT:
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
			break;
	}
}

void scan_match(Module* module, Match* match, Scope* scope) {
}

void scan_branch(Module* module, Branch* branch, Scope* scope) {
	if (branch->init) scan_expression(module, branch->init, scope);
	if (branch->cond) scan_expression(module, branch->cond, scope);
	if (branch->inc)  scan_expression(module, branch->inc, scope);
	scan_code(module, &branch->code);
}

void scan_controlflow(Module* module, ControlFlow* controlflow, Scope* scope) {
	for (u64 i = 0; i < controlflow->branch_count; i++) {
		Branch* branch = &controlflow->branches[i];
		scan_branch(module, branch, scope);
	}
}

Function* find_function(Module* module, char* name) {
	for (u64 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];

		if (func->name->string.data != name)
			continue;

		return func;
	}

	return null;
}

Variable* find_variable(Scope* scope, char* name) {
	for (u64 i = 0; i < scope->variable_count; i++) {
		Variable* var = scope->variables[i];

		// if (!(var->flags & VAR_SCANNED))
		// 	break;

		if (var->name->string.data != name)
			continue;

		return var;
	}

	if (scope->parent_scope)
		return find_variable(scope->parent_scope, name);

	return null;
}

void scan_identifier(Module* module, Expression* expr, Scope* scope) {
	Token* ident = expr->token;
	bool isconst = ident->kind != TOKEN_IDENTIFIER_VARIABLE;
	Variable* var = find_variable(scope, ident->identifier.data);

	if (isconst)
		expr->flags |= EXPR_FLAG_CONSTANT;

	if (!var && isconst)
		errort(expr->begin, "Constant '%' not defined.\n", arg_token(ident));

	if (!var && !isconst)
		errort(expr->begin, "Variable '%' wasn't declared.\n", arg_token(ident));

	expr->var = var;
}

void scan_call(Module* module, Expression* expr, Scope* scope) {
	Expression* func_expr = expr->left;

	if (func_expr->token->kind != TOKEN_IDENTIFIER_FORMAL) {
		scan_expression(module, func_expr, scope);
	}
	else {
		Token* name_token = func_expr->token;
		String name = name_token->string;

		Function* func = find_function(module, name.data);

		if (!func) {
			errort(name_token, "No such function called %\n", arg_string(name));
		}

		func_expr->kind = EXPR_FUNCTION;
		func_expr->func = func;
	}
}

void scan_expression(Module* module, Expression* expr, Scope* scope) {
	switch (expr->kind) {
		case EXPR_NULL:
		case EXPR_TRUE:
		case EXPR_FALSE: {
			expr->flags |= EXPR_FLAG_CONSTANT;
		} break;

		case EXPR_LITERAL: {
			expr->flags |= VAR_CONSTANT;
		} return;

		case EXPR_FUNCTION: {
			print("unhandled\n");
		} break;

		case EXPR_IDENTIFIER_CONSTANT:
		case EXPR_IDENTIFIER_VARIABLE: {
			scan_identifier(module, expr, scope);
		} break;

		case EXPR_IDENTIFIER_FORMAL: {
		} break;

		case EXPR_BASETYPE_PRIMITIVE: {
			expr->type = TYPE_TYPEID;
		} break;

		case EXPR_BASETYPE_IDENTIFIER: {
		} break;

		case EXPR_ARRAY:
		case EXPR_TUPLE:

		case EXPR_UNARY_ABS:
		case EXPR_UNARY_INVERSE:
		case EXPR_UNARY_NOT:
		case EXPR_UNARY_BIT_NOT:
		case EXPR_UNARY_PTR:
		case EXPR_UNARY_REF:
		case EXPR_UNARY_SPEC_PTR:
		case EXPR_UNARY_SPEC_ARRAY:
		case EXPR_UNARY_SPEC_FIXED:
			print("unhandled\n");
			break;

		case EXPR_BINARY_ADD: {
		case EXPR_BINARY_SUB:
		case EXPR_BINARY_MUL:
		case EXPR_BINARY_DIV:
		case EXPR_BINARY_MOD:
		case EXPR_BINARY_BIT_XOR:
		case EXPR_BINARY_BIT_AND:
		case EXPR_BINARY_BIT_OR:
		case EXPR_BINARY_LSHIFT:
		case EXPR_BINARY_RSHIFT:
			scan_expression(module, expr->left,  scope);
			scan_expression(module, expr->right, scope);
		} break;

		case EXPR_BINARY_OR:
		case EXPR_BINARY_AND:

		case EXPR_BINARY_EQUAL:
		case EXPR_BINARY_NOT_EQUAL:
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_LESS_OR_EQUAL:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_GREATER_OR_EQUAL:

		case EXPR_BINARY_DOT:

		case EXPR_BINARY_DOT_DOT:

		case EXPR_BINARY_SPAN:
			print("unhandled\n");
			break;

		case EXPR_CALL: scan_call(module, expr, scope); break;
		case EXPR_INDEX:

		case EXPR_TERNARY_IF_ELSE:
			print("unhandled\n");
			break;
	}
}

void scan_vardecl(Module* module, Variable* var, Scope* scope) {
	assert(var->type_expr || var->init_expr);

	if (var->type_expr)
		scan_expression(module, var->type_expr, scope);

	if (var->init_expr)
		scan_expression(module, var->init_expr, scope);
}

void scan_statement(Module* module, Statement* statement, Code* code) {
	switch (statement->kind) {
		case STATEMENT_ASSIGNMENT: {
			scan_assignment(module, statement, &code->scope);
		} break;

		case STATEMENT_ASSIGNMENT_LSH:
		case STATEMENT_ASSIGNMENT_RSH:
		case STATEMENT_ASSIGNMENT_DIV:
		case STATEMENT_ASSIGNMENT_MOD:
		case STATEMENT_ASSIGNMENT_ADD:
		case STATEMENT_ASSIGNMENT_SUB:
		case STATEMENT_ASSIGNMENT_MUL:
		case STATEMENT_ASSIGNMENT_BIT_XOR:
		case STATEMENT_ASSIGNMENT_BIT_AND:
		case STATEMENT_ASSIGNMENT_BIT_OR: {
			scan_assignment(module, statement, &code->scope);
		} break;

		case STATEMENT_EXPRESSION: {
			scan_expression(module, statement->expr, &code->scope);
		} break;

		case STATEMENT_CONTROLFLOW: {
			scan_controlflow(module, &statement->controlflow, &code->scope);
		} break;

		case STATEMENT_VARDECL: {
			scan_vardecl(module, statement->var, &code->scope);
		} break;

		case STATEMENT_RETURN: {
			if (statement->ret.expr) {
				scan_expression(module, statement->ret.expr, &code->scope);
			}
		} break;

		case STATEMENT_BREAK: {
		} break;

		case STATEMENT_CONTINUE: {
		} break;

		case STATEMENT_INC:
		case STATEMENT_DEC: {
			scan_expression(module, statement->expr, &code->scope);
		} break;
	}
}

void scan_code(Module* module, Code* code) {
	scan_scope(module, &code->scope);

	for (u64 i = 0; i < code->statement_count; i++) {
		Statement* statement = &code->statements[i];
		scan_statement(module, statement, code);
	}
}

void scan_scope(Module* module, Scope* scope) {
	for (u64 i = 0; i < scope->variable_count; i++) {
		// Variable* var = scope->variables[i];
	}
}

void scan_function(Module* module, Function* func) {
	for (u32 i = 0; i < func->param_count; i++) {
		Variable* var = &func->params[i];
		scope_push_var(&func->code.scope, var);
	}

	scan_code(module, &func->code);
}

void scan_module(Module* module) {
	module = module;
	scan_scope(module, &module->scope);

	for (u64 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		scan_function(module, func);
	}
}

