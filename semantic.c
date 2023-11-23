#include "semantic.h"
#include "ast.h"

Module* current_module;

void scan_assignment(Statement* statement, Scope* scope) {
	Assignment* assign = &statement->assign;

	scan_expression(assign->left, scope);
	scan_expression(assign->right, scope);

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

void scan_match(Match* match, Scope* scope) {
}

void scan_branch(Branch* branch, Scope* scope) {
	if (branch->init) scan_expression(branch->init, scope);
	if (branch->cond) scan_expression(branch->cond, scope);
	if (branch->inc)  scan_expression(branch->inc, scope);
	scan_code(&branch->code);
}

void scan_controlflow(ControlFlow* controlflow, Scope* scope) {
	for (u64 i = 0; i < controlflow->branch_count; i++) {
		Branch* branch = &controlflow->branches[i];
		scan_branch(branch, scope);
	}
}

Function* find_function(char* name) {
	for (u64 i = 0; i < current_module->function_count; i++) {
		Function* func = &current_module->functions[i];

		if (func->name->aux.string.data != name)
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

		if (var->name->aux.string.data != name)
			continue;

		return var;
	}

	if (scope->parent_scope)
		return find_variable(scope->parent_scope, name);

	return null;
}

void scan_identifier(Expression* expr, Scope* scope) {
	Token* ident = expr->token;
	bool isconst = ident->kind != TOKEN_IDENTIFIER_VARIABLE;
	Variable* var = find_variable(scope, ident->aux.identifier.data);

	if (isconst)
		expr->flags |= EXPR_FLAG_CONSTANT;

	if (!var && isconst)
		error("Constant '%' not defined.\n", arg_token(ident));

	if (!var && !isconst)
		error("Variable '%' wasn't declared.\n", arg_token(ident));

	expr->var = var;
}

void scan_call(Expression* expr, Scope* scope) {
}

void scan_expression(Expression* expr, Scope* scope) {
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
			scan_identifier(expr, scope);
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
			scan_expression(expr->left,  scope);
			scan_expression(expr->right, scope);
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

		case EXPR_CALL: scan_call(expr, scope); break;
		case EXPR_INDEX:

		case EXPR_TERNARY_IF_ELSE:
			print("unhandled\n");
			break;
	}
}

void scan_vardecl(Variable* var, Scope* scope) {
	scan_expression(var->type_expr, scope);
	scan_expression(var->init_expr, scope);
}

void scan_statement(Statement* statement, Code* code) {
	switch (statement->kind) {
		case STATEMENT_ASSIGNMENT: {
			scan_assignment(statement, &code->scope);
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
			scan_assignment(statement, &code->scope);
		} break;

		case STATEMENT_EXPRESSION: {
			scan_expression(statement->expr, &code->scope);
		} break;

		case STATEMENT_CONTROLFLOW: {
			scan_controlflow(&statement->controlflow, &code->scope);
		} break;

		case STATEMENT_VARDECL: {
			scan_vardecl(statement->var, &code->scope);
		} break;

		case STATEMENT_RETURN: {
			scan_expression(statement->ret.expr, &code->scope);
		} break;

		case STATEMENT_BREAK: {
		} break;

		case STATEMENT_CONTINUE: {
		} break;

		case STATEMENT_INC:
		case STATEMENT_DEC: {
			scan_expression(statement->expr, &code->scope);
		} break;
	}
}

void scan_code(Code* code) {
	scan_scope(&code->scope);

	for (u64 i = 0; i < code->statement_count; i++) {
		Statement* statement = &code->statements[i];
		scan_statement(statement, code);
	}
}

void scan_scope(Scope* scope) {
	for (u64 i = 0; i < scope->variable_count; i++) {
		Variable* var = scope->variables[i];
	}
}

void scan_function(Function* func) {
	for (u32 i = 0; i < func->param_count; i++) {
		Variable* var = &func->params[i];
		scope_push_var(&func->code.scope, var);
	}

	scan_code(&func->code);
}

void scan_module(Module* module) {
	current_module = module;
	scan_scope(&module->scope);

	for (u64 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		scan_function(func);
	}
}

