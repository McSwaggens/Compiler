#include "semantic.h"
#include "ast.h"

static void scan_expression(ScanHelper* helper,  Expression* expr, Scope* scope);
static void scan_assignment(ScanHelper* helper, Assignment* assignment, Code* code);
static void scan_controlflow(ScanHelper* helper, ControlFlow* controlflow, Scope* scope);
static void scan_statement(ScanHelper* helper,   Statement* statement, Code* code);
static void scan_code(ScanHelper* helper,        Code* code);
static void scan_function(ScanHelper* helper,    Function* func);

static void scan_expression(ScanHelper* helper, Expression* expr, Scope* scope) {
}

static void scan_variable(ScanHelper* helper, Variable* var, Scope* scope) {
}

static void scan_branch(ScanHelper* helper, Branch* branch, Scope* scope) {
	bool is_branch = false;

	switch (branch->branch_kind) {
		case BRANCH_NAKED: {
		} break;

		case BRANCH_IF: {
			scan_expression(helper, branch->cond, scope); // @Todo: Convert to bool
		} break;

		case BRANCH_FOR: {
			is_branch = true;
			scan_variable(helper, branch->var, scope);
			scan_expression(helper, branch->cond, scope); // @Todo: Convert to bool
			scan_expression(helper, branch->nextval, scope); // @Todo: Check if nextval type can be converted to branch->var->type
		} break;

		case BRANCH_WHILE: {
			is_branch = true;
			scan_expression(helper, branch->cond, scope); // @Todo: Convert to bool
		} break;

		case BRANCH_MATCH: {
			is_branch = true;
			scan_expression(helper, branch->cond, scope);
		} break;
	}

	Branch* last_loop = helper->loop;

	if (is_branch) {
		helper->loop = branch;
	}

	scan_code(helper, &branch->code);

	helper->loop = last_loop;
}

static void scan_controlflow(ScanHelper* helper, ControlFlow* controlflow, Scope* scope) {
	for (u32 i = 0; i < controlflow->branch_count; i++) {
		Branch* branch = &controlflow->branches[i];
		scan_branch(helper, branch, scope);
	}
}

static void scan_assignment(ScanHelper* helper, Assignment* assignment, Code* code) {
	scan_expression(helper, assignment->left, &code->scope);
	scan_expression(helper, assignment->right, &code->scope);
}

static void scan_statement(ScanHelper* helper, Statement* statement, Code* code) {
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
			scan_assignment(helper, &statement->assign, code);
			break;

		case STATEMENT_EXPRESSION:
			scan_expression(helper, statement->expr, &code->scope);
			break;

		case STATEMENT_CONTROLFLOW:
			scan_controlflow(helper, &statement->controlflow, &code->scope);
			break;

		case STATEMENT_VARDECL:
		case STATEMENT_RETURN:
		case STATEMENT_BREAK:
		case STATEMENT_CONTINUE:
		case STATEMENT_INC:
		case STATEMENT_DEC: {
		} break;
	}
}

static void scan_code(ScanHelper* helper, Code* code) {
	for (u32 i = 0; i < code->statement_count; i++) {
		Statement* statement = &code->statements[i];
		scan_statement(helper, statement, code);
	}
}

static void scan_function(ScanHelper* helper, Function* func) {
	scan_code(helper, &func->code);
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

