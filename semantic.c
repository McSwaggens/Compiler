#include "semantic.h"
#include "ast.h"

static void scan_expression(ScanHelper* helper, Expression* expr, Scope* scope) {
}

static void scan_assignment(ScanHelper* helper, Statement* statement, Scope* scope) {
}

static void scan_controlflow(ScanHelper* helper, ControlFlow* controlflow, Scope* scope) {
}

static void scan_variable(ScanHelper* helper, Variable* var, Scope* scope) {
}

static void scan_statement(ScanHelper* helper, Statement* statement, Code* code) {
	switch (statement->kind) {
		default: assert_unreachable();

		case STATEMENT_ASSIGNMENT: {
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
		case STATEMENT_ASSIGNMENT_BIT_OR:
		case STATEMENT_EXPRESSION:
		case STATEMENT_CONTROLFLOW:
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

