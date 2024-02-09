#include "semantic.h"
#include "ast.h"

static void scan_expression(ScanHelper* helper,  Expression* expr, Scope* scope);
static void scan_assignment(ScanHelper* helper, Assignment* assignment, Code* code);
static void scan_controlflow(ScanHelper* helper, ControlFlow* controlflow, Scope* scope);
static void scan_statement(ScanHelper* helper,   Statement* statement, Code* code);
static void scan_code(ScanHelper* helper,        Code* code);
static void scan_function(ScanHelper* helper,    Function* func);

static bool can_implicit_cast(TypeID a, TypeID b) {
	if (a == b)
		return true;

	return false;
}

static bool can_coercive_cast(TypeID a, TypeID b) {
	if (can_implicit_cast(a, b))
		return true;

	return false;
}

static bool can_explicit_cast(TypeID a, TypeID b) {
	if (can_coercive_cast(a, b))
		return true;

	return false;
}

static bool can_force_cast(TypeID a, TypeID b) {
	if (get_type_size(a) <= get_type_size(b))
		return true;

	return false;
}

static Function* find_function(ScanHelper* helper, String name, TypeID* arg_types, u32 arg_type_count) {
	Module* module = helper->module;

	for (u32 i = 0; module->function_count; i++) {
		Function* func = &module->functions[i];

		if (func->param_count != arg_type_count)
			continue;

		if (name.data != func->name->string.data)
			continue;

		bool fail = false;
		for (u32 i = 0; i < arg_type_count; i++) {
			if (!can_implicit_cast(arg_types[i], func->params[i].type)) {
				fail = true;
				break;
			}
		}

		if (fail)
			continue;

		return func;
	}

	return null;
}

typedef struct UserTypeResult { TypeKind kind; union { Struct* s; Enum* e; }; } UserTypeResult;

static UserTypeResult find_usertype(ScanHelper* helper, String name) {
	Module* module = helper->module;

	for (u32 i = 0; i < module->struct_count; i++) {
		Struct* st = &module->structs[i];

		if (name.data != st->name->string.data)
			continue;

		return (UserTypeResult){ TYPE_KIND_STRUCT, { .s = st } };
	}

	for (u32 i = 0; i < module->enum_count; i++) {
		Enum* en = &module->enums[i];

		if (name.data != en->name->string.data)
			continue;

		return (UserTypeResult){ TYPE_KIND_ENUM, { .e = en } };
	}

	return (UserTypeResult){ };
}

static Struct* find_struct(ScanHelper* helper, String name) {
	Module* module = helper->module;

	for (u32 i = 0; i < module->struct_count; i++) {
		Struct* st = &module->structs[i];

		if (name.data != st->name->string.data)
			continue;

		return st;
	}

	return null;
}

static Enum* find_enum(ScanHelper* helper, String name) {
	Module* module = helper->module;

	for (u32 i = 0; i < module->enum_count; i++) {
		Enum* en = &module->enums[i];

		if (name.data != en->name->string.data)
			continue;

		return en;
	}

	return null;
}

static void scan_identifier(ScanHelper* helper, Expression* expr, Scope* scope) {
	if (expr->term.var) {
		Variable* var = expr->term.var;
		expr->type = var->type;
		return;
	}

	String name = expr->term.token->identifier;

	Scope* out_scope = null;
	Variable* var = find_var(scope, name, &out_scope);

	if (var)
		errore(expr, 1, "Use of variable '%' before it's declaration.\n", arg_string(name));

	errore(expr, 1, "Variable with name '%' does not exist.\n", arg_string(name));
}

static void scan_array(ScanHelper* helper, Expression* expr, Scope* scope) {
	for (u32 i = 0; i < expr->array.elem_count; i++) {
		Expression* elem = expr->array.elems[i];
		scan_expression(helper, elem, scope);
	}
}

static void scan_tuple(ScanHelper* helper, Expression* expr, Scope* scope) {
	TypeID types[expr->tuple.elem_count];

	for (u32 i = 0; i < expr->tuple.elem_count; i++) {
		Expression* elem = expr->tuple.elems[i];
		scan_expression(helper, elem, scope);
		types[i] = elem->type;
	}

	expr->type = get_tuple_type(types, expr->tuple.elem_count);
}

static void scan_formal_usertype_identifier(ScanHelper* helper, Expression* expr, Scope* scope) {
	UserTypeResult utr = find_usertype(helper, expr->term.token->identifier);
	expr->type = TYPE_TYPEID;

	if (!utr.kind)
		errore(expr, 3, "No such struct or enum called '%'.\n", arg_string(expr->term.token->identifier));

	if (utr.kind == TYPE_KIND_STRUCT) {
		expr->kind = EXPR_BASETYPE_STRUCT;
		utr.s = utr.s;
	}
	else {
		expr->kind = EXPR_BASETYPE_ENUM;
		utr.e = utr.e;
	}
}

static void scan_expression(ScanHelper* helper, Expression* expr, Scope* scope) {
	switch (expr->kind) {
		case EXPR_NULL:
		case EXPR_TRUE:
		case EXPR_FALSE:
		case EXPR_LITERAL:
		case EXPR_BASETYPE_PRIMITIVE:
		case EXPR_BASETYPE_STRUCT:
		case EXPR_BASETYPE_ENUM:
		case EXPR_FUNCTION:
		case EXPR_UNARY_IMPLICIT_CAST:
			break;

		case EXPR_IDENTIFIER_FORMAL: {
			scan_formal_usertype_identifier(helper, expr, scope);
		} break;

		case EXPR_IDENTIFIER_CONSTANT:
		case EXPR_IDENTIFIER_VARIABLE: {
			scan_identifier(helper, expr, scope);
		} break;

		case EXPR_ARRAY: {
			scan_array(helper, expr, scope);
		} break;

		case EXPR_TUPLE: {
			scan_tuple(helper, expr, scope);
		} break;

		case EXPR_SPEC_PTR: {
			scan_expression(helper, expr->specifier.sub, scope);
		} break;

		case EXPR_SPEC_ARRAY: {
			scan_expression(helper, expr->specifier.sub, scope);
		} break;

		case EXPR_SPEC_FIXED: {
			scan_expression(helper, expr->specifier.length, scope);
			scan_expression(helper, expr->specifier.sub, scope);
		} break;

		case EXPR_UNARY_PTR:
		case EXPR_UNARY_REF:
		case EXPR_UNARY_ABS:
		case EXPR_UNARY_INVERSE:
		case EXPR_UNARY_NOT:
		case EXPR_UNARY_BIT_NOT: {
		} break;

		case EXPR_BINARY_ADD: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_SUB: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_MUL: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_DIV: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_MOD: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_BIT_XOR: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_BIT_AND: {
			scan_expression(helper, expr->binary.left,  scope);
			scan_expression(helper, expr->binary.right, scope);
		};

		case EXPR_BINARY_BIT_OR:
		case EXPR_BINARY_OR:
		case EXPR_BINARY_AND:
		case EXPR_BINARY_EQUAL:
		case EXPR_BINARY_NOT_EQUAL:
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_LESS_OR_EQUAL:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_GREATER_OR_EQUAL:
		case EXPR_BINARY_DOT_DOT:
		case EXPR_BINARY_LSHIFT:
		case EXPR_BINARY_RSHIFT: {
			scan_expression(helper, expr->binary.left, scope);
			scan_expression(helper, expr->binary.right, scope);
		} break;

		case EXPR_BINARY_DOT: {
		} break;

		case EXPR_BINARY_SPAN: {
		} break;

		case EXPR_CALL: {
		} break;

		case EXPR_INDEX: {
		} break;

		case EXPR_TERNARY_IF_ELSE: {
		} break;

	}
}

static void scan_variable(ScanHelper* helper, Variable* var, Scope* scope) {
	if (var->type_expr)
		scan_expression(helper, var->type_expr, scope);

	if (var->init_expr)
		scan_expression(helper, var->init_expr, scope);

	// @Todo: Get TypeID from type_expr
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

	if (!(assignment->left->flags & EXPR_FLAG_REF))
		errore(assignment->left, 3, "Expression is not assignable.\n");

	scan_expression(helper, assignment->right, &code->scope);

	if (!can_coercive_cast(assignment->left->type, assignment->right->type))
		errore(assignment->right, 3, "Cannot convert type '%' to '%'\n", arg_type(assignment->right->type), arg_type(assignment->left->type));
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
		case STATEMENT_ASSIGNMENT_BIT_OR: {
			scan_assignment(helper, &statement->assign, code);
		} break;

		case STATEMENT_EXPRESSION:
			scan_expression(helper, statement->expr, &code->scope);
			break;

		case STATEMENT_CONTROLFLOW:
			scan_controlflow(helper, &statement->controlflow, &code->scope);
			break;

		case STATEMENT_VARDECL: {
			scan_variable(helper, statement->var, &code->scope);
		} break;

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
		.loop = null,
		.module = module,
	};

	print("Scanning...\n");
	for (u32 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		print("Scanning function '%'\n", arg_token(func->name));
		scan_function(&helper, func);
	}
}

