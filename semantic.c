#include "semantic.h"
#include "ast.h"

static void scan_expression(ScanHelper* helper, Scope* scope, Expression* expr);
static void scan_assignment(ScanHelper* helper, Code* code, Assignment* assignment);
static void scan_controlflow(ScanHelper* helper, Scope* scope, ControlFlow* controlflow);
static void scan_statement(ScanHelper* helper, Code* code, Statement* statement);
static void scan_code(ScanHelper* helper, Code* code);
static void scan_function(ScanHelper* helper, Function* func);

static void insert_cast_expression(ScanHelper* helper, Expression** pexpr, TypeID to) {
	Expression* expr = *pexpr;

	Expression* result = alloc_expression();
	*result = (Expression){
		.kind = EXPR_UNARY_IMPLICIT_CAST,
		.unary.sub = expr,
		.type = to,
		.begin = expr->begin,
		.end = expr->end,
		.scope = expr->scope,
	};

	*pexpr = result;
}

static void cast_to_integral_type(ScanHelper* helper, Expression** pexpr) {
	Expression* expr = *pexpr;
	Module* module = helper->module;

	assert(expr->type);

	if (expr->type == TYPE_BOOL) {
		insert_cast_expression(helper, pexpr, TYPE_INT8);
	}
}

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

static void scan_identifier(ScanHelper* helper, Scope* scope, Expression* expr) {
	if (expr->term.var) {
		Variable* var = expr->term.var;
		expr->type = var->type;
		print("var->type = %\n", arg_type(var->type));
		return;
	}

	String name = expr->term.token->identifier;

	Scope* out_scope = null;
	Variable* var = find_var(scope, name, &out_scope);

	if (var)
		errore(expr, 1, "Use of variable '%' before it's declaration.\n", arg_string(name));

	errore(expr, 1, "Variable with name '%' does not exist.\n", arg_string(name));
}

static void scan_array(ScanHelper* helper, Scope* scope, Expression* expr) {
	for (u32 i = 0; i < expr->array.elem_count; i++) {
		Expression* elem = expr->array.elems[i];
		scan_expression(helper, scope, elem);
	}
}

static void scan_tuple(ScanHelper* helper, Scope* scope, Expression* expr) {
	TypeID types[expr->tuple.elem_count];

	for (u32 i = 0; i < expr->tuple.elem_count; i++) {
		Expression* elem = expr->tuple.elems[i];
		scan_expression(helper, scope, elem);
		types[i] = elem->type;
	}

	expr->type = get_tuple_type(types, expr->tuple.elem_count);
}

static void scan_formal_usertype_identifier(ScanHelper* helper, Scope* scope, Expression* expr) {
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

static void scan_unary_ptr(ScanHelper* helper, Scope* scope, Expression* expr) {
	scan_expression(helper, scope, expr->unary.sub);
	Expression* sub = expr->unary.sub;
	assert(sub->type);

	if (sub->type == TYPE_TYPEID && (sub->flags & EXPR_FLAG_CONSTANT)) {
		Value* v = ir_get_value(sub->value);
		assert(sub->value);
		assert(v->is_const);
		TypeID subtype = v->const_int;
		TypeID newtype = get_ptr_type(subtype);

		*expr = (Expression) {
			.kind  = EXPR_SPEC_PTR,
			.flags = expr->flags,
			.type  = TYPE_TYPEID,
			.value = ir_int(newtype),

			.specifier = {
				.sub    = sub,
				.length = null,
			},
		};

		return;
	}

	if (!(sub->flags & EXPR_FLAG_REF))
		errore(sub, 3, "Expression must be referential.\n");

	expr->type = get_ptr_type(sub->type);

	// Set expr->value
}

static void scan_unary_ref(ScanHelper* helper, Scope* scope, Expression* expr) {
	Expression* sub = expr->unary.sub;
	scan_expression(helper, scope, sub);

	if (!is_ptr(sub->type))
		errore(sub, 3, "Expression must be a pointer.\n");

	expr->type = get_subtype(sub->type);
}

static void scan_expression(ScanHelper* helper, Scope* scope, Expression* expr) {
	// print("expr = %\n", arg_expression(expr));
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
			scan_formal_usertype_identifier(helper, scope, expr);
		} break;

		case EXPR_IDENTIFIER_CONSTANT:
		case EXPR_IDENTIFIER_VARIABLE: {
			scan_identifier(helper, scope, expr);
		} break;

		case EXPR_ARRAY: {
			scan_array(helper, scope, expr);
		} break;

		case EXPR_TUPLE: {
			scan_tuple(helper, scope, expr);
		} break;

		case EXPR_SPEC_PTR: {
			scan_expression(helper, scope, expr->specifier.sub);
		} break;

		case EXPR_SPEC_ARRAY: {
			scan_expression(helper, scope, expr->specifier.sub);
		} break;

		case EXPR_SPEC_FIXED: {
			scan_expression(helper, scope, expr->specifier.length);
			scan_expression(helper, scope, expr->specifier.sub);
		} break;

		case EXPR_UNARY_PTR: {
			scan_unary_ptr(helper, scope, expr);
		} break;

		case EXPR_UNARY_REF: {
			scan_unary_ref(helper, scope, expr);
		} break;

		case EXPR_UNARY_ABS:
		case EXPR_UNARY_INVERSE:
		case EXPR_UNARY_NOT:
		case EXPR_UNARY_BIT_NOT: {
		} break;

		case EXPR_BINARY_ADD: {
		};

		case EXPR_BINARY_SUB: {
		};

		case EXPR_BINARY_MUL: {
		};

		case EXPR_BINARY_DIV: {
		};

		case EXPR_BINARY_MOD: {
		};

		case EXPR_BINARY_BIT_XOR: {
		};

		case EXPR_BINARY_BIT_AND: {
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
			scan_expression(helper, scope, expr->binary.left);
			scan_expression(helper, scope, expr->binary.right);
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

	assert(expr->type);
}

static void scan_variable(ScanHelper* helper, Scope* scope, Variable* var) {
	if (var->type_expr) {
		// print("var->type_expr\n");
		scan_expression(helper, scope, var->type_expr);
		var->type = ir_get_const_int(var->type_expr->value);
	}

	if (var->init_expr) {
		// print("var->init_expr\n");
		scan_expression(helper, scope, var->init_expr);

		// if ((var->init_expr->flags & EXPR_FLAG_CONSTANT)
	}

	// @Todo: Get TypeID from type_expr
}

static void scan_branch(ScanHelper* helper, Scope* scope, Branch* branch) {
	bool is_branch = false;

	switch (branch->branch_kind) {
		case BRANCH_NAKED: {
		} break;

		case BRANCH_IF: {
			scan_expression(helper, scope, branch->cond); // @Todo: Convert to bool
		} break;

		case BRANCH_FOR: {
			is_branch = true;
			scan_variable(helper, scope, branch->var);
			scan_expression(helper, scope, branch->cond); // @Todo: Convert to bool
			scan_expression(helper, scope, branch->nextval); // @Todo: Check if nextval type can be converted to branch->var->type
		} break;

		case BRANCH_WHILE: {
			is_branch = true;
			scan_expression(helper, scope, branch->cond); // @Todo: Convert to bool
		} break;

		case BRANCH_MATCH: {
			is_branch = true;
			scan_expression(helper, scope, branch->cond);
		} break;
	}

	Branch* last_loop = helper->loop;

	if (is_branch) {
		helper->loop = branch;
	}

	scan_code(helper, &branch->code);

	helper->loop = last_loop;
}

static void scan_controlflow(ScanHelper* helper, Scope* scope, ControlFlow* controlflow) {
	for (u32 i = 0; i < controlflow->branch_count; i++) {
		Branch* branch = &controlflow->branches[i];
		scan_branch(helper, scope, branch);
	}
}

static void scan_assignment(ScanHelper* helper, Code* code, Assignment* assignment) {
	scan_expression(helper, &code->scope, assignment->left);

	if (!(assignment->left->flags & EXPR_FLAG_REF))
		errore(assignment->left, 3, "Expression is not assignable.\n");

	scan_expression(helper, &code->scope, assignment->right);

	if (!can_coercive_cast(assignment->left->type, assignment->right->type))
		errore(assignment->right, 3, "Cannot convert type '%' to '%'\n", arg_type(assignment->right->type), arg_type(assignment->left->type));
}

static void scan_statement(ScanHelper* helper, Code* code, Statement* statement) {
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
			scan_assignment(helper, code, &statement->assign);
		} break;

		case STATEMENT_EXPRESSION: {
			scan_expression(helper, &code->scope, statement->expr);
		} break;

		case STATEMENT_CONTROLFLOW: {
			scan_controlflow(helper, &code->scope, &statement->controlflow);
		} break;

		case STATEMENT_VARDECL: {
			scan_variable(helper, &code->scope, statement->var);
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
		scan_statement(helper, code, statement);
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

