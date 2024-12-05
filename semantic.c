#include "semantic.h"
#include "ast.h"

static void scan_expression(ScanHelper* helper, Scope* scope, Expression* expr);
static void scan_assignment(ScanHelper* helper, Code* code, Assignment* assignment);
static void scan_controlflow(ScanHelper* helper, Scope* scope, ControlFlow* controlflow);
static void scan_statement(ScanHelper* helper, Code* code, Statement* statement);
static void scan_code(ScanHelper* helper, Code* code);
static void scan_function(ScanHelper* helper, Function* func);

static void insert_cast_expression(ScanHelper* helper, Expression** pexpr, TypeID to) {
	assert(pexpr);
	assert(*pexpr);

	Expression* expr = *pexpr;

	if (expr->type == to)
		return;

	Expression* result = alloc_expression();
	*result = (Expression){
		.kind  = EXPR_UNARY_IMPLICIT_CAST,
		.left  = expr,
		.type  = to,
		.begin = expr->begin,
		.end   = expr->end,
		.scope = expr->scope,
	};

	*pexpr = result;
}

static void insert_cast_to_integral(ScanHelper* helper, Expression** pexpr) {
	assert(pexpr);
	assert(*pexpr);
	Expression* expr = *pexpr;
	assert(expr->type);
	TypeID integral_type = ts_get_integral_type(expr->type);

	if (integral_type)
		insert_cast_expression(helper, pexpr, integral_type);
}

static bool can_implicit_cast(TypeID a, TypeID b) {
	if (a == b)
		return true;

	if (a == ts_get_ptr(TYPE_BYTE) && ts_is_ptr(b))
		return true;

	if (ts_is_ptr(a) && b == ts_get_ptr(TYPE_BYTE))
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
	if (ts_get_size(a) <= ts_get_size(b))
		return true;

	return false;
}

static Function* find_function(ScanHelper* helper, String name, TypeID* arg_types, u32 arg_type_count) {
	Module* module = helper->module;

	for (u32 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];

		if (func->param_count != arg_type_count)
			continue;

		if (name.data != func->name->string.data)
			continue;

		for (u32 i = 0; i < arg_type_count; i++)
			if (!can_implicit_cast(arg_types[i], func->params[i].type))
				goto skip;

		return func;
		skip: continue;
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

	return (UserTypeResult){ 0, null };
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
		// print("var->type = %\n", arg_type(var->type));
		return;
	}

	String name = expr->term.token->identifier;

	Scope* out_scope = null;
	Variable* var = find_var(scope, name, &out_scope);

	if (var)
		errore(expr, 1, "Use of variable '%' before it's declaration.\n", arg_string(name));

	errore(expr, 1, "Variable with name '%' does not exist.\n", arg_string(name));

	expr->type = var->type;
}

static void scan_array(ScanHelper* helper, Scope* scope, Expression* expr) {
	Expression** elems = expr_table_get(&expr->array.elems);
	for (u32 i = 0; i < expr->array.elems.count; i++) {
		scan_expression(helper, scope, elems[i]);
	}
}

static void scan_tuple(ScanHelper* helper, Scope* scope, Expression* expr) {
	TypeID types[expr->tuple.elems.count];
	Expression** elems = expr_table_get(&expr->tuple.elems);

	for (u32 i = 0; i < expr->tuple.elems.count; i++) {
		scan_expression(helper, scope, elems[i]);
		types[i] = elems[i]->type;
	}

	expr->type = ts_get_tuple(types, expr->tuple.elems.count);
}

static void scan_formal_usertype_identifier(ScanHelper* helper, Scope* scope, Expression* expr) {
	expr->type = TYPE_TYPEID;

	UserTypeResult utr = find_usertype(helper, expr->term.token->identifier);

	if (!utr.kind)
		errore(expr, 3, "No such struct or enum called '%'.\n", arg_string(expr->term.token->identifier));

	if (utr.kind == TYPE_KIND_STRUCT) {
		expr->kind = EXPR_BASETYPE_STRUCT;
		expr->value = ir_int((u64)utr.s);
	}
	else {
		expr->kind = EXPR_BASETYPE_ENUM;
		expr->value = ir_int((u64)utr.e);
	}
}

static void scan_unary_ptr(ScanHelper* helper, Scope* scope, Expression* expr) {
	scan_expression(helper, scope, expr->left);
	Expression* sub = expr->left;
	assert(sub->type);

	if (sub->type == TYPE_TYPEID && (sub->flags & EXPR_FLAG_CONSTANT)) {
		Value* v = ir_get_value(sub->value);
		assert(sub->value);
		assert(v->is_const);
		TypeID subtype = v->const_int;
		TypeID newtype = ts_get_ptr(subtype);

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

	expr->type = ts_get_ptr(sub->type);

	// Set expr->value
}

static void scan_unary_ref(ScanHelper* helper, Scope* scope, Expression* expr) {
	Expression* sub = expr->left;
	scan_expression(helper, scope, sub);

	if (!ts_is_ptr(sub->type))
		errore(sub, 3, "Expression must be a pointer.\n");

	expr->type = ts_get_subtype(sub->type);
}

static TypeID get_preferred_integral(TypeID a, TypeID b) {
	assert(ts_is_int(a));
	assert(ts_is_int(b));

	if (a == b)
		return a;

	TypeID result_type = ts_get_size(a) >= ts_get_size(a) ? a : b;

	if (ts_is_unsigned(a) || ts_is_unsigned(b))
		result_type = ts_get_unsigned(result_type);

	return result_type;
}

static void scan_binary_add(ScanHelper* helper, Scope* scope, Expression* expr) {
	scan_expression(helper, scope, expr->left);
	scan_expression(helper, scope, expr->right);

	// *T + uint
	// float64 + float64
	// float32 + float32
	// uint + uint
	// int  + int

	TypeID ltype = expr->left->type;
	TypeID rtype = expr->right->type;

	// *T + uint
	if (ts_is_ptr(ltype) && can_coercive_cast(rtype, TYPE_UINT64)) {
		expr->kind = EXPR_BINARY_ADD_INDEX;

		insert_cast_expression(helper, &expr->right, TYPE_UINT64);
		expr->type = expr->left->type;
		return;
	}

	// float64 + float64
	if (ltype == TYPE_FLOAT64 || rtype == TYPE_FLOAT64) {
		expr->kind = EXPR_BINARY_ADD_FP64;

		if (!can_coercive_cast(ltype, TYPE_FLOAT64))
			errore(expr->left, 3, "Type % is not convertable to float64\n", arg_type(ltype));

		if (!can_coercive_cast(rtype, TYPE_FLOAT64))
			errore(expr->right, 3, "Type % is not convertable to float64\n", arg_type(rtype));

		insert_cast_expression(helper, &expr->left,  TYPE_FLOAT64);
		insert_cast_expression(helper, &expr->right, TYPE_FLOAT64);

		return;
	}

	// float32 + float32
	if (ltype == TYPE_FLOAT32 || rtype == TYPE_FLOAT32) {
		expr->kind = EXPR_BINARY_ADD_FP32;

		if (!can_coercive_cast(ltype, TYPE_FLOAT32))
			errore(expr->left, 3, "Type % is not convertable to float32\n", arg_type(ltype));

		if (!can_coercive_cast(rtype, TYPE_FLOAT32))
			errore(expr->right, 3, "Type % is not convertable to float32\n", arg_type(rtype));

		insert_cast_expression(helper, &expr->left,  TYPE_FLOAT32);
		insert_cast_expression(helper, &expr->right, TYPE_FLOAT32);
		return;
	}

	if (!ts_get_integral_type(ltype))
		errore(expr, 3, "'%' is not an integral type.\n", arg_type(ltype));

	if (!ts_get_integral_type(rtype))
		errore(expr, 3, "'%' is not an integral type.\n", arg_type(rtype));

	insert_cast_to_integral(helper, &expr->left);
	insert_cast_to_integral(helper, &expr->right);

	ltype = expr->left->type;
	rtype = expr->right->type;

	TypeID preferred_type = get_preferred_integral(ltype, rtype);

	insert_cast_expression(helper, &expr->left,  preferred_type);
	insert_cast_expression(helper, &expr->right, preferred_type);

	expr->type = preferred_type;
}

static void scan_binary_sub(ScanHelper* helper, Scope* scope, Expression* expr) {
	scan_expression(helper, scope, expr->left);
	scan_expression(helper, scope, expr->right);

	// *T - *T
	// *T - int
	// float64 - float64
	// float32 - float32
	// uint - uint
	// int  - int

	TypeID ltype = expr->left->type;
	TypeID rtype = expr->right->type;

	// *T - *T
	if (ts_is_ptr(ltype) && ts_is_ptr(rtype)) {
		if (ltype != rtype)
			errore(expr->left, 3, "Cannot subtract types '%' and '%'.\n", arg_type(ltype), arg_type(rtype));

		expr->type = TYPE_UINT64;

		return;
	}

	// *T - int
	if (ts_is_ptr(ltype) && ts_is_integral_type(rtype)) {
		insert_cast_to_integral(helper, &expr->right);

		expr->type = ltype;

		return;
	}

	// float64 - float64
	if (ltype == TYPE_FLOAT64 || rtype == TYPE_FLOAT64) {
		insert_cast_expression(helper, &expr->left,  TYPE_FLOAT64);
		insert_cast_expression(helper, &expr->right, TYPE_FLOAT64);

		expr->type = TYPE_FLOAT64;

		return;
	}

	// float32 - float32
	if (ltype == TYPE_FLOAT32 || rtype == TYPE_FLOAT32) {
		insert_cast_expression(helper, &expr->left,  TYPE_FLOAT32);
		insert_cast_expression(helper, &expr->right, TYPE_FLOAT32);

		expr->type = TYPE_FLOAT32;

		return;
	}

	if (!ts_get_integral_type(ltype))
		errore(expr, 3, "'%' is not an integral type.\n", arg_type(ltype));

	if (!ts_get_integral_type(rtype))
		errore(expr, 3, "'%' is not an integral type.\n", arg_type(rtype));

	insert_cast_to_integral(helper, &expr->left);
	insert_cast_to_integral(helper, &expr->right);

	ltype = expr->left->type;
	rtype = expr->right->type;

	TypeID type = get_preferred_integral(ltype, rtype);

	insert_cast_expression(helper, &expr->left,  type);
	insert_cast_expression(helper, &expr->right, type);
}

static bool is_type_indexable(TypeID tid) {
	TypeKind kind = ts_get_kind(tid);

	switch (kind) {
		case TYPE_KIND_ARRAY:
		case TYPE_KIND_FIXED:
		case TYPE_KIND_PTR:
			return true;

		default:
			return false;
	}
}

static void scan_call(ScanHelper* helper, Scope* scope, Expression* expr) {
	Expression* func_expr = expr->call.function;

	// Make sure function expression gets evaluated before params.
	if (expr->call.function->kind != EXPR_IDENTIFIER_FORMAL) {
		scan_expression(helper, scope, func_expr);
	}

	u32 arg_count = expr->call.args.count;
	Expression** args = expr_table_get(&expr->call.args);
	TypeID arg_types[arg_count];

	for (u32 i = 0; i < arg_count; i++) {
		scan_expression(helper, scope, args[i]);
		arg_types[i] = args[i]->type;
	}

	if (func_expr->kind == EXPR_IDENTIFIER_FORMAL) {
		// Manually scan the subexpression.
		String name = func_expr->term.token->string;
		Function* function = find_function(helper, name, arg_types, arg_count);

		if (!function)
			errore(func_expr, 3, "No such function called: '%'.\n", arg_string(name));

		func_expr->term.func = function;
		print("function->type = %\n", arg_type(function->type));
		func_expr->type = function->type;
		func_expr->flags |= EXPR_FLAG_REF;
	}

	expr->type = ts_get_function_return_type(func_expr->type);
}

static void scan_dot(ScanHelper* helper, Scope* scope, Expression* expr) {
	scan_expression(helper, scope, expr->left);

	if (expr->right->kind != EXPR_IDENTIFIER_VARIABLE)
		errore(expr->right, 3, "Invalid dot expression, expected field identifier.\n");

	// @todo
}

static void scan_expression(ScanHelper* helper, Scope* scope, Expression* expr) {
	// print("expr = %\n", arg_expression(expr));

	assert(expr);

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
			break; // parser.c sets up these expressions. No scan needed.

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

		case EXPR_BINARY_OR:
		case EXPR_BINARY_AND: {
			scan_expression(helper, scope, expr->left);
			scan_expression(helper, scope, expr->right);

			if (!can_coercive_cast(expr->left->type, TYPE_BOOL))
				errore(expr->left, 3, "Cannot cast type % to bool.\n", arg_type(expr->left->type));

			if (!can_coercive_cast(expr->right->type, TYPE_BOOL))
				errore(expr->right, 3, "Cannot cast type % to bool.\n", arg_type(expr->right->type));

			insert_cast_expression(helper, &expr->left,  TYPE_BOOL);
			insert_cast_expression(helper, &expr->right, TYPE_BOOL);

			expr->type = TYPE_BOOL;
		} break;

		case EXPR_BINARY_ADD: {
			scan_binary_add(helper, scope, expr);
		} break;

		case EXPR_BINARY_SUB: {
			scan_binary_sub(helper, scope, expr);
		} break;

		case EXPR_BINARY_MUL: {
		} break;

		case EXPR_BINARY_DIV: {
		} break;

		case EXPR_BINARY_MOD: {
		} break;

		case EXPR_BINARY_BIT_XOR:
		case EXPR_BINARY_BIT_AND:
		case EXPR_BINARY_BIT_OR:

		case EXPR_BINARY_EQUAL:
		case EXPR_BINARY_NOT_EQUAL:
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_LESS_OR_EQUAL:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_GREATER_OR_EQUAL:
		case EXPR_BINARY_DOT_DOT:
		case EXPR_BINARY_LSHIFT:
		case EXPR_BINARY_RSHIFT: {
			scan_expression(helper, scope, expr->left);
			scan_expression(helper, scope, expr->right);
		} break;

		case EXPR_BINARY_DOT: {
			scan_dot(helper, scope, expr);
		} break;

		case EXPR_BINARY_SPAN: {
			scan_expression(helper, scope, expr->left);
			scan_expression(helper, scope, expr->right);
		} break;

		case EXPR_CALL: {
			scan_call(helper, scope, expr);
		} break;

		case EXPR_INDEX: {
			scan_expression(helper, scope, expr->subscript.base);
			scan_expression(helper, scope, expr->subscript.index);

			if (!can_coercive_cast(expr->subscript.index->type, TYPE_UINT64))
				errore(expr->subscript.base, 3, "Cannot convert type '%' to type 'uint64'\n", arg_type(expr->subscript.index->type));

			if (!is_type_indexable(expr->subscript.base->type))
				errore(expr->subscript.base, 3, "Type '%' is not an indexable type.\n", arg_type(expr->subscript.base->type));

			expr->type = ts_get_subtype(expr->subscript.base->type);
		} break;

		case EXPR_TERNARY_IF_ELSE: {
			scan_expression(helper, scope, expr->left);
			scan_expression(helper, scope, expr->middle);
			scan_expression(helper, scope, expr->right);

			insert_cast_expression(helper, &expr->middle, TYPE_BOOL);
			insert_cast_expression(helper, &expr->right, expr->left->type);
		} break;

		default:
			assert_unreachable();

	}

	assert(expr->type);
}

static void scan_variable(ScanHelper* helper, Scope* scope, Variable* var) {
	if (var->type_expr) {
		scan_expression(helper, scope, var->type_expr);
		var->type = ir_get_const_int(var->type_expr->value);

		if (var->type == TYPE_EMPTY_TUPLE)
			errore(var->type_expr, 3, "Cannot create variable with empty tuple type.\n");
	}

	if (var->init_expr) {
		scan_expression(helper, scope, var->init_expr);

		if (!var->type_expr) {
			var->type = var->init_expr->type;

			if (var->type == TYPE_EMPTY_TUPLE)
				errore(var->init_expr, 3, "Assignment to empty tuple is not allowed.\n");
		}
		// if ((var->init_expr->flags & EXPR_FLAG_CONSTANT)
	}

	if (var->type_expr && var->init_expr) {
		if (!can_coercive_cast(var->init_expr->type, var->type))
			errore(var->init_expr, 3, "Cannot cast '%' to '%'\n", arg_type(var->init_expr->type), arg_type(var->type));

		insert_cast_expression(helper, &var->init_expr, var->type);
	}

	print("Scanned variable declaration:\n\tname = '%'\n\ttype = %\n\tinit_expr = %\n\tinit_expr->type = %\n",
		arg_token(var->name),
		arg_type(var->type),
		arg_expression(var->init_expr),
		arg_type(var->init_expr ? var->init_expr->type : TYPE_EMPTY_TUPLE)
	);
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

		case STATEMENT_RETURN: {
			scan_expression(helper, &code->scope, statement->ret.expr);
		} break;

		case STATEMENT_BREAK:
		case STATEMENT_CONTINUE: {
		} break;

		case STATEMENT_INC:
		case STATEMENT_DEC: {
			bool dir = statement->kind == STATEMENT_INC;

			scan_expression(helper, &code->scope, statement->expr);
		} break;

		default:
			assert_unreachable();
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

static void prescan_function(ScanHelper* helper, Function* func) {
	Module* module = helper->module;
	Expression* retexpr = func->return_type_expr;

	TypeID input_type  = TYPE_EMPTY_TUPLE;
	TypeID output_type = TYPE_EMPTY_TUPLE;

	if (retexpr) {
		scan_expression(helper, &module->scope, func->return_type_expr);

		if (retexpr->type != TYPE_TYPEID)
			errore(retexpr, 5, "Invalid return type: %\n", arg_expression(retexpr));

		if (!(retexpr->flags & EXPR_FLAG_CONSTANT))
			errore(retexpr, 5, "Return type must be a constant.\n", arg_expression(retexpr));

		output_type = ir_get_const_int(func->return_type_expr->value);
	}

	TypeID param_types[func->param_count];
	for (u32 i = 0; i < func->param_count; i++) {
		Variable* param = &func->params[i];
		scan_variable(helper, &module->scope, param);
		param_types[i] = param->type;
	}

	input_type = ts_get_tuple(param_types, func->param_count);

	func->type = ts_get_func(input_type, output_type);

}

static void prescan(ScanHelper* helper)
{
	Module* module = helper->module;

	for (u32 i = 0; i < module->function_count; i++) {
		Function* function = &module->functions[i];
		print("Prescanning function: '%'\n", arg_string(function->name->string));
		prescan_function(helper, function);
		print("Function '%' has type %\n", arg_string(function->name->string), arg_type(function->type));
	}
}

static void scan_module(Module* module) {
	ScanHelper helper = (ScanHelper){
		.loop = null,
		.module = module,
	};

	prescan(&helper);

	print("Scanning...\n");
	for (u32 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		print("Scanning function '%'\n", arg_token(func->name));
		scan_function(&helper, func);
	}
}

