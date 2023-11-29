#include "ir.h"
#include "ast.h"

Procedure* initproc;
Procedure** procedures;
u32 procedure_count;
u32 procedure_capacity;

#define NONE ((Value){ .kind = VALUE_NONE })
static Value vint(s64 i)              { return (Value){ .kind = VALUE_INT,         .i = i }; }
static Value vinst(Instruction* inst) { return (Value){ .kind = VALUE_INSTRUCTION, .instruction = inst }; }
static Value vblock(Block* block)     { return (Value){ .kind = VALUE_BLOCK,       .block = block }; }
static Value vproc(Procedure* proc)   { return (Value){ .kind = VALUE_PROCEDURE,   .procedure = proc }; }

static char* instruction_kind_to_cstring(InstructionKind kind) {
	switch (kind) {
		case INSTRUCTION_NOOP:     return "noop";
		case INSTRUCTION_STACK:    return "stack";
		case INSTRUCTION_PARAM:    return "param";
		case INSTRUCTION_GLOBAL:   return "global";

		case INSTRUCTION_SELECT:   return "select";
		case INSTRUCTION_INDEX:    return "index";

		case INSTRUCTION_LOAD:     return "load";
		case INSTRUCTION_STORE:    return "store";

		case INSTRUCTION_CMPEQ:    return "cmpeq";
		case INSTRUCTION_CMPNE:    return "cmpne";
		case INSTRUCTION_SCMPL:    return "scmpl";
		case INSTRUCTION_SCMPLE:   return "scmple";
		case INSTRUCTION_SCMPG:    return "scmpg";
		case INSTRUCTION_SCMPGE:   return "scmpge";
		case INSTRUCTION_UCMPL:    return "ucmpl";
		case INSTRUCTION_UCMPLE:   return "ucmple";
		case INSTRUCTION_UCMPG:    return "ucmpg";
		case INSTRUCTION_UCMPGE:   return "ucmpge";

		case INSTRUCTION_AND:      return "and";
		case INSTRUCTION_OR:       return "or";

		case INSTRUCTION_ADD:      return "add";
		case INSTRUCTION_SUB:      return "sub";
		case INSTRUCTION_SMUL:     return "smul";
		case INSTRUCTION_SDIV:     return "sdiv";
		case INSTRUCTION_SMOD:     return "smod";
		case INSTRUCTION_UMUL:     return "umul";
		case INSTRUCTION_UDIV:     return "udiv";
		case INSTRUCTION_UMOD:     return "umod";

		case INSTRUCTION_SLSH:     return "slsh";
		case INSTRUCTION_SRSH:     return "srsh";

		case INSTRUCTION_ULSH:     return "ulsh";
		case INSTRUCTION_URSH:     return "ursh";

		case INSTRUCTION_BIT_NOT:  return "bit_not";
		case INSTRUCTION_BIT_XOR:  return "bit_xor";
		case INSTRUCTION_BIT_AND:  return "bit_and";
		case INSTRUCTION_BIT_OR:   return "bit_or";

		case INSTRUCTION_F32_ADD:  return "f32 add";
		case INSTRUCTION_F32_SUB:  return "f32 sub";
		case INSTRUCTION_F32_MUL:  return "f32 mul";
		case INSTRUCTION_F32_DIV:  return "f32 div";

		case INSTRUCTION_F64_ADD:  return "f64 add";
		case INSTRUCTION_F64_SUB:  return "f64 sub";
		case INSTRUCTION_F64_MUL:  return "f64 mul";
		case INSTRUCTION_F64_DIV:  return "f64 div";

		case INSTRUCTION_PHI:      return "phi";

		case INSTRUCTION_CALL:     return "call";

		case INSTRUCTION_JUMP:     return "jmp";
		case INSTRUCTION_BRANCH:   return "br";
		case INSTRUCTION_RETURN:   return "ret";
	}
};

void print_value(Value value) {
	switch (value.kind) {
		case VALUE_NONE:        print("VALUE_NONE"); return;
		case VALUE_BLOCK:       print("block%", arg_u32(value.block->id)); return;
		case VALUE_PROCEDURE:   print("proc_%", arg_string(value.procedure->name)); return;
		case VALUE_INSTRUCTION: print("\\%%", arg_u32(value.instruction->id)); return;
		case VALUE_INT: print("int %", arg_s64(value.i));   return;
		case VALUE_F32: print("f32 %", arg_f32(value.f32)); return;
		case VALUE_F64: print("f64 %", arg_f64(value.f64)); return;
	}
}

bool is_value_instruction(InstructionKind kind) {
	switch (kind) {
		case INSTRUCTION_BRANCH:
		case INSTRUCTION_JUMP:
		case INSTRUCTION_RETURN:
			return false;
		default:
			return true;
	}
}

void print_instruction(Instruction* instruction) {
	print("  ");

	if (is_value_instruction(instruction->kind)) {
		print_value(vinst(instruction));
		print(" = ");
	}

	print("% ", arg_cstring(instruction_kind_to_cstring(instruction->kind)));

	if (instruction->a.kind) {
		print_value(instruction->a);
	}

	if (instruction->b.kind) {
		print(", ");
		print_value(instruction->b);
	}

	if (instruction->c.kind) {
		print(", ");
		print_value(instruction->c);
	}

	print("\n");
}

void print_block(Block* block) {
	print("block%:\n", arg_u32(block->id));
	for (u32 i = 0; i < block->instruction_count; i++) {
		Instruction* instruction = block->instructions[i];
		print_instruction(instruction);
	}
}

void print_procedure(Procedure* proc) {
	print("proc_%:\n", arg_string(proc->name));
	for (u32 i = 0; i < proc->block_count; i++) {
		Block* block = proc->blocks[i];
		print_block(block);
	}
}

Procedure* make_procedure(String name) {
	Procedure* proc = alloc(sizeof(Procedure));

	*proc = (Procedure){
		.name = name,
	};

	if (procedure_count == procedure_capacity) {
		procedure_capacity *= 2;
		procedures = realloc(procedures, sizeof(Procedure*)*procedure_count, sizeof(Procedure*)*procedure_capacity);
	}

	procedures[procedure_count++] = proc;

	return proc;
}

Block* add_block(Procedure* proc) {
	Block* block = alloc(sizeof(Block));
	zero(block, sizeof(Block));
	block->id = proc->block_count;

	block->instruction_capacity = 16;
	block->instruction_count = 0,
	block->instructions = alloc(sizeof(Instruction*)*block->instruction_capacity);
	block->procedure = proc;

	if (is_pow2_or_zero(proc->block_count))
		proc->blocks = realloc(proc->blocks, sizeof(Block*)*proc->block_count, sizeof(Block*)*next_pow2(proc->block_count));

	proc->blocks[proc->block_count++] = block;

	return block;
}

void reg_user(Value value, Instruction* user) {
	UserStore* user_store = null;

	switch (value.kind) {
		case VALUE_NONE:
		case VALUE_INT:
		case VALUE_F32:
		case VALUE_F64:
			return;

		case VALUE_INSTRUCTION: user_store = &value.instruction->users; break;
		case VALUE_BLOCK:       user_store = &value.block->users;       break;
		case VALUE_PROCEDURE:   user_store = &value.procedure->users;   break;
	}

	if (user_store->count == user_store->capacity) {
		user_store->capacity = next_pow2(user_store->capacity);
		user_store->instructions = realloc(user_store->instructions, sizeof(Instruction*)*user_store->count, sizeof(Instruction*)*user_store->capacity);
	}

	user_store->instructions[user_store->count++] = user;
}

Instruction* add_instruction(Block* block, Instruction inst) {
	Instruction* instruction = alloc(sizeof(Instruction));
	*instruction = inst;
	instruction->block = block;
	instruction->id = block->procedure->instruction_id_counter++;

	if (block->instruction_count == block->instruction_capacity) {
		assert(block->instruction_capacity);
		block->instruction_capacity <<= 2; // instruction_capacity = 16 on init
		block->instructions = realloc(
			block->instructions,
			sizeof(Instruction*) * block->instruction_count,
			sizeof(Instruction*) * block->instruction_capacity
		);
	}

	block->instructions[block->instruction_count++] = instruction;

	reg_user(instruction->a, instruction);
	reg_user(instruction->b, instruction);
	reg_user(instruction->c, instruction);

	return instruction;
}

void set_init_function(Function* func) {
	initproc = func->proc;
}

Instruction* convert_param(Block* block, Variable* var, s64 n) {
	Instruction* iparam = add_instruction(block, (Instruction){
		.kind = INSTRUCTION_PARAM,
		.a    = vint(n),
	});

	Instruction* istack = add_instruction(block, (Instruction){
		.kind = INSTRUCTION_STACK,
		.a    = vint(get_type_size(var->type)),
	});

	var->stack = istack;

	add_instruction(block, (Instruction){
		.kind = INSTRUCTION_STORE,
		.a    = vinst(istack),
		.b    = vinst(iparam),
	});

	return istack;
}

Value convert_expression(Expression* expr, Block* block) {
	switch (expr->kind) {
		case EXPR_NULL:  return vint(0);
		case EXPR_TRUE:  return vint(1);
		case EXPR_FALSE: return vint(0);

		case EXPR_LITERAL: {
			return vint(expr->term.token->i);
		} break;

		case EXPR_FUNCTION: {
			return vproc(expr->term.func->proc);
		} break;

		case EXPR_BASETYPE_PRIMITIVE:
		case EXPR_BASETYPE_IDENTIFIER:

		case EXPR_ARRAY:
		case EXPR_TUPLE: {
			assert(0);
		} break;

		case EXPR_IDENTIFIER_CONSTANT:
		case EXPR_IDENTIFIER_VARIABLE: {
			return vinst(expr->term.var->stack);
		} break;

		case EXPR_IDENTIFIER_FORMAL: {
			return vproc(expr->term.func->proc);
		} break;

		case EXPR_UNARY_ABS:
		case EXPR_UNARY_INVERSE:
		case EXPR_UNARY_NOT:
		case EXPR_UNARY_BIT_NOT:
		case EXPR_UNARY_PTR:
		case EXPR_UNARY_REF: {
			assert(0);
		} break;

		case EXPR_SPEC_PTR:
		case EXPR_SPEC_ARRAY:
		case EXPR_SPEC_FIXED: {
			// Interpreter?
		} break;

		case EXPR_BINARY_ADD:
		case EXPR_BINARY_SUB:
		case EXPR_BINARY_MUL:
		case EXPR_BINARY_DIV:
		case EXPR_BINARY_MOD:
		case EXPR_BINARY_BIT_XOR:
		case EXPR_BINARY_BIT_AND:
		case EXPR_BINARY_BIT_OR:
		case EXPR_BINARY_LSHIFT:
		case EXPR_BINARY_RSHIFT:
		case EXPR_BINARY_OR:
		case EXPR_BINARY_AND:
		case EXPR_BINARY_EQUAL:
		case EXPR_BINARY_NOT_EQUAL:
		case EXPR_BINARY_LESS:
		case EXPR_BINARY_LESS_OR_EQUAL:
		case EXPR_BINARY_GREATER:
		case EXPR_BINARY_GREATER_OR_EQUAL: {
			static const InstructionKind kindlut[][2] = {
				//                                 SIGNED               UNSIGNED
				[EXPR_BINARY_ADD]              = { INSTRUCTION_ADD,     INSTRUCTION_ADD     },
				[EXPR_BINARY_SUB]              = { INSTRUCTION_SUB,     INSTRUCTION_SUB     },
				[EXPR_BINARY_MUL]              = { INSTRUCTION_SMUL,    INSTRUCTION_UMUL    },
				[EXPR_BINARY_DIV]              = { INSTRUCTION_SDIV,    INSTRUCTION_UDIV    },
				[EXPR_BINARY_MOD]              = { INSTRUCTION_SMOD,    INSTRUCTION_UMOD    },
				[EXPR_BINARY_BIT_XOR]          = { INSTRUCTION_BIT_XOR, INSTRUCTION_BIT_XOR },
				[EXPR_BINARY_BIT_AND]          = { INSTRUCTION_BIT_AND, INSTRUCTION_BIT_AND },
				[EXPR_BINARY_BIT_OR]           = { INSTRUCTION_BIT_OR,  INSTRUCTION_BIT_OR  },
				[EXPR_BINARY_LSHIFT]           = { INSTRUCTION_SLSH,    INSTRUCTION_ULSH    },
				[EXPR_BINARY_RSHIFT]           = { INSTRUCTION_SRSH,    INSTRUCTION_URSH    },
				[EXPR_BINARY_OR]               = { INSTRUCTION_OR,      INSTRUCTION_OR      },
				[EXPR_BINARY_AND]              = { INSTRUCTION_AND,     INSTRUCTION_AND     },
				[EXPR_BINARY_EQUAL]            = { INSTRUCTION_CMPEQ,   INSTRUCTION_CMPEQ   },
				[EXPR_BINARY_NOT_EQUAL]        = { INSTRUCTION_CMPNE,   INSTRUCTION_CMPNE   },
				[EXPR_BINARY_LESS]             = { INSTRUCTION_SCMPL,   INSTRUCTION_UCMPL   },
				[EXPR_BINARY_LESS_OR_EQUAL]    = { INSTRUCTION_SCMPLE,  INSTRUCTION_UCMPLE  },
				[EXPR_BINARY_GREATER]          = { INSTRUCTION_SCMPG,   INSTRUCTION_UCMPG   },
				[EXPR_BINARY_GREATER_OR_EQUAL] = { INSTRUCTION_SCMPGE,  INSTRUCTION_UCMPGE  },
			};

			bool unsign = is_unsigned(expr->binary.left->type);

			Value vleft  = convert_expression(expr->binary.left, block);
			Value vright = convert_expression(expr->binary.right, block);

			Instruction* iadd = add_instruction(block, (Instruction){
				.kind = kindlut[expr->kind][unsign],
				.a    = vleft,
				.b    = vright,
			});

			return vinst(iadd);
		}

		case EXPR_BINARY_DOT: {
			// @Todo
		} break;

		case EXPR_BINARY_DOT_DOT:
		case EXPR_BINARY_SPAN: {
		} break;

		case EXPR_CALL: {
			Value vleft  = convert_expression(expr->call.function, block);

			for (u32 i = 0; i < expr->call.arg_count; i++) {
				Expression* arg = expr->call.args[i];
				convert_expression(arg, block);
			}

			Instruction* icall = add_instruction(block, (Instruction){
				.kind = INSTRUCTION_CALL,
				.a    = vleft,
			});

			return vinst(icall);
		} break;

		case EXPR_INDEX: {
			Value varray = convert_expression(expr->subscript.base, block);
			Value vindex = convert_expression(expr->subscript.index, block);
		} break;

		case EXPR_TERNARY_IF_ELSE: {
			assert(0);	
		} break;
	}

	assert(0);
	return NONE;
}

Value load(Block* block, Value v) {
	return vinst(
		add_instruction(block,
			(Instruction){
				.kind = INSTRUCTION_LOAD,
				.a = v,
			}
		)
	);
}

void store(Block* block, Value dest, Value v) {
	add_instruction(block,
		(Instruction){
			.kind = INSTRUCTION_STORE,
			.a = dest,
			.b = v,
		}
	);
}

Value convert_expression_deref(Expression* expr, Block* block) {
	Value v = convert_expression(expr, block);

	if (is_ref(expr->type)) {
		v = load(block, v);
	}

	return v;
}

void convert_statement(Statement* statement, Block* block) {
	switch (statement->kind) {
		case STATEMENT_ASSIGNMENT: {
			Value vleft  = convert_expression_deref(statement->assign.left, block);
			Value vright = convert_expression(statement->assign.right, block);
			store(block, vleft, vright);
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
			InstructionKind lut[] = {
				[STATEMENT_ASSIGNMENT_ADD]     = INSTRUCTION_ADD,
				[STATEMENT_ASSIGNMENT_SUB]     = INSTRUCTION_SUB,
				[STATEMENT_ASSIGNMENT_MUL]     = INSTRUCTION_SMUL,
				[STATEMENT_ASSIGNMENT_DIV]     = INSTRUCTION_SDIV,
				[STATEMENT_ASSIGNMENT_MOD]     = INSTRUCTION_SMOD,
				[STATEMENT_ASSIGNMENT_LSH]     = INSTRUCTION_SLSH,
				[STATEMENT_ASSIGNMENT_RSH]     = INSTRUCTION_SRSH,
				[STATEMENT_ASSIGNMENT_BIT_XOR] = INSTRUCTION_BIT_XOR,
				[STATEMENT_ASSIGNMENT_BIT_AND] = INSTRUCTION_BIT_AND,
				[STATEMENT_ASSIGNMENT_BIT_OR]  = INSTRUCTION_BIT_OR,
			};

			Value vleft  = convert_expression(statement->assign.left, block);
			Value v_deref_left = load(block, vleft);
			Value vright = convert_expression_deref(statement->assign.right, block);

			Value vres = vinst(add_instruction(block,
				(Instruction){
					.kind = lut[statement->kind],
					.a = v_deref_left,
					.b = vright,
				}
			));

			store(block, vleft, vres);
		} break;


		case STATEMENT_EXPRESSION: {
			convert_expression(statement->expr, block);
		} break;

		case STATEMENT_CONTROLFLOW: {
		} break;

		case STATEMENT_VARDECL: {
			Variable* var = statement->var;
			var->stack = add_instruction(block, (Instruction){
				.kind = INSTRUCTION_STACK,
				.a = get_type_size(var->type),
			});

			Value vinit = convert_expression_deref(statement->var->init_expr, block);

			store(block, vinst(var->stack), vinit);
		} break;

		case STATEMENT_RETURN: {
			Value retval = NONE;

			if (statement->ret.expr) {
				retval = convert_expression_deref(statement->ret.expr, block);
			}

			add_instruction(block, (Instruction){
				.kind = INSTRUCTION_RETURN,
			});
		} break;

		case STATEMENT_BREAK: {
		} break;

		case STATEMENT_CONTINUE: {
		} break;

		case STATEMENT_INC:
		case STATEMENT_DEC: {
			int dir = statement->kind == STATEMENT_INC ? 1 : -1;
			Value vexpr_ptr = convert_expression(statement->expr, block);
			Type* type = null;

			Instruction* iload = add_instruction(block, (Instruction){
				.kind = INSTRUCTION_LOAD,
				.a    = vexpr_ptr,
				.b    = vint(type->size),
			});

			Instruction* iadd = add_instruction(block, (Instruction){
				.kind = INSTRUCTION_ADD,
				.a    = vinst(iload),
				.b    = vint(dir),
			});

			add_instruction(block, (Instruction){
				.kind = INSTRUCTION_STORE,
				.a    = vexpr_ptr,
				.b    = vinst(iadd),
			});

		} break;
	}
}

void convert_code(Code* code, Block* block) {
	for (u32 i = 0; i < code->statement_count; i++) {
		Statement* statement = &code->statements[i];
		convert_statement(statement, block);
	}
}

// Probs need some params here to indicate the instance of the function? (Constant arguments)
void convert_function(Function* func) {
	Procedure* proc = func->proc;
	proc->entry = add_block(proc);

	for (u32 i = 0; i < func->param_count; i++) {
		Variable* var = &func->params[i];
		convert_param(proc->entry, var, i);
	}

	convert_code(&func->code, proc->entry);
}

void preconvert_module(Module* module) {
	for (u32 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		func->proc = make_procedure(func->name->string);
	}
}

void convert_module(Module* module) {
	for (u32 i = 0; i < module->function_count; i++) {
		Function* func = &module->functions[i];
		Procedure* proc = func->proc;
		proc->name = func->name->string;
		convert_function(func);
	}

	for (u32 i = 0; i < procedure_count; i++) {
		Procedure* proc = procedures[i];
		print_procedure(proc);
	}
}

void init_ir(void) {
	initproc = null;
	procedure_count = 0;
	procedure_capacity = 1024;
	procedures = alloc(sizeof(Procedure*)*procedure_capacity);
}

