#ifndef IR_H
#define IR_H

#include "general.h"
#include "ast.h"

typedef enum InstructionKind InstructionKind;
typedef enum ValueKind       ValueKind;

typedef struct UserStore   UserStore;
typedef struct Value       Value;
typedef struct Procedure   Procedure;
typedef struct Instruction Instruction;
typedef struct Block       Block;

enum ValueKind {
	VALUE_NONE = 0,
	VALUE_INSTRUCTION,
	VALUE_BLOCK,
	VALUE_PROCEDURE,
	VALUE_INT,
	VALUE_F32,
	VALUE_F64,
};

struct Value {
	ValueKind kind;
	union {
		Instruction* instruction;
		Block* block;
		Procedure* procedure;
		s64   i;
		float32 f32;
		float64 f64;
	};
};

enum InstructionKind {
	INSTRUCTION_NOOP = 0,
	INSTRUCTION_STACK,
	INSTRUCTION_PARAM,
	INSTRUCTION_GLOBAL,

	INSTRUCTION_SELECT,
	INSTRUCTION_INDEX,

	INSTRUCTION_LOAD,
	INSTRUCTION_STORE,

	INSTRUCTION_CMPEQ,
	INSTRUCTION_CMPNE,
	INSTRUCTION_SCMPL,
	INSTRUCTION_SCMPLE,
	INSTRUCTION_SCMPG,
	INSTRUCTION_SCMPGE,
	INSTRUCTION_UCMPL,
	INSTRUCTION_UCMPLE,
	INSTRUCTION_UCMPG,
	INSTRUCTION_UCMPGE,

	INSTRUCTION_AND,
	INSTRUCTION_OR,

	INSTRUCTION_ADD,
	INSTRUCTION_SUB,
	INSTRUCTION_SMUL,
	INSTRUCTION_SDIV,
	INSTRUCTION_SMOD,
	INSTRUCTION_UMUL,
	INSTRUCTION_UDIV,
	INSTRUCTION_UMOD,

	INSTRUCTION_SLSH,
	INSTRUCTION_SRSH,

	INSTRUCTION_ULSH,
	INSTRUCTION_URSH,

	INSTRUCTION_BIT_NOT,
	INSTRUCTION_BIT_XOR,
	INSTRUCTION_BIT_AND,
	INSTRUCTION_BIT_OR,

	INSTRUCTION_F32_ADD,
	INSTRUCTION_F32_SUB,
	INSTRUCTION_F32_MUL,
	INSTRUCTION_F32_DIV,

	INSTRUCTION_F64_ADD,
	INSTRUCTION_F64_SUB,
	INSTRUCTION_F64_MUL,
	INSTRUCTION_F64_DIV,

	INSTRUCTION_PHI,

	INSTRUCTION_CALL,

	INSTRUCTION_JUMP,
	INSTRUCTION_BRANCH,
	INSTRUCTION_RETURN,
};

struct UserStore {
	Instruction** instructions;
	u32 count;
	u32 capacity;
};

struct Instruction {
	InstructionKind kind;
	Block* block;
	u32 id;
	Value a;
	Value b;
	Value c;
	UserStore users;
};

struct Block {
	Instruction** instructions;
	u32 instruction_count;
	u32 instruction_capacity;
	u32 id;
	Procedure* procedure;
	UserStore users;
};

struct Procedure {
	String name;
	Block* entry;
	Block** blocks;
	u64 block_count;
	u32 instruction_id_counter;

	UserStore users;
};

static void init_ir(void);
static void convert_function(Function* func);
static void preconvert_module(Module* module);
static void convert_module(Module* module);

#endif
