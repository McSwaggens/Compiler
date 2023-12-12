#ifndef AST_H
#define AST_H

#include "type.h"
#include "lexer.h"

typedef struct Module       Module;
typedef struct Function     Function;
typedef struct Code         Code;
typedef struct Scope        Scope;
typedef struct Expression   Expression;
typedef struct Statement    Statement;
typedef struct Assignment   Assignment;
typedef struct Variable     Variable;
typedef struct Branch       Branch;
typedef struct ControlFlow  ControlFlow;
typedef struct Return       Return;
typedef struct Break        Break;
typedef struct Struct       Struct;
typedef struct StructField  StructField;
typedef struct Enum         Enum;
typedef struct EnumField    EnumField;
typedef struct Match        Match;
typedef struct MatchGroup   MatchGroup;

typedef enum AstKind        AstKind;
typedef enum StatementKind  StatementKind;
typedef enum ExpressionKind ExpressionKind;
typedef enum BranchKind     BranchKind;
typedef enum ClauseKind     ClauseKind;

typedef enum ScopeFlags      ScopeFlags;
typedef enum VariableFlags   VariableFlags;
typedef enum ExpressionFlags ExpressionFlags;

typedef union Ast Ast;

enum AstKind {
	AST_INVALID = 0,

	AST_FUNCTION,
	AST_VARIABLE,

	AST_STRUCT,
	AST_STRUCT_MEMBER,

	AST_ENUM,
	AST_ENUM_MEMBER,

	AST_EXPR_NULL,
	AST_EXPR_TRUE,
	AST_EXPR_FALSE,
	AST_EXPR_LITERAL,
	AST_EXPR_FUNCTION,
	AST_EXPR_BASETYPE_PRIMITIVE,
	AST_EXPR_BASETYPE_IDENTIFIER,
	AST_EXPR_IDENTIFIER_CONSTANT,
	AST_EXPR_IDENTIFIER_FORMAL,
	AST_EXPR_IDENTIFIER_VARIABLE,
	AST_EXPR_ARRAY,
	AST_EXPR_TUPLE,
	AST_EXPR_SPEC_PTR,
	AST_EXPR_SPEC_ARRAY,
	AST_EXPR_SPEC_FIXED,
	AST_EXPR_UNARY_PTR,
	AST_EXPR_UNARY_REF,
	AST_EXPR_UNARY_ABS,
	AST_EXPR_UNARY_INVERSE,
	AST_EXPR_UNARY_NOT,
	AST_EXPR_UNARY_BIT_NOT,
	AST_EXPR_UNARY_IMPLICIT_CAST,
	AST_EXPR_BINARY_ADD,
	AST_EXPR_BINARY_SUB,
	AST_EXPR_BINARY_MUL,
	AST_EXPR_BINARY_DIV,
	AST_EXPR_BINARY_MOD,
	AST_EXPR_BINARY_BIT_XOR,
	AST_EXPR_BINARY_BIT_AND,
	AST_EXPR_BINARY_BIT_OR,
	AST_EXPR_BINARY_OR,
	AST_EXPR_BINARY_AND,
	AST_EXPR_BINARY_EQUAL,
	AST_EXPR_BINARY_NOT_EQUAL,
	AST_EXPR_BINARY_LESS,
	AST_EXPR_BINARY_LESS_OR_EQUAL,
	AST_EXPR_BINARY_GREATER,
	AST_EXPR_BINARY_GREATER_OR_EQUAL,
	AST_EXPR_BINARY_DOT,
	AST_EXPR_BINARY_DOT_DOT,
	AST_EXPR_BINARY_LSHIFT,
	AST_EXPR_BINARY_RSHIFT,
	AST_EXPR_BINARY_SPAN,
	AST_EXPR_CALL,
	AST_EXPR_INDEX,
	AST_EXPR_TERNARY_IF_ELSE,

	AST_STATEMENT_ASSIGNMENT,
	AST_STATEMENT_ASSIGNMENT_LSH,
	AST_STATEMENT_ASSIGNMENT_RSH,
	AST_STATEMENT_ASSIGNMENT_DIV,
	AST_STATEMENT_ASSIGNMENT_MOD,
	AST_STATEMENT_ASSIGNMENT_ADD,
	AST_STATEMENT_ASSIGNMENT_SUB,
	AST_STATEMENT_ASSIGNMENT_MUL,
	AST_STATEMENT_ASSIGNMENT_BIT_XOR,
	AST_STATEMENT_ASSIGNMENT_BIT_AND,
	AST_STATEMENT_ASSIGNMENT_BIT_OR,
	AST_STATEMENT_EXPRESSION,
	AST_STATEMENT_CONTROLFLOW,
	AST_STATEMENT_VARDECL,
	AST_STATEMENT_RETURN,
	AST_STATEMENT_BREAK,
	AST_STATEMENT_CONTINUE,
	AST_STATEMENT_INC,
	AST_STATEMENT_DEC,
};

struct StructField {
	AstKind kind : 8;
	Token* name;
	// TypeID type;
	Expression* type_expr;
};

struct Struct {
	AstKind kind : 8;
	Token* name;
	StructField* fields;
	u64 field_count;
};

struct EnumField {
	AstKind kind : 8;
	Token* name;
	Expression* value;
};

struct Enum {
	AstKind kind : 8;
	Token* name;
	Expression* type_expr;
	EnumField* fields;
	u64 field_count;
};

enum ScopeFlags {
	IS_CODE_FLAG = 0x01,
};

struct Scope {
	ScopeFlags flags;
	Scope* parent_scope;

	Variable** variables;
	u32 variable_count;
	u32 variable_capacity;
};

struct Code {
	Scope scope;

	Statement* statements;
	u32 statement_count;
	u32 statement_capacity;
};

struct Function {
	AstKind kind : 8;
	Token* name;
	Variable* params;
	u64 param_count;
	Expression* return_type_expr;
	TypeID type;
	Code code;
	struct Procedure* proc;
};

enum ExpressionFlags {
	EXPR_FLAG_CONSTANT = 0x01,
};

struct Expression {
	AstKind kind : 8;
	ExpressionFlags flags : 8;
	TypeID type;
	Ast* user;

	union {
		struct {
			Token* optoken;
			Expression* sub;
		} unary;

		struct {
			Token* optoken;
			Expression* left;
			Expression* right;
		} binary;

		struct {
			Token* optoken;
			Expression* left;
			Expression* middle;
			Expression* right;
		} ternary;

		struct {
			Expression* function;
			Expression** args;
			u64 arg_count;
		} call;

		struct {
			Expression** elems;
			u64 elem_count;
		} tuple;

		struct {
			Expression** elems;
			u64 elem_count;
		} array;

		struct {
			Expression* sub;
			Expression* length;
		} specifier;

		struct {
			Variable* var;
			Function* func;
			Token* token;
		} term;

		struct {
			Expression* base;
			Expression* index;
		} subscript;

		struct {
			Expression* left;
			Expression* right;
		} span;
	};

	Token* begin;
	Token* end;
};

struct Assignment {
	Token* token;
	Expression* left;
	Expression* right;
};

enum BranchKind {
	BRANCH_NAKED,
	BRANCH_IF,
	BRANCH_FOR,
	BRANCH_WHILE,
	BRANCH_MATCH,
};

enum ClauseKind {
	CLAUSE_INIT,
	CLAUSE_ELSE,
	CLAUSE_THEN,
};

struct MatchGroup {
	ClauseKind kind;
	Code code;
	Expression** exprs;
	u64 expr_count;
	MatchGroup* clause_then;
};

struct Match {
	MatchGroup* groups;
	u64 group_count;
};

struct Branch {
	AstKind kind : 8;
	Code code;
	BranchKind branch_kind;
	ClauseKind clause_kind;
	Variable* var;
	Expression* cond;
	Expression* new;
	Match match;
	Branch* belse;
	Branch* bthen;
};

struct ControlFlow {
	Branch* branches;
	u64 branch_count;
};

struct Return {
	Expression* expr;
};

struct Break {
};

struct Continue {
};

enum VariableFlags {
	VAR_VARIABLE = 0x1,
	VAR_CONSTANT = 0x2,
	VAR_GLOBAL   = 0x8,
	VAR_SCANNED = 0x10,
};

struct Variable {
	AstKind kind : 8;
	VariableFlags flags;
	Token* name;
	Expression* type_expr;
	Expression* init_expr;
	TypeID type;
	struct Instruction* stack;
};

struct Statement {
	AstKind kind : 8;

	union {
		Assignment assign;
		Expression* expr;
		ControlFlow controlflow;
		Variable* var;
		Return ret;
		Break brk;
	};
};

struct Module {
	// @Todo: imports

	Scope scope;

	String file;
	Token* tokens;
	Token* tokens_end;
	TokenAux* auxs;

	Function* functions;
	u64 function_count;

	Struct* structs;
	u64 struct_count;

	Enum* enums;
	u64 enum_count;

	char** usertype_names;
};

union Ast {
	AstKind     kind : 8;
	Function    func;
	Expression  expr;
	Statement   statement;
	Variable    var;
	Struct      ustruct;
	StructField ustruct_field;
	Enum        uenum;
	EnumField   uenum_field;
};

static Module*   find_module(Token* token);
static TokenAux* get_aux(Module* module, Token* token);
static Position  get_pos(Module* module, Token* token);

#endif // AST_H

