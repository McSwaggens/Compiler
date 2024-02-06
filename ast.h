#ifndef AST_H
#define AST_H

#include "type.h"
#include "lexer.h"
#include "mm.h"

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

typedef enum StatementKind  StatementKind;
typedef enum ExpressionKind ExpressionKind;
typedef enum BranchKind     BranchKind;
typedef enum ClauseKind     ClauseKind;

typedef enum ScopeFlags      ScopeFlags;
typedef enum VariableFlags   VariableFlags;
typedef enum ExpressionFlags ExpressionFlags;
typedef enum FunctionFlags FunctionFlags;

#include "ir.h"

enum ExpressionFlags {
	EXPR_FLAG_CONSTANT = 0x01,
	EXPR_FLAG_REF      = 0x02,
};

enum VariableFlags {
	VAR_FLAG_GLOBAL   = 0x01,
	VAR_FLAG_CONSTANT = 0x02,
	VAR_FLAG_PARAM    = 0x04,
};

enum FunctionFlags {
	FUNC_PLACEHOLDER = 0,
};

enum ExpressionKind {
	EXPR_NULL,
	EXPR_TRUE,
	EXPR_FALSE,
	EXPR_LITERAL,
	EXPR_FUNCTION,
	EXPR_BASETYPE_PRIMITIVE,
	EXPR_BASETYPE_IDENTIFIER,
	EXPR_IDENTIFIER_CONSTANT,
	EXPR_IDENTIFIER_FORMAL,
	EXPR_IDENTIFIER_VARIABLE,
	EXPR_ARRAY,
	EXPR_TUPLE,
	EXPR_SPEC_PTR,
	EXPR_SPEC_ARRAY,
	EXPR_SPEC_FIXED,
	EXPR_UNARY_PTR,
	EXPR_UNARY_REF,
	EXPR_UNARY_ABS,
	EXPR_UNARY_INVERSE,
	EXPR_UNARY_NOT,
	EXPR_UNARY_BIT_NOT,
	EXPR_UNARY_IMPLICIT_CAST,
	EXPR_BINARY_ADD,
	EXPR_BINARY_SUB,
	EXPR_BINARY_MUL,
	EXPR_BINARY_DIV,
	EXPR_BINARY_MOD,
	EXPR_BINARY_BIT_XOR,
	EXPR_BINARY_BIT_AND,
	EXPR_BINARY_BIT_OR,
	EXPR_BINARY_OR,
	EXPR_BINARY_AND,
	EXPR_BINARY_EQUAL,
	EXPR_BINARY_NOT_EQUAL,
	EXPR_BINARY_LESS,
	EXPR_BINARY_LESS_OR_EQUAL,
	EXPR_BINARY_GREATER,
	EXPR_BINARY_GREATER_OR_EQUAL,
	EXPR_BINARY_DOT,
	EXPR_BINARY_DOT_DOT,
	EXPR_BINARY_LSHIFT,
	EXPR_BINARY_RSHIFT,
	EXPR_BINARY_SPAN,
	EXPR_CALL,
	EXPR_INDEX,
	EXPR_TERNARY_IF_ELSE,
};

enum StatementKind {
	STATEMENT_ASSIGNMENT,
	STATEMENT_ASSIGNMENT_LSH,
	STATEMENT_ASSIGNMENT_RSH,
	STATEMENT_ASSIGNMENT_DIV,
	STATEMENT_ASSIGNMENT_MOD,
	STATEMENT_ASSIGNMENT_ADD,
	STATEMENT_ASSIGNMENT_SUB,
	STATEMENT_ASSIGNMENT_MUL,
	STATEMENT_ASSIGNMENT_BIT_XOR,
	STATEMENT_ASSIGNMENT_BIT_AND,
	STATEMENT_ASSIGNMENT_BIT_OR,
	STATEMENT_EXPRESSION,
	STATEMENT_CONTROLFLOW,
	STATEMENT_VARDECL,
	STATEMENT_RETURN,
	STATEMENT_BREAK,
	STATEMENT_CONTINUE,
	STATEMENT_INC,
	STATEMENT_DEC,
};

struct StructField {
	Token* name;
	TypeID type;
	Expression* type_expr;
};

struct Struct {
	Token* name;
	StructField* fields;
	u64 field_count;
};

struct EnumField {
	Token* name;
	Expression* value;
};

struct Enum {
	Token* name;
	Expression* type_expr;
	EnumField* fields;
	u64 field_count;
};

enum ScopeFlags {
	SCOPE_FLAG_GLOBAL      = 0x01,
	SCOPE_FLAG_CODE        = 0x02,
	SCOPE_FLAG_INSIDE_LOOP = 0x04,
};

struct Scope {
	ScopeFlags flags;
	Scope* parent_scope;

	Variable** variables;
	char** var_names;
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
	FunctionFlags flags;
	Token* name;
	Variable* params;
	u64 param_count;
	Expression* return_type_expr;
	TypeID type;
	Code code;
};

struct Expression {
	ExpressionKind kind;
	ExpressionFlags flags;
	TypeID type;
	Scope* scope;
	V32 value;

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
	Code code;
	BranchKind branch_kind;
	ClauseKind clause_kind;
	Variable* var;
	Expression* cond;
	Expression* nextval;
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

struct Variable {
	VariableFlags flags;
	Token* name;
	Expression* type_expr;
	Expression* init_expr;
	TypeID type;
	M32 stack;
};

struct Statement {
	StatementKind kind;

	union {
		Assignment assign;
		Expression* expr;
		ControlFlow controlflow;
		Variable* var;
		Return ret;
		Break brk;
	};
};

typedef struct ExpressionTable ExpressionTable;

struct ExpressionTable {
	Expression** expressions;
	u32 count;
	u32 capacity;
};

struct Module {
	// @Todo: imports

	Scope scope;

	String file;
	Token* tokens;
	Token* tokens_end;
	TokenAux* auxs;

	Line* lines;
	u64   line_count;

	Function* functions;
	u64 function_count;

	Struct* structs;
	u64 struct_count;

	Enum* enums;
	u64 enum_count;

	char** usertype_names;
};

static Module*   find_module(Token* token);
static TokenAux* get_aux(Module* module, Token* token);
static Position  get_pos(Module* module, Token* token);

#endif // AST_H

