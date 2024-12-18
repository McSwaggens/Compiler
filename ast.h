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

typedef enum StatementKind  StatementKind;
typedef enum ExpressionKind ExpressionKind;
typedef enum BranchKind     BranchKind;
typedef enum ClauseKind     ClauseKind;
typedef enum AstKind        AstKind;

typedef enum ScopeFlags      ScopeFlags;
typedef enum VariableFlags   VariableFlags;
typedef enum ExpressionFlags ExpressionFlags;
typedef enum FunctionFlags   FunctionFlags;

#include "ir.h"

enum ExpressionFlags {
	EXPR_FLAG_CONSTANT = 0x01,
	EXPR_FLAG_REF      = 0x02,
	EXPR_FLAG_UNARY    = 0x04,
	EXPR_FLAG_BINARY   = 0x08,
	EXPR_FLAG_TERNARY  = 0x10,
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
	EXPR_FUNCTION, // (parser.c)term.token, (semantic.c)term.func
	EXPR_BASETYPE_PRIMITIVE,
	EXPR_BASETYPE_STRUCT,
	EXPR_BASETYPE_ENUM,
	EXPR_IDENTIFIER_CONSTANT,
	EXPR_IDENTIFIER_FORMAL,
	EXPR_IDENTIFIER_VARIABLE,
	EXPR_IDENTIFIER_STRUCT_FIELD,
	EXPR_ARRAY,                   // {a, b}
	EXPR_TUPLE,                   // (a, b)
	EXPR_SPEC_PTR,                //   *e
	EXPR_SPEC_ARRAY,              //  []e
	EXPR_SPEC_FIXED,              // [N]e
	EXPR_UNARY_PTR,               // *e
	EXPR_UNARY_REF,               // @e
	EXPR_UNARY_ABS,               // +e
	EXPR_UNARY_INVERSE,           // -e
	EXPR_UNARY_NOT,               // !e
	EXPR_UNARY_BIT_NOT,           // ~e
	EXPR_UNARY_IMPLICIT_CAST,

	EXPR_BINARY_ADD,              // a + b
	EXPR_BINARY_ADD_INDEX,        // p + i
	EXPR_BINARY_ADD_INT,          // i + i
	EXPR_BINARY_ADD_FP32,         // f + f
	EXPR_BINARY_ADD_FP64,         // f + f

	EXPR_BINARY_SUB,              // a - b
	EXPR_BINARY_SUB_PTR,          // p - p
	EXPR_BINARY_SUB_INT,          // i - i
	EXPR_BINARY_SUB_F32,          // f - f
	EXPR_BINARY_SUB_F64,          // f - f

	EXPR_BINARY_MUL,              // a * b
	EXPR_BINARY_DIV,              // a / b
	EXPR_BINARY_MOD,              // a \ b

	EXPR_BINARY_BIT_XOR,          // a ^ b
	EXPR_BINARY_BIT_AND,          // a & b
	EXPR_BINARY_BIT_OR,           // a | b

	EXPR_BINARY_OR,               // a || b
	EXPR_BINARY_AND,              // a && b
	EXPR_BINARY_EQUAL,            // a = b
	EXPR_BINARY_NOT_EQUAL,        // a != b
	EXPR_BINARY_LESS,             // a < b
	EXPR_BINARY_LESS_OR_EQUAL,    // a <= b
	EXPR_BINARY_GREATER,          // a > b
	EXPR_BINARY_GREATER_OR_EQUAL, // a >= b
	EXPR_BINARY_DOT,              // a.b
	EXPR_BINARY_DOT_DOT,          // a .. b
	EXPR_BINARY_LSHIFT,           // a << b
	EXPR_BINARY_RSHIFT,           // a >> b
	EXPR_BINARY_SPAN,             // [a..b]
	EXPR_CALL,                    // a()
	EXPR_INDEX,                   // a[b]
	EXPR_TERNARY_IF_ELSE,         // a if b else c
};

enum StatementKind {
	STATEMENT_EXPRESSION,  // e
	STATEMENT_CONTROLFLOW, // if, while, for, match
	STATEMENT_VARDECL,     // a : T = e
	STATEMENT_RETURN,      // return, return e
	STATEMENT_BREAK,       // break
	STATEMENT_CONTINUE,    // continue
	STATEMENT_INC,         // inc e
	STATEMENT_DEC,         // dec e
	STATEMENT_ASSIGNMENT,         // e = e
	STATEMENT_ASSIGNMENT_ADD,     // e += e
	STATEMENT_ASSIGNMENT_SUB,     // e -= e
	STATEMENT_ASSIGNMENT_MUL,     // e *= e
	STATEMENT_ASSIGNMENT_DIV,     // e /= e
	STATEMENT_ASSIGNMENT_MOD,     // e \= e
	STATEMENT_ASSIGNMENT_LSH,     // e <<= e
	STATEMENT_ASSIGNMENT_RSH,     // e >>= e
	STATEMENT_ASSIGNMENT_BIT_XOR, // e ^= e
	STATEMENT_ASSIGNMENT_BIT_AND, // e &= e
	STATEMENT_ASSIGNMENT_BIT_OR,  // e |= e
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

#define EXPRESSION_TABLE_INLINE_COUNT 2

typedef struct ExpressionTable ExpressionTable;

struct ExpressionTable {
	u64 count;

	union {
		Expression* inline_expressions[EXPRESSION_TABLE_INLINE_COUNT];
		Expression** external_expressions;
	};
};

static inline Expression** expr_table_get(ExpressionTable* table);
static inline ExpressionTable make_expr_table(u64 count);

enum AstKind {
	AST_STATEMENT,
	AST_EXPRESSION,
	AST_VARIABLE,
	AST_FUNCTION,
};

struct Expression {
	ExpressionKind kind;
	ExpressionFlags flags;
	TypeID type;
	Scope* scope;
	Value* value;

	union {
		Expression* subs[3];

		struct {
			Expression* left;
			Expression* right;
			Expression* middle;
			Token* optoken;
		};

		struct { Expression* function; ExpressionTable args; } call;

		struct { ExpressionTable elems; } tuple;
		struct { ExpressionTable elems; } array;

		struct { Expression* sub;  Expression* length; } specifier;
		struct { Expression* base; Expression* index;  } subscript;

		struct {
			union {
				Variable* var;
				Function* func;
				Struct* user_struct;
				Enum*   user_enum;
				StructField* struct_field;
				EnumField*   enum_field;
			};
			Token* token;
		} term;
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

