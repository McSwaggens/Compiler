#ifndef PRINT_H
#define PRINT_H

#include "general.h"
#include "ast.h"
#include "lexer.h"
#include "file.h"
#include "ir.h"

static void printbuff(OutputBuffer* buffer, const char* format, ...);
static void print(const char* format, ...);
static void error(String file, Position pos, const char* format, ...);
static void errort(Token* token, const char* format, ...);

typedef enum FormatArgumentKind {
	PRINT_ARG_BOOL,
	PRINT_ARG_INT8,
	PRINT_ARG_INT16,
	PRINT_ARG_INT32,
	PRINT_ARG_INT64,
	PRINT_ARG_UINT8,
	PRINT_ARG_UINT16,
	PRINT_ARG_UINT32,
	PRINT_ARG_UINT64,
	PRINT_ARG_FLOAT32,
	PRINT_ARG_FLOAT64,
	PRINT_ARG_CHAR,
	PRINT_ARG_STRING,
	PRINT_ARG_CSTRING,
	PRINT_ARG_TOKEN_KIND,
	PRINT_ARG_TOKEN, 
	PRINT_ARG_EXPRESSION,
	PRINT_ARG_STATEMENT,
	PRINT_ARG_AST,
	PRINT_ARG_AST_KIND,
	PRINT_ARG_VALUE,
	PRINT_ARG_TYPE,
} FormatArgumentKind;

typedef struct FormatArg {
	FormatArgumentKind kind;

	union {
		bool b;

		s8  s8;
		s16 s16;
		s32 s32;
		s64 s64;

		u8  u8;
		u16 u16;
		u32 u32;
		u64 u64;

		float32 f32;
		float32 f64;

		char c;
		char* s;

		String str;
		TokenKind token_kind;
		Token* token;
		Ast* ast;
		AstKind ast_kind;
		Expression* expr;
		Statement* statement;
		Value value;
		TypeID type;
	};
} FormatArg;

// Use _Generic from C11?
static FormatArg arg_bool(bool v)                 { return (FormatArg){ PRINT_ARG_BOOL,       .b          = v }; }
static FormatArg arg_s8(s8 v)                     { return (FormatArg){ PRINT_ARG_INT8,       .s8         = v }; }
static FormatArg arg_s16(s16 v)                   { return (FormatArg){ PRINT_ARG_INT16,      .s16        = v }; }
static FormatArg arg_s32(s32 v)                   { return (FormatArg){ PRINT_ARG_INT32,      .s32        = v }; }
static FormatArg arg_s64(s64 v)                   { return (FormatArg){ PRINT_ARG_INT64,      .s64        = v }; }
static FormatArg arg_u8(u8 v)                     { return (FormatArg){ PRINT_ARG_UINT8,      .u8         = v }; }
static FormatArg arg_u16(u16 v)                   { return (FormatArg){ PRINT_ARG_UINT16,     .u16        = v }; }
static FormatArg arg_u32(u32 v)                   { return (FormatArg){ PRINT_ARG_UINT32,     .u32        = v }; }
static FormatArg arg_u64(u64 v)                   { return (FormatArg){ PRINT_ARG_UINT64,     .u64        = v }; }
static FormatArg arg_f32(float32 v)               { return (FormatArg){ PRINT_ARG_FLOAT32,    .f32        = v }; }
static FormatArg arg_f64(float64 v)               { return (FormatArg){ PRINT_ARG_FLOAT64,    .f64        = v }; }
static FormatArg arg_char(char c)                 { return (FormatArg){ PRINT_ARG_CHAR,       .c          = c }; }
static FormatArg arg_string(String s)             { return (FormatArg){ PRINT_ARG_STRING,     .str        = s }; }
static FormatArg arg_cstring(char* s)             { return (FormatArg){ PRINT_ARG_CSTRING,    .s          = s }; }
static FormatArg arg_token(Token* token)          { return (FormatArg){ PRINT_ARG_TOKEN,      .token      = token}; }
static FormatArg arg_token_kind(TokenKind kind)   { return (FormatArg){ PRINT_ARG_TOKEN_KIND, .token_kind = kind }; }
static FormatArg arg_ast_kind(AstKind kind)       { return (FormatArg){ PRINT_ARG_AST_KIND,   .ast_kind   = kind }; }
static FormatArg arg_ast(Ast* ast)                { return (FormatArg){ PRINT_ARG_AST,        .ast        = ast }; }
static FormatArg arg_statement(Statement* stmt)   { return (FormatArg){ PRINT_ARG_STATEMENT,  .statement  = stmt }; }
static FormatArg arg_expression(Expression* expr) { return (FormatArg){ PRINT_ARG_EXPRESSION, .expr       = expr }; }
static FormatArg arg_value(Value value)           { return (FormatArg){ PRINT_ARG_VALUE,      .value      = value }; }
static FormatArg arg_type(TypeID type)            { return (FormatArg){ PRINT_ARG_TYPE,       .type       = type }; }

#endif // PRINT_H

