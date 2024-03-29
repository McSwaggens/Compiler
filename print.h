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
static void errore(Expression* expr, u64 extra_lines, const char* format, ...);

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
	PRINT_ARG_F32,
	PRINT_ARG_F64,
	PRINT_ARG_CHAR,
	PRINT_ARG_STRING,
	PRINT_ARG_CSTRING,
	PRINT_ARG_TOKEN_KIND,
	PRINT_ARG_TOKEN, 
	PRINT_ARG_EXPRESSION,
	PRINT_ARG_STATEMENT,
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

		f32 f;
		f64 d;

		char c;
		char* s;

		String str;
		TokenKind token_kind;
		Token* token;
		Expression* expr;
		Statement* statement;
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
static FormatArg arg_f32(f32 v)                   { return (FormatArg){ PRINT_ARG_F32,        .f          = v }; }
static FormatArg arg_f64(f64 v)                   { return (FormatArg){ PRINT_ARG_F64,        .d          = v }; }
static FormatArg arg_char(char c)                 { return (FormatArg){ PRINT_ARG_CHAR,       .c          = c }; }
static FormatArg arg_string(String s)             { return (FormatArg){ PRINT_ARG_STRING,     .str        = s }; }
static FormatArg arg_cstring(char* s)             { return (FormatArg){ PRINT_ARG_CSTRING,    .s          = s }; }
static FormatArg arg_token(Token* token)          { return (FormatArg){ PRINT_ARG_TOKEN,      .token      = token}; }
static FormatArg arg_token_kind(TokenKind kind)   { return (FormatArg){ PRINT_ARG_TOKEN_KIND, .token_kind = kind }; }
static FormatArg arg_statement(Statement* stmt)   { return (FormatArg){ PRINT_ARG_STATEMENT,  .statement  = stmt }; }
static FormatArg arg_expression(Expression* expr) { return (FormatArg){ PRINT_ARG_EXPRESSION, .expr       = expr }; }
static FormatArg arg_type(TypeID type)            { return (FormatArg){ PRINT_ARG_TYPE,       .type       = type }; }

#endif // PRINT_H

