#ifndef PRINT_H
#define PRINT_H

#include "general.h"
#include "ast.h"
#include "lexer.h"
#include "file.h"

void printbuff(OutputBuffer* buffer, const char* format, ...);
void print(const char* format, ...);
void error(const char* format, ...);

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
	PRINT_ARG_EXPRESSION_KIND,
	PRINT_ARG_STATEMENT,
	PRINT_ARG_STATEMENT_KIND,
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
		Expression* expr;
		ExpressionKind expr_kind;
		Statement* statement;
		StatementKind statement_kind;
	};
} FormatArg;

FormatArg arg_bool(bool v)                                 { return (FormatArg){ PRINT_ARG_BOOL,            .b              = v }; }
FormatArg arg_s8(s8 v)                                     { return (FormatArg){ PRINT_ARG_INT8,            .s8             = v }; }
FormatArg arg_s16(s16 v)                                   { return (FormatArg){ PRINT_ARG_INT16,           .s16            = v }; }
FormatArg arg_s32(s32 v)                                   { return (FormatArg){ PRINT_ARG_INT32,           .s32            = v }; }
FormatArg arg_s64(s64 v)                                   { return (FormatArg){ PRINT_ARG_INT64,           .s64            = v }; }
FormatArg arg_u8(u8 v)                                     { return (FormatArg){ PRINT_ARG_UINT8,           .u8             = v }; }
FormatArg arg_u16(u16 v)                                   { return (FormatArg){ PRINT_ARG_UINT16,          .u16            = v }; }
FormatArg arg_u32(u32 v)                                   { return (FormatArg){ PRINT_ARG_UINT32,          .u32            = v }; }
FormatArg arg_u64(u64 v)                                   { return (FormatArg){ PRINT_ARG_UINT64,          .u64            = v }; }
FormatArg arg_f32(float32 v)                               { return (FormatArg){ PRINT_ARG_FLOAT32,         .f32            = v }; }
FormatArg arg_f64(float64 v)                               { return (FormatArg){ PRINT_ARG_FLOAT64,         .f64            = v }; }
FormatArg arg_char(char c)                                 { return (FormatArg){ PRINT_ARG_CHAR,            .c              = c }; }
FormatArg arg_string(String s)                             { return (FormatArg){ PRINT_ARG_STRING,          .str            = s }; }
FormatArg arg_cstring(char* s)                             { return (FormatArg){ PRINT_ARG_CSTRING,         .s              = s }; }
FormatArg arg_token(Token* token)                          { return (FormatArg){ PRINT_ARG_TOKEN,           .token          = token}; }
FormatArg arg_token_kind(TokenKind kind)                   { return (FormatArg){ PRINT_ARG_TOKEN_KIND,      .token_kind     = kind }; }
FormatArg arg_expression_kind(ExpressionKind kind)         { return (FormatArg){ PRINT_ARG_EXPRESSION_KIND, .expr_kind      = kind }; }
FormatArg arg_expression(Expression* expr)                 { return (FormatArg){ PRINT_ARG_EXPRESSION,      .expr           = expr }; }
FormatArg arg_statement(Statement* statement)              { return (FormatArg){ PRINT_ARG_STATEMENT,       .statement      = statement }; }
FormatArg arg_statement_kind(StatementKind statement_kind) { return (FormatArg){ PRINT_ARG_STATEMENT_KIND,  .statement_kind = statement_kind }; }


#endif // PRINT_H

