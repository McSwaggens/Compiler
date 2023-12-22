#ifndef LEXER_H
#define LEXER_H

typedef enum TokenKind TokenKind;
typedef union TokenAuxilaryInfo TokenAuxilaryInfo;
typedef struct Token    Token;
typedef struct Position Position;
typedef struct TokenAux TokenAux;
typedef struct Line     Line;
typedef u16 Indent16;
typedef u8  TokenFlags8;

enum TokenKind {
	TOKEN_EOF = 0,

	TOKEN_IDENTIFIER_VARIABLE,
	TOKEN_IDENTIFIER_FORMAL,
	TOKEN_IDENTIFIER_CONSTANT,

	TOKEN_LITERAL_INT8,
	TOKEN_LITERAL_INT16,
	TOKEN_LITERAL_INT32,
	TOKEN_LITERAL_INT64,
	TOKEN_LITERAL_UINT8,
	TOKEN_LITERAL_UINT16,
	TOKEN_LITERAL_UINT32,
	TOKEN_LITERAL_UINT64,
	TOKEN_LITERAL_FLOAT32,
	TOKEN_LITERAL_FLOAT64,
	TOKEN_LITERAL_STRING,

	TOKEN_IF,
	TOKEN_ELSE,
	TOKEN_THEN,
	TOKEN_FOR,
	TOKEN_WHILE,
	TOKEN_MATCH,
	TOKEN_BREAK,
	TOKEN_RETURN,
	TOKEN_CONTINUE,
	TOKEN_TRUE,
	TOKEN_FALSE,
	TOKEN_NULL,
	TOKEN_INC,
	TOKEN_DEC,

	TOKEN_BYTE,
	TOKEN_BOOL,
	TOKEN_INT,
	TOKEN_INT8,
	TOKEN_INT16,
	TOKEN_INT32,
	TOKEN_INT64,
	TOKEN_UINT,
	TOKEN_UINT8,
	TOKEN_UINT16,
	TOKEN_UINT32,
	TOKEN_UINT64,
	TOKEN_FLOAT32,
	TOKEN_FLOAT64,
	TOKEN_TYPE_ID,

	TOKEN_STRUCT,
	TOKEN_ENUM,

	// @Note: CLOSE = OPEN+1
	TOKEN_OPEN_BRACE,
	TOKEN_CLOSE_BRACE,
	TOKEN_OPEN_BRACKET,
	TOKEN_CLOSE_BRACKET,
	TOKEN_OPEN_PAREN,
	TOKEN_CLOSE_PAREN,

	TOKEN_BACK_SLASH,
	TOKEN_PIKE,
	TOKEN_COLON,
	TOKEN_SEMICOLON,
	TOKEN_DOT,
	TOKEN_DOT_DOT,
	TOKEN_ARROW,
	TOKEN_COMMA,
	TOKEN_PLUS,
	TOKEN_MINUS,
	TOKEN_ASTERISK,
	TOKEN_EXCLAMATION,
	TOKEN_SLASH,
	TOKEN_PERCENT,
	TOKEN_TILDA,
	TOKEN_BACKTICK,
	TOKEN_AT,
	TOKEN_HASH,
	TOKEN_DOLLAR,
	TOKEN_CARET,
	TOKEN_AMPERSAND,
	TOKEN_QUESTION,
	TOKEN_EQUAL,
	TOKEN_AND,
	TOKEN_OR,
	TOKEN_LEFT_SHIFT,
	TOKEN_RIGHT_SHIFT,

	TOKEN_LESS,
	TOKEN_LESS_OR_EQUAL,
	TOKEN_GREATER,
	TOKEN_GREATER_OR_EQUAL,

	TOKEN_LEFT_SHIFT_EQUAL,
	TOKEN_RIGHT_SHIFT_EQUAL,
	TOKEN_NOT_EQUAL,
	TOKEN_SLASH_EQUAL,
	TOKEN_PERCENT_EQUAL,
	TOKEN_PLUS_EQUAL,
	TOKEN_MINUS_EQUAL,
	TOKEN_ASTERISK_EQUAL,
	TOKEN_AMPERSAND_EQUAL,
	TOKEN_CARET_EQUAL,
	TOKEN_TILDA_EQUAL,
	TOKEN_PIKE_EQUAL,
};

struct Line {
	String string;
	Token* token_begin;
	Token* token_end;
};

struct Position {
	u64 line;   // Zero indexed
	u64 column; // Zero indexed
};

enum {
	NEWLINE = 0x01,
	LSPACED = 0x02,
	RSPACED = 0x04,
};

// Tokens are partially SOA'd to reduce size in cache.
struct Token {
	TokenKind kind : 8;
	TokenFlags8 flags;
	Indent16 indent;

	union {
		s64 i;
		f32 f;
		f64 d;
		String string;
		String identifier;
		struct {
			Token* closure; // Used as linked list while lexing
			u64 comma_count;
		};
	};
};

// TokenAux is for cold data only.
struct TokenAux {
	Position pos; // Only used when we're printing an error.
	u64 width;
};

static inline
bool is_newline(Token* token);

static inline
bool is_lspace(Token* token);

static inline
bool is_rspace(Token* token);

#endif // LEXER_H

