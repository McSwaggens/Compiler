#include "global_string_table.h"

typedef struct TokenStore {
	Token* tokens;
	u64 count;
} TokenStore;

typedef struct Lexer {
	char* cursor;
	Token* head;
	Token* tokens_begin;
	char* end;
	char* line_begin;
	u64 line;
	u16 indent;
	String file;
	Module* module;
} Lexer;

typedef enum Base {
	BASE_NONE = 0,
	BASE_BINARY = 2,
	BASE_DECIMAL = 10,
	BASE_HEXADECIMAL = 16,
} Base;

typedef enum LiteralFormat {
	LITERAL_FORMAT_UNSPECIFIED = 0,
	LITERAL_FORMAT_SIGNED_INTEGER,
	LITERAL_FORMAT_UNSIGNED_INTEGER,
	LITERAL_FORMAT_FLOAT,
} LiteralFormat;

typedef enum LiteralScalar {
	LITERAL_SCALAR_NONE = 00,
	LITERAL_SCALAR_K    = 10,
	LITERAL_SCALAR_M    = 20,
	LITERAL_SCALAR_G    = 30,
	LITERAL_SCALAR_T    = 40,
	LITERAL_SCALAR_P    = 50,
	LITERAL_SCALAR_E    = 60,
} LiteralScalar;

typedef struct LiteralQualifier {
	bool present;
	bool certain;
	Base base;
	LiteralFormat format;
	LiteralScalar scalar;
	u64 bits;
	char* end;
} LiteralQualifier;

typedef struct LiteralComponent {
	LiteralQualifier qualifier;
	Base base;
	u64 digits;
	char* begin;
	char* end;
} LiteralComponent;

static
Position make_pos(Lexer* lexer, char* p) {
	return (Position) {
		.line = lexer->line,
		.column = p-lexer->line_begin
	};
}

s64 decode_binary_literal_to_int(char* begin, char* end) {
	s64 result = 0;

	for (char* p = begin; p < end; p++) {
		if (*p == '_')
			continue;

		assert(is_binary(*p));

		result <<= 1;
		result |= (*p & 1);
	}

	return result;
}

s64 decode_decimal_literal_to_int(char* begin, char* end) {
	s64 result = 0;

	if (end-begin == 1)
		return *begin - '0';

	for (char* p = begin; p < end; p++) {
		if (*p == '_')
			continue;

		assert(is_decimal(*p));

		result *= 10;
		result += *p-'0';
	}

	return result;
}

s8 decode_digit(char c) {
	return (s8[256]){
		['0'] = 0,   ['1'] = 1,   ['2'] = 2,   ['3'] = 3,   ['4'] = 4,
		['5'] = 5,   ['6'] = 6,   ['7'] = 7,   ['8'] = 8,   ['9'] = 9,
		['a'] = 0xA, ['b'] = 0xB, ['c'] = 0xC, ['d'] = 0xD, ['e'] = 0xE, ['f'] = 0xF,
		['A'] = 0xA, ['B'] = 0xB, ['C'] = 0xC, ['D'] = 0xD, ['E'] = 0xE, ['F'] = 0xF,
	}[c];
}

s64 decode_hex_literal_to_int(char* begin, char* end) {
	s64 result = 0;

	if (end-begin == 1)
		return *begin - '0';

	for (char* p = begin; p < end; p++) {
		if (*p == '_')
			continue;

		assert(is_hex(*p));

		result <<= 4;
		result |= decode_digit(*p);
	}

	return result;
}

s64 decode_int(LiteralComponent whole, Base base) {
	switch (base) {
		case BASE_BINARY:       return decode_binary_literal_to_int(whole.begin, whole.end);
		case BASE_DECIMAL:      return decode_decimal_literal_to_int(whole.begin, whole.end);
		case BASE_HEXADECIMAL:  return decode_hex_literal_to_int(whole.begin, whole.end);
		default: assert(false); return 0;
	}
}

float64 decode_float(LiteralComponent whole, LiteralComponent fract, Base base) {
	float64 val = 0.0;
	float64 mul;
	float64 d;

	switch (base) {
		default: assert_unreachable();
		case BASE_BINARY:      mul =  2.0; d = 1.0/mul; break;
		case BASE_DECIMAL:     mul = 10.0; d = 1.0/mul; break;
		case BASE_HEXADECIMAL: mul = 16.0; d = 1.0/mul; break;
	}

	for (char* p = whole.begin; p < whole.end; p++) {
		if (*p == '_')
			continue;

		val = val*mul + decode_digit(*p);
	}

	mul = d;

	for (char* p = fract.begin; p < fract.end; p++) {
		if (*p == '_')
			continue;

		val += decode_digit(*p)*mul;
		mul *= d;
	}

	return val;
}

char* parse_literal_qualifier(char* p, LiteralQualifier* out) {
	LiteralQualifier qualifier = { 0 };
	char* backup = p;

	switch (*p) {
		case 'b':
			qualifier.base  = BASE_BINARY;
			qualifier.present = true;
			p++;
			break;

		case 'h':
			qualifier.base    = BASE_HEXADECIMAL;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		default:
			qualifier.base = BASE_DECIMAL;
			break;
	}

	switch (*p) {
		case 'f':
			qualifier.format  = LITERAL_FORMAT_FLOAT; qualifier.present = true;
			p++;
			break;

		case 'u':
			qualifier.format  = LITERAL_FORMAT_UNSIGNED_INTEGER;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		case 's':
			qualifier.format  = LITERAL_FORMAT_SIGNED_INTEGER;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		default:
			qualifier.format  = LITERAL_FORMAT_UNSPECIFIED;
			break;
	}

	switch (*p) {
		case 'k':
			qualifier.scalar  = LITERAL_SCALAR_K;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		case 'm':
			qualifier.scalar  = LITERAL_SCALAR_M;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		case 'g':
			qualifier.scalar  = LITERAL_SCALAR_G;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		case 't':
			qualifier.scalar  = LITERAL_SCALAR_T;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		case 'p':
			qualifier.scalar  = LITERAL_SCALAR_P;
			qualifier.present = true;
			qualifier.certain = true;
			p++;
			break;

		case 'e':
			qualifier.scalar  = LITERAL_SCALAR_E;
			qualifier.present = true;
			p++;
			break;

		default:
			qualifier.scalar = LITERAL_SCALAR_NONE;
			break;
	}

	if (qualifier.present && is_digit(*p)) {
		if (compare(p, "8", 1) && qualifier.format != LITERAL_FORMAT_FLOAT) {
			qualifier.bits = 8;
			p += 1;
		}
		else if (compare(p, "16", 2) && qualifier.format != LITERAL_FORMAT_FLOAT) {
			qualifier.bits = 16;
			p += 2;
		}
		else if (compare(p, "32", 2)) {
			qualifier.bits = 32;
			p += 2;
		}
		else if (compare(p, "64", 2)) {
			qualifier.bits = 64;
			p += 2;
		}
	}

	if (is_alpha(*p) || is_digit(*p) || *p == '_') {
		qualifier = (LiteralQualifier){ 0 };
		p = backup;
	}

	qualifier.end = p;
	*out = qualifier;
	return p;
}

LiteralComponent parse_component(char* p) {
	Base base = BASE_NONE;
	LiteralComponent component = { 0 };
	LiteralQualifier qualifier = { 0 };

	u64 unders = 0;

	component.begin = p;

	if (is_binary(*p)) {
		base = BASE_BINARY;
		for (; is_binary(*p) || *p == '_'; p++)
			if (*p == '_') unders++;
	}

	if (is_decimal(*p)) {
		base = BASE_DECIMAL;
		for (; is_decimal(*p) || *p == '_'; p++)
			if (*p == '_') unders++;
	}

	component.end = p;

	if (is_hex(*p)) {
		p = parse_literal_qualifier(p, &qualifier);

		if (!qualifier.present) {
			base = BASE_HEXADECIMAL;
			for (; is_hex(*p) || *p == '_'; p++)
				if (*p == '_') unders++;
			component.end = p;
		}
	}


	if (!qualifier.present)
		p = parse_literal_qualifier(p, &qualifier);

	component.digits = component.end-component.begin-unders;
	component.qualifier = qualifier;
	component.base = base;
	return component;
}

bool keep_fract(LiteralComponent whole, LiteralComponent fract) {
	if (is_digit(whole.end[1]))
		return true;

	if (fract.qualifier.certain)
		return true;

	return false;
}

String base_to_string(Base base) {
	static char* lut[] = {
		[BASE_NONE]        = "BASE_NONE",
		[BASE_BINARY]      = "BASE_BINARY",
		[BASE_DECIMAL]     = "BASE_DECIMAL",
		[BASE_HEXADECIMAL] = "BASE_HEXADECIMAL",
	};

	return tostr(lut[base]);
}

String format_to_string(LiteralFormat format) {
	static char* lut[] = {
		[LITERAL_FORMAT_UNSPECIFIED]      = "LITERAL_FORMAT_UNSPECIFIED",
		[LITERAL_FORMAT_FLOAT]            = "LITERAL_FORMAT_FLOAT",
		[LITERAL_FORMAT_SIGNED_INTEGER]   = "LITERAL_FORMAT_SIGNED_INTEGER",
		[LITERAL_FORMAT_UNSIGNED_INTEGER] = "LITERAL_FORMAT_UNSIGNED_INTEGER",
	};

	return tostr(lut[format]);
}

void print_qualifier(LiteralQualifier qualifier) {
	print("      present = %\n", arg_bool(qualifier.present));
	print("      certain = %\n", arg_bool(qualifier.certain));
	print("      base    = %\n", arg_string(base_to_string(qualifier.base)));
	print("      bits    = %\n", arg_u64(qualifier.bits));
	print("      format  = %\n", arg_string(format_to_string(qualifier.format)));
	print("      scalar  = %\n", arg_u64(qualifier.scalar));
}

void print_component(LiteralComponent comp) {
	print("    raw  = \"%\"\n", arg_string((String){ comp.begin, comp.end-comp.begin }));
	print("    base = %\n",     arg_string(base_to_string(comp.base)));
	print("    qualifier:\n");
	print_qualifier(comp.qualifier);
}

void parse_literal(Lexer* lexer) {
	Token* token = lexer->head;

	char* p = lexer->cursor;
	char* begin = p;
	Position pos = make_pos(lexer, begin);

	LiteralQualifier qualifier = { 0 };
	LiteralComponent whole = { 0 };
	LiteralComponent fract = { 0 };
	Base base = BASE_NONE;
	u64 bits = 0;
	LiteralFormat format = LITERAL_FORMAT_SIGNED_INTEGER;
	bool has_fract = false;

	whole = parse_component(p);
	qualifier = whole.qualifier;
	p = whole.end;
	base = whole.base;

	if (!whole.qualifier.certain && whole.end[0] == '.') {
		fract = parse_component(whole.end+1);
		has_fract = keep_fract(whole, fract);
	}

	if (has_fract) {
		format = LITERAL_FORMAT_FLOAT;
		qualifier = fract.qualifier;
		p = fract.end;
		base = max_u64(base, fract.base);
	}

	assert(base != BASE_NONE);

	if (qualifier.present) {
		bits = qualifier.bits;
		p = qualifier.end;

		if (qualifier.format)
			format = qualifier.format;

		if (format == LITERAL_FORMAT_FLOAT && bits == 0)
			bits = 64;

		if (format == LITERAL_FORMAT_FLOAT && bits < 32)
			error(lexer->file, pos, "Float qualifier with invalid size: %. Valid sizes are 32 and 64.\n", arg_u64(bits));

		if (base == BASE_HEXADECIMAL && qualifier.base != BASE_HEXADECIMAL)
			error(lexer->file, pos, "Hexadecimal with wrong base qualifier\n");

		if (base == BASE_DECIMAL && qualifier.base == BASE_BINARY)
			error(lexer->file, pos, "Binary literal with decimal digits.\n");

		if (has_fract && qualifier.format != LITERAL_FORMAT_UNSPECIFIED && qualifier.format != LITERAL_FORMAT_FLOAT)
			error(lexer->file, pos, "Invalid format qualifier for literal with fractional component. Expected 'f' format qualifier.\n");

		base = qualifier.base;
	}
	else {
		if (base == BASE_BINARY)
			base = BASE_DECIMAL;

		if (base == BASE_HEXADECIMAL)
			error(lexer->file, pos, "Hexadecimal missing 'h' qualifier\n");
	}

	if (!bits)
		bits = 64;

	if (format == LITERAL_FORMAT_FLOAT) {
		float64 value = decode_float(whole, fract, base);
		value *= 1llu << qualifier.scalar; // Not correct? Manually shift the bits in the mantissa?

		if (bits == 32) {
			token->kind = TOKEN_LITERAL_FLOAT32;
			token->aux.f32 = (float32)value;
		}
		else if (bits == 64) {
			token->kind = TOKEN_LITERAL_FLOAT64;
			token->aux.f64 = (float64)value;
		}
		else assert_unreachable();
	}
	else {
		bool sign = format == LITERAL_FORMAT_SIGNED_INTEGER;

		switch (bits) {
			default: assert_unreachable();
			case 8:  token->kind = sign ? TOKEN_LITERAL_INT8  : TOKEN_LITERAL_UINT8;
			case 16: token->kind = sign ? TOKEN_LITERAL_INT16 : TOKEN_LITERAL_UINT16;
			case 32: token->kind = sign ? TOKEN_LITERAL_INT32 : TOKEN_LITERAL_UINT32;
			case 64: token->kind = sign ? TOKEN_LITERAL_INT64 : TOKEN_LITERAL_UINT64;
		}

		s64 value = decode_int(whole, base);

		if (boi(value) + qualifier.scalar > bits)
			error(lexer->file, pos, "Scalar qualifier overflows %bit seger\n", arg_u64(bits));

		value <<= qualifier.scalar;

		token->aux.i = value;
	}

	String str = (String){ begin, p-begin };

	// print("Literal: %\n", arg_string(str));
	// print("  base   = %\n", arg_string(base_to_string(base)));
	// print("  format = %\n", arg_string(format_to_string(format)));
	// print("  whole:\n");
	// print_component(whole);
	// if (has_fract)
	// {
	// 	print("  fract:\n");
	// 	print_component(fract);
	// }
	// print("\n");

	if (is_alpha(*p) || *p == '_') {
		error(
			lexer->file, pos,
			"Unexpected character after literal %: '%'",
			arg_string(str), arg_char(*p)
		);
	}

	lexer->cursor = p;
}

bool test_if_hex(char* p) {
	while (is_hex(*p) || *p == '_') p++;

	if (*p == '.') {
		p++;
		while (is_hex(*p) || *p == '_') p++;
	}

	if (*p != 'h')
		return false;

	LiteralQualifier qualifier = { 0 };
	p = parse_literal_qualifier(p, &qualifier);

	if (!qualifier.present || qualifier.base != BASE_HEXADECIMAL)
		return false;

	if (is_alpha(*p) || is_digit(*p) || *p == '_')
		return false;

	return true;
}

void parse_identifier(Lexer* lexer) {
	Token* token = lexer->head;
	char* begin = lexer->cursor;
	bool begins_with_upper = is_upper(*begin);
	char* p = begin;

	enum {
		CFLAG_DIGIT = 1,
		CFLAG_LOWER = 2,
		CFLAG_UPPER = 4,
		CFLAG_UNDER = 8,
	};

	// Need to use a lut here because '_' is uppercase in ascii :(
	static const s8 lut[256] = {
		['0'] = CFLAG_DIGIT, ['1'] = CFLAG_DIGIT, ['2'] = CFLAG_DIGIT, ['3'] = CFLAG_DIGIT, ['4'] = CFLAG_DIGIT,
		['5'] = CFLAG_DIGIT, ['6'] = CFLAG_DIGIT, ['7'] = CFLAG_DIGIT, ['8'] = CFLAG_DIGIT, ['9'] = CFLAG_DIGIT,

		['a'] = CFLAG_LOWER, ['b'] = CFLAG_LOWER, ['c'] = CFLAG_LOWER, ['d'] = CFLAG_LOWER, ['e'] = CFLAG_LOWER, ['f'] = CFLAG_LOWER,
		['g'] = CFLAG_LOWER, ['h'] = CFLAG_LOWER, ['i'] = CFLAG_LOWER, ['j'] = CFLAG_LOWER, ['k'] = CFLAG_LOWER, ['l'] = CFLAG_LOWER,
		['m'] = CFLAG_LOWER, ['n'] = CFLAG_LOWER, ['o'] = CFLAG_LOWER, ['p'] = CFLAG_LOWER, ['q'] = CFLAG_LOWER, ['r'] = CFLAG_LOWER,
		['s'] = CFLAG_LOWER, ['t'] = CFLAG_LOWER, ['u'] = CFLAG_LOWER, ['v'] = CFLAG_LOWER, ['w'] = CFLAG_LOWER, ['x'] = CFLAG_LOWER,
		['y'] = CFLAG_LOWER, ['z'] = CFLAG_LOWER,

		['A'] = CFLAG_UPPER, ['B'] = CFLAG_UPPER, ['C'] = CFLAG_UPPER, ['D'] = CFLAG_UPPER, ['E'] = CFLAG_UPPER, ['F'] = CFLAG_UPPER,
		['G'] = CFLAG_UPPER, ['H'] = CFLAG_UPPER, ['I'] = CFLAG_UPPER, ['J'] = CFLAG_UPPER, ['K'] = CFLAG_UPPER, ['L'] = CFLAG_UPPER,
		['M'] = CFLAG_UPPER, ['N'] = CFLAG_UPPER, ['O'] = CFLAG_UPPER, ['P'] = CFLAG_UPPER, ['Q'] = CFLAG_UPPER, ['R'] = CFLAG_UPPER,
		['S'] = CFLAG_UPPER, ['T'] = CFLAG_UPPER, ['U'] = CFLAG_UPPER, ['V'] = CFLAG_UPPER, ['W'] = CFLAG_UPPER, ['X'] = CFLAG_UPPER,
		['Y'] = CFLAG_UPPER, ['Z'] = CFLAG_UPPER,

		['_'] = CFLAG_UNDER,
	};

	s8 mask = 0;
	while (lut[*p])
		mask |= lut[*p++];

	bool contains_lower = (mask & CFLAG_LOWER);
	u64 length = p - begin;
	char* s = gst_lookup(begin, length);

	token->kind = TOKEN_IDENTIFIER_VARIABLE;

	if (begins_with_upper) {
		token->kind = TOKEN_IDENTIFIER_CONSTANT;

		if (contains_lower) {
			token->kind = TOKEN_IDENTIFIER_FORMAL;
			lexer->module->function_count++; // Overkill. Only do when unique?
		}
	}

	token->aux.string = (String){ s, length };

	lexer->cursor += length;
}

static
bool test_keyword(Lexer* lexer, const char* keyword, TokenKind kind) {
	Token* token = lexer->head;
	u64 length = count_cstring(keyword);

	if (!compare(lexer->cursor, keyword, length))
		return false;

	char c = lexer->cursor[length];
	if (is_alpha(c) || is_digit(c) || c == '_')
		return false;

	token->kind = kind;

	lexer->cursor += length;

	return true;
}

void parse_string(Lexer* lexer) {
	Token* token = lexer->head;
	char* begin = lexer->cursor;
	char* p = begin+1;
	s64 subs = 0;

	assert(*begin == '"');

	token->kind = TOKEN_LITERAL_STRING;

	for (; p < lexer->end && *p != '"'; p++) {
		if (*p != '\\')
			continue;

		switch (*p) {
			default:
				continue;

			case '0':
			case 'a':
			case 'b':
			case 't':
			case 'n':
			case 'v':
			case 'f':
			case 'r':
			case '\"':
			case '\\':
				subs++;
				p++;
				break;
		}
	}

	u64 input_numchars  = p-(begin+1);
	u64 result_numchars = input_numchars-subs;

	if (p == lexer->end)
		error(lexer->file, token->pos, "String not terminated.\n");

	token->aux.string = (String){ begin+1, result_numchars };

	// @todo @bug: Handle multiline strings
	if (subs) {
		char* s = alloc(result_numchars);
		char* p = begin+1;

		for (u64 i = 0, j = 0; i < input_numchars; i++, j++) {
			s[j] = p[i];

			if (p[i] != '\\')
				continue;

			switch (p[i+1]) {
				default: continue;
				case '0':  s[j] = '\0'; i++; break;
				case 'a':  s[j] = '\a'; i++; break;
				case 'b':  s[j] = '\b'; i++; break;
				case 't':  s[j] = '\t'; i++; break;
				case 'n':  s[j] = '\n'; i++; break;
				case 'v':  s[j] = '\v'; i++; break;
				case 'f':  s[j] = '\f'; i++; break;
				case 'r':  s[j] = '\r'; i++; break;
				case '\"': s[j] = '\"'; i++; break;
				case '\\': s[j] = '\\'; i++; break;
			}
		}

		token->aux.string = (String){ s, result_numchars };
	}

	lexer->cursor = p;
}

bool is_whitespace(char c) {
	switch (c) {
		case ' ':
		case '\t':
		case '\v':
		case '\n':
		case '\r':
			return true;

		default:
			return false;
	}
}

void skip_whitespace(Lexer* lexer) {
	Token* token = lexer->head;
	char* p = lexer->cursor;
	char* before = p;
	bool newline = false;

	while (is_whitespace(*p) || compare(p, "//", 2)) {
		if (*p == '\n') {
			lexer->line_begin = p+1;
			lexer->line++;
			lexer->indent = 0;
			newline = true;

			for (p++; *p == '\t'; lexer->indent++, p++);
		}
		else if (compare(p, "//", 2)) {
			for (p += 2; p < lexer->end && *p != '\n'; p++);
		}
		else p++;
	}

	bool spaced = (p != before);

	token->lspace = spaced;
	token->newline = newline;
	token->indent = lexer->indent;

	if (lexer->head != lexer->tokens_begin)
		lexer->head[-1].rspace = spaced;

	lexer->cursor = p;
}

void lex(Module* module) {
	FileHandle32 file_handle = open_file(module->file, FILE_MODE_OPEN, FILE_ACCESS_FLAG_READ);
	FileData file = load_file(file_handle, 16);
	close_file(file_handle);

	print("Lexing file \"%\":\n\n%\n",
		arg_string(module->file),
		arg_string((String){ file.data, file.size })
	);

	// u64 start_timer = read_timestamp_counter();

	Token* tokens = alloc((file.size + 1) * sizeof(Token));
	module->tokens = tokens;

	Lexer lexer = {
		.cursor       = file.data,
		.head         = tokens,
		.tokens_begin = tokens,
		.end          = file.data + file.size,
		.indent       = 0,
		.line         = 0,
		.line_begin   = file.data,
		.file         = module->file,
		.module       = module,
	};

	while (*lexer.cursor == '\t') lexer.cursor++, lexer.indent++;

	while (lexer.cursor < lexer.end) {
		skip_whitespace(&lexer);

		if (lexer.cursor >= lexer.end) break;

		lexer.head->pos = make_pos(&lexer, lexer.cursor);

		switch (*lexer.cursor) {
			case 'a': {
				goto GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER;
			}

			case 'b': {
				if (test_keyword(&lexer, "byte",    TOKEN_BYTE))  break;
				if (test_keyword(&lexer, "bool",    TOKEN_BOOL))  break;
				if (test_keyword(&lexer, "break",   TOKEN_BREAK)) break;
				goto GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER;
			}

			case 'c': {
				if (test_keyword(&lexer, "continue",    TOKEN_CONTINUE))  break;
				goto GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER;
			}

			case 'd': {
				if (test_keyword(&lexer, "dec",    TOKEN_DEC))  break;
				goto GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER;
			}

			case 'e': {
				if (test_keyword(&lexer, "else",    TOKEN_ELSE)) break;
				if (test_keyword(&lexer, "enum",    TOKEN_ENUM)) {
					module->enum_count++;
					break;
				}
				goto GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER;
			}

			case 'f': {
				if (test_keyword(&lexer, "for",     TOKEN_FOR))     break;
				if (test_keyword(&lexer, "false",   TOKEN_FALSE))   break;
				if (test_keyword(&lexer, "float32", TOKEN_FLOAT32)) break;
				if (test_keyword(&lexer, "float64", TOKEN_FLOAT64)) break;
				goto GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER;
			}

			case 'A': case 'B': case 'C':
			case 'D': case 'E': case 'F':
			GOTO_LEXER_COULD_BE_HEXADECIMAL_OR_IDENTIFIER: {
				if (!test_if_hex(lexer.cursor))
					goto GOTO_PARSE_IDENTIFIER;
			}

			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9': {
				parse_literal(&lexer);
			} break;

			case 'i':
				if (test_keyword(&lexer, "if",      TOKEN_IF))    break;
				if (test_keyword(&lexer, "inc",     TOKEN_INC))   break;
				if (test_keyword(&lexer, "int",     TOKEN_INT))   break;
				if (test_keyword(&lexer, "int8",    TOKEN_INT8))  break;
				if (test_keyword(&lexer, "int16",   TOKEN_INT16)) break;
				if (test_keyword(&lexer, "int32",   TOKEN_INT32)) break;
				if (test_keyword(&lexer, "int64",   TOKEN_INT64)) break;
				goto GOTO_PARSE_IDENTIFIER;

			case 'm':
				if (test_keyword(&lexer, "match",   TOKEN_MATCH)) break;
				goto GOTO_PARSE_IDENTIFIER;

			case 'o':
				goto GOTO_PARSE_IDENTIFIER;

			case 'q':
				goto GOTO_PARSE_IDENTIFIER;

			case 'r':
				if (test_keyword(&lexer, "return", TOKEN_RETURN)) break;
				goto GOTO_PARSE_IDENTIFIER;

			case 's':
				if (test_keyword(&lexer, "struct",  TOKEN_STRUCT)) {
					module->struct_count++;
					break;
				}
				goto GOTO_PARSE_IDENTIFIER;

			case 't':
				if (test_keyword(&lexer, "then",    TOKEN_THEN))    break;
				if (test_keyword(&lexer, "true",    TOKEN_TRUE))    break;
				if (test_keyword(&lexer, "type_id", TOKEN_TYPE_ID)) break;
				goto GOTO_PARSE_IDENTIFIER;

			case 'u':
				if (test_keyword(&lexer, "uint",    TOKEN_UINT))   break;
				if (test_keyword(&lexer, "uint8",   TOKEN_UINT8))  break;
				if (test_keyword(&lexer, "uint16",  TOKEN_UINT16)) break;
				if (test_keyword(&lexer, "uint32",  TOKEN_UINT32)) break;
				if (test_keyword(&lexer, "uint64",  TOKEN_UINT64)) break;
				goto GOTO_PARSE_IDENTIFIER;

			case 'w':
				if (test_keyword(&lexer, "while",   TOKEN_WHILE)) break;
				goto GOTO_PARSE_IDENTIFIER;

			case 'G': case 'H': case 'I': case 'J': case 'K':
			case 'L': case 'M': case 'N': case 'O': case 'P':
			case 'Q': case 'R': case 'S': case 'T': case 'U':
			case 'V': case 'W': case 'X': case 'Y': case 'Z':
			case 'g': case 'j': case 'k': case 'l' :case 'n':
			case 'h': case 'p': case 'v': case 'x': case 'y':
			case 'z':
			GOTO_PARSE_IDENTIFIER: {
				parse_identifier(&lexer);
			} break;

			case '!':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_NOT_EQUAL;         lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_EXCLAMATION;       lexer.cursor += 1; break; }

			case '%':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_PERCENT_EQUAL;     lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_PERCENT;           lexer.cursor += 1; break; }

			case '&':
				if (lexer.cursor[1] == '&')          { lexer.head->kind = TOKEN_AND;               lexer.cursor += 2; break; }
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_AMPERSAND_EQUAL;   lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_AMPERSAND;         lexer.cursor += 1; break; }

			case '+':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_PLUS_EQUAL;        lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_PLUS;              lexer.cursor += 1; break; }

			case '-':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_MINUS_EQUAL;       lexer.cursor += 2; break; }
				if (lexer.cursor[1] == '>')          { lexer.head->kind = TOKEN_ARROW;             lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_MINUS;             lexer.cursor += 1; break; }

			case '*':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_ASTERISK_EQUAL;    lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_ASTERISK;          lexer.cursor += 1; break; }

			case '/':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_SLASH_EQUAL;       lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_SLASH;             lexer.cursor += 1; break; }

			case '^':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_CARET_EQUAL;       lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_CARET;             lexer.cursor += 1; break; }

			case '~':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_TILDA_EQUAL;       lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_TILDA;             lexer.cursor += 1; break; }

			case '|':
				if (lexer.cursor[1] == '|')          { lexer.head->kind = TOKEN_OR;                lexer.cursor += 2; break; }
				else if (lexer.cursor[1] == '=')     { lexer.head->kind = TOKEN_PIKE_EQUAL;        lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_PIKE;              lexer.cursor += 1; break; }

			case '<':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_LESS_OR_EQUAL;     lexer.cursor += 2; break; }
				if (compare(lexer.cursor, "<<=", 3)) { lexer.head->kind = TOKEN_LEFT_SHIFT_EQUAL;  lexer.cursor += 3; break; }
				if (lexer.cursor[1] == '<')          { lexer.head->kind = TOKEN_LEFT_SHIFT;        lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_LESS;              lexer.cursor += 1; break; }

			case '>':
				if (lexer.cursor[1] == '=')          { lexer.head->kind = TOKEN_GREATER_OR_EQUAL;  lexer.cursor += 2; break; }
				if (compare(lexer.cursor, ">>=", 3)) { lexer.head->kind = TOKEN_RIGHT_SHIFT_EQUAL; lexer.cursor += 3; break; }
				if (lexer.cursor[1] == '>')          { lexer.head->kind = TOKEN_RIGHT_SHIFT;       lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_GREATER;           lexer.cursor += 1; break; }

			case '.':
				if (lexer.cursor[1] == '.')          { lexer.head->kind = TOKEN_DOT_DOT;           lexer.cursor += 2; break; }
				                                     { lexer.head->kind = TOKEN_DOT;               lexer.cursor += 1; break; }

			case '\\':                               { lexer.head->kind = TOKEN_BACK_SLASH;        lexer.cursor += 1; break; }
			case ',':                                { lexer.head->kind = TOKEN_COMMA;             lexer.cursor += 1; break; }
			case ':':                                { lexer.head->kind = TOKEN_COLON;             lexer.cursor += 1; break; }
			case ';':                                { lexer.head->kind = TOKEN_SEMICOLON;         lexer.cursor += 1; break; }
			case '=':                                { lexer.head->kind = TOKEN_EQUAL;             lexer.cursor += 1; break; }
			case '?':                                { lexer.head->kind = TOKEN_QUESTION;          lexer.cursor += 1; break; }
			case '@':                                { lexer.head->kind = TOKEN_AT;                lexer.cursor += 1; break; }
			case '#':                                { lexer.head->kind = TOKEN_HASH;              lexer.cursor += 1; break; }
			case '$':                                { lexer.head->kind = TOKEN_DOLLAR;            lexer.cursor += 1; break; }
			case '`':                                { lexer.head->kind = TOKEN_BACKTICK;          lexer.cursor += 1; break; }
			case '[':                                { lexer.head->kind = TOKEN_OPEN_BRACKET;      lexer.cursor += 1; break; }
			case ']':                                { lexer.head->kind = TOKEN_CLOSE_BRACKET;     lexer.cursor += 1; break; }
			case '{':                                { lexer.head->kind = TOKEN_OPEN_BRACE;        lexer.cursor += 1; break; }
			case '}':                                { lexer.head->kind = TOKEN_CLOSE_BRACE;       lexer.cursor += 1; break; }
			case '(':                                { lexer.head->kind = TOKEN_OPEN_PAREN;        lexer.cursor += 1; break; }
			case ')':                                { lexer.head->kind = TOKEN_CLOSE_PAREN;       lexer.cursor += 1; break; }

			case '"': {
				parse_string(&lexer);
			} break;

			case '\'':
			default:
				error(module->file, make_pos(&lexer, lexer.cursor), "Unexepected character: '%'\n", arg_char(*lexer.cursor));
		}

		lexer.head++;
	}

	*lexer.head = (Token){
		.kind = TOKEN_EOF,
		.pos = make_pos(&lexer, lexer.cursor),
		.indent = 0,
		.newline = true,
	};

	print("Tokens:");
	for (Token* token = lexer.tokens_begin; token < lexer.head+1; token++) {
		print("%| % | %\n", arg_cstring(token->newline ? "\n--" : "  "), arg_u16(token->indent), arg_token(token));
	}
	print("\n");

	// u64 end_timer = read_timestamp_counter();
	// print("Lexer took % cycles.\n",
	// 	arg_u64(end_timer-start_timer)
	// );
}
