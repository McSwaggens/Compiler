#ifndef GENERAL_H
#define GENERAL_H

// -------------------------------------------- //

typedef signed char      s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;

static const s64 INT8_MAX  = 0x7F;               // 127
static const s64 INT16_MAX = 0x7FFF;             // 32767
static const s64 INT32_MAX = 0x7FFFFFFF;         // 2147483647
static const s64 INT64_MAX = 0x7FFFFFFFFFFFFFFF; // 9223372036854775807

static const s64 INT8_MIN  = 0x80;                 // -128
static const s64 INT16_MIN = 0x8000;               // -32768
static const s64 INT32_MIN = 0x80000000;           // -2147483648
static const s64 INT64_MIN = 0x8000000000000000ll; // -9223372036854775808

static const s64 INT8_MAX_DIGITS  = 3;
static const s64 INT16_MAX_DIGITS = 5;
static const s64 INT32_MAX_DIGITS = 10;
static const s64 INT64_MAX_DIGITS = 19;

// -------------------------------------------- //

typedef unsigned char      u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

static const u64 UINT8_MIN  = 0;
static const u64 UINT16_MIN = 0;
static const u64 UINT32_MIN = 0;
static const u64 UINT64_MIN = 0;

static const u64 UINT8_MAX  = 0xFF;
static const u64 UINT16_MAX = 0xFFFF;
static const u64 UINT32_MAX = 0xFFFFFFFF;
static const u64 UINT64_MAX = 0xFFFFFFFFFFFFFFFFull;

static const s64 UINT8_MAX_DIGITS  = 3;
static const s64 UINT16_MAX_DIGITS = 5;
static const s64 UINT32_MAX_DIGITS = 10;
static const s64 UINT64_MAX_DIGITS = 20;

// ------------------------------------ //

typedef _Bool bool;
enum { false = 0, true = 1 };

// ------------------------------------ //

typedef float  float32;
typedef double float64;

static const float32 FLOAT32_INFINITY = __builtin_inff();
static const float32 FLOAT64_INFINITY = __builtin_inf();
static inline bool is_nan_f32(float32 f) { return *(u32*)&f > 0x7F800000; }
static inline bool is_nan_f64(float64 f) { return *(u64*)&f > 0x7F80000000000000; } // @Warning: Test me

// ------------------------------------ //

typedef char byte;
#define null ((void*)0)

// ------------------------------------ //

static void copy(void* to, const void* from, u64 size) { __builtin_memcpy(to, from, size); }
static void zero(void* p, u64 size) { __builtin_memset(p, 0, size); }
static bool compare(const void* a, const void* b, u64 count) { return __builtin_memcmp(a, b, count) == 0; }

// ------------------------------------ //

static inline s8  count_trailing_zeroes8(s8 n)   { return n == 0 ? 8  : __builtin_ctz((u32)n)-24; };
static inline s16 count_trailing_zeroes16(s16 n) { return n == 0 ? 16 : __builtin_ctz((u32)n)-16; };
static inline s32 count_trailing_zeroes32(s32 n) { return n == 0 ? 32 : __builtin_ctz(n); };
static inline s64 count_trailing_zeroes64(s64 n) { return n == 0 ? 64 : __builtin_ctzll(n); };

static inline s8  count_leading_zeroes8(s8 n)   { return n == 0 ? 8  : __builtin_clz((u32)n)-24; };
static inline s16 count_leading_zeroes16(s16 n) { return n == 0 ? 16 : __builtin_clz((u32)n)-16; };
static inline s32 count_leading_zeroes32(s32 n) { return n == 0 ? 32 : __builtin_clz(n); };
static inline s64 count_leading_zeroes64(s64 n) { return n == 0 ? 64 : __builtin_clzll(n); };

static inline s64 next_pow2(s64 n) { return 1llu << (64llu - count_leading_zeroes64(n)); }

static inline s64 boi(s64 n) { return 64-count_leading_zeroes64(n); }
static inline s64 bit_count(s64 n) { return __builtin_popcount(n); }

static inline bool is_pow2_or_zero(u64 n) {
	return (n & n-1) == 0;
}

static inline bool is_pow2(s64 n) {
	return bit_count(n) == 1;
}

static inline s64 round_pow2(s64 n) {
	if (bit_count(n) == 1)
		return n;

	return next_pow2(n);
}

static inline s64 round_to_nearest_mulpow2(s64 n, u64 pow2) {
	return (n + (pow2-1)) & ~(pow2-1);
}

// ------------------------------------ //

#define debug_break() __builtin_trap()
#define unreachable() __builtin_unreachable()
#define assert_unreachable() { error("Unreachable code reached.\n"); debug_break(); }

// ------------------------------------ //

typedef struct String {
	char* data;
	u64 length;
} String;

// ------------------------------------ //

typedef struct Stack {
	byte* head;
	byte* tail;
} Stack;

extern s64 system_call(s64 rax, s64 rsi, s64 rdi, s64 rdx, s64 r10, s64 r8, s64 r9);
extern u64 read_timestamp_counter(void);

// ------------------------------------ //

u64 count_cstring(const char* s);
static inline String get_string(char* s) { return (String){ s, count_cstring(s) }; }

// ------------------------------------ //

static void exit_program(void);
static void init_page_cache(void);

static inline u64 start_timer(void) { return read_timestamp_counter(); }
static inline u64 end_timer(u64 timer) { return read_timestamp_counter() - timer; }

// ------------------------------------ //

static inline bool is_upper(char c)   { return c >= 'A' && c <= 'Z'; }
static inline bool is_lower(char c)   { return c >= 'a' && c <= 'z'; }
static inline bool is_alpha(char c)   { return ((u8)c | 0x20u) - 0x61u < 26u; }
static inline bool is_digit(char c)   { return (u8)(c - '0') < 10u; }
static inline bool is_binary(char c)  { return (u8)(c - '0') < 2u; }
static inline bool is_decimal(char c) { return (u8)(c - '0') < 10u; }
static inline bool is_hex(char c)     { return (u8)(c - '0') < 10u || (u8)((c|0x20) - 'a') < 6u; }

#define STRX(x) #x
#define STR(x) STRX(x)
#ifdef DEBUG
#define assert(x) { if (!(x)) { print(__FILE__ ":" STR(__LINE__) ": error: assert in % tripped: " #x "\n", arg_cstring((char*)&(__func__[0]))); flush_output_buffer(&standard_output_buffer); debug_break(); } }
#else
#define assert(x)
#endif

#endif // GENERAL_H

