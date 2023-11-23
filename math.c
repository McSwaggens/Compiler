#ifndef MATH_C
#define MATH_C

static inline s64 max_s64(s64 a, s64 b) { return a >= b ? a : b; }
static inline s32 max_s32(s32 a, s32 b) { return a >= b ? a : b; }
static inline s16 max_s16(s16 a, s16 b) { return a >= b ? a : b; }
static inline s8  max_s8(s8 a, s8 b)    { return a >= b ? a : b; }

static inline s64 min_s64(s64 a, s64 b) { return a <= b ? a : b; }
static inline s32 min_s32(s32 a, s32 b) { return a <= b ? a : b; }
static inline s16 min_s16(s16 a, s16 b) { return a <= b ? a : b; }
static inline s8  min_s8(s8 a, s8 b)    { return a <= b ? a : b; }

static inline u64 max_u64(u64 a, u64 b) { return a >= b ? a : b; }
static inline u32 max_u32(u32 a, u32 b) { return a >= b ? a : b; }
static inline u16 max_u16(u16 a, u16 b) { return a >= b ? a : b; }
static inline u8  max_u8(u8 a,   u8 b)    { return a >= b ? a : b; }

static inline u64 min_u64(u64 a, u64 b) { return a <= b ? a : b; }
static inline u32 min_u32(u32 a, u32 b) { return a <= b ? a : b; }
static inline u16 min_u16(u16 a, u16 b) { return a <= b ? a : b; }
static inline u8  min_u8(u8 a, u8 b)    { return a <= b ? a : b; }

// ------------------------------------ //
static const float64 MATH_E = 2.71828182845904523542816810799394;

static float32 floor_f32(float32 f) {
	union { float32 f; u32 i; } u = { .f = f };
	u32 exponent = (u.i>>23u)&255u;
	s32 fractbits = 127 - exponent + 23;
	if (fractbits > 23) return 0.0;
	if (fractbits > 0)  u.i &= (-1u << fractbits);
	return u.f;
}

static float64 floor_f64(float64 f) {
	union { float64 f; u64 i; } u = { .f = f };
	u64 exponent = (u.i>>52ll)&2047ull;
	s64 fractbits = 1023ll - exponent + 52ll;
	if (fractbits > 52ll) return 0.0;
	if (fractbits > 0ll)  u.i &= (-1ull << fractbits);
	return u.f;
}

static inline s64 get_exponent_f32(float32 f) { return (*(u32*)&f >> 23) & 255; }
static inline s64 get_exponent_f64(float64 f) { return (*(u64*)&f >> 52llu) & 2047llu; }

static float32 left_shift_f32(float32 f, s64 n) {
	union { u32 i; float32 f; } u = { .f = f };
	u.i += n << 23;
	return u.f;
}

static float32 right_shift_f32(float32 f, s64 n) {
	union { u32 i; float32 f; } u = { .f = f };
	u.i -= n << 23;
	return u.f;
}

static float64 left_shift_f64(float64 f, s64 n) {
	union { u64 i; float64 f; } u = { .f = f };
	u.i += n << 52llu;
	return u.f;
}

static float64 right_shift_f64(float64 f, s64 n) {
	union { u64 i; float64 f; } u = { .f = f };
	u.i -= n << 52llu;
	return u.f;
}

static float64 get_fractional_part64(float64 f) { return f-floor_f64(f); }
static float32 get_fractional_part32(float32 f) { return f-floor_f32(f); }

static inline u64 pow10u(s64 n) {
	static const u64 lut[20] = {
		1llu,
		10llu,
		100llu,
		1000llu,
		10000llu,
		100000llu,
		1000000llu,
		10000000llu,
		100000000llu,
		1000000000llu,
		10000000000llu,
		100000000000llu,
		1000000000000llu,
		10000000000000llu,
		100000000000000llu,
		1000000000000000llu,
		10000000000000000llu,
		100000000000000000llu,
		1000000000000000000llu,
		10000000000000000000llu,
	};

	return lut[n];
}

static s64 log10(u64 n) {
	static const u64 pows[65] = {
		0, 0, 0, 0,
		1, 1, 1,
		2, 2, 2,
		3, 3, 3, 3,
		4, 4, 4,
		5, 5, 5,
		6, 6, 6, 6,
		7, 7, 7,
		8, 8, 8,
		9, 9, 9, 9,
		10, 10, 10,
		11, 11, 11, 12,
		12, 12, 12,
		13, 13, 13,
		14, 14, 14,
		15, 15, 15, 15,
		16, 16, 16,
		17, 17, 17,
		18, 18, 18, 18,
		19
	};

	u64 p = pows[boi(n)];
	return p + (n >= pow10u(p));
}

static u64 digit_count_base10(u64 n) { return n ? log10(n) : 1; }

#endif
