/*
 * field.c - secp256k1 field arithmetic implementation
 *
 * Uses 4 x 64-bit limb representation with special reduction for secp256k1.
 * The prime p = 2^256 - 2^32 - 977 allows efficient reduction.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "field.h"

/* Field prime p = 2^256 - 2^32 - 977 */
const fe_t FE_P = {{
    0xFFFFFFFEFFFFFC2FULL,
    0xFFFFFFFFFFFFFFFFULL,
    0xFFFFFFFFFFFFFFFFULL,
    0xFFFFFFFFFFFFFFFFULL
}};

const fe_t FE_ZERO = {{0, 0, 0, 0}};
const fe_t FE_ONE = {{1, 0, 0, 0}};

/* Helper: add with carry */
VANITY_INLINE uint64_t adc(uint64_t a, uint64_t b, uint64_t *carry) {
    __uint128_t sum = (__uint128_t)a + b + *carry;
    *carry = (uint64_t)(sum >> 64);
    return (uint64_t)sum;
}

/* Helper: subtract with borrow */
VANITY_INLINE uint64_t sbb(uint64_t a, uint64_t b, uint64_t *borrow) {
    __uint128_t diff = (__uint128_t)a - b - *borrow;
    *borrow = (diff >> 64) ? 1 : 0;
    return (uint64_t)diff;
}

/* Helper: multiply with carry */
VANITY_INLINE uint64_t mac(uint64_t a, uint64_t b, uint64_t c, uint64_t *carry) {
    __uint128_t prod = (__uint128_t)a * b + c + *carry;
    *carry = (uint64_t)(prod >> 64);
    return (uint64_t)prod;
}

VANITY_INLINE void fe_from_bytes(fe_t *r, const uint8_t b[32]) {
    /* Big-endian to little-endian limbs */
    r->d[3] = ((uint64_t)b[0]  << 56) | ((uint64_t)b[1]  << 48) |
              ((uint64_t)b[2]  << 40) | ((uint64_t)b[3]  << 32) |
              ((uint64_t)b[4]  << 24) | ((uint64_t)b[5]  << 16) |
              ((uint64_t)b[6]  << 8)  | ((uint64_t)b[7]);
    r->d[2] = ((uint64_t)b[8]  << 56) | ((uint64_t)b[9]  << 48) |
              ((uint64_t)b[10] << 40) | ((uint64_t)b[11] << 32) |
              ((uint64_t)b[12] << 24) | ((uint64_t)b[13] << 16) |
              ((uint64_t)b[14] << 8)  | ((uint64_t)b[15]);
    r->d[1] = ((uint64_t)b[16] << 56) | ((uint64_t)b[17] << 48) |
              ((uint64_t)b[18] << 40) | ((uint64_t)b[19] << 32) |
              ((uint64_t)b[20] << 24) | ((uint64_t)b[21] << 16) |
              ((uint64_t)b[22] << 8)  | ((uint64_t)b[23]);
    r->d[0] = ((uint64_t)b[24] << 56) | ((uint64_t)b[25] << 48) |
              ((uint64_t)b[26] << 40) | ((uint64_t)b[27] << 32) |
              ((uint64_t)b[28] << 24) | ((uint64_t)b[29] << 16) |
              ((uint64_t)b[30] << 8)  | ((uint64_t)b[31]);
}

VANITY_INLINE void fe_to_bytes(uint8_t b[32], const fe_t *a) {
    /* Little-endian limbs to big-endian bytes */
    b[0]  = (uint8_t)(a->d[3] >> 56); b[1]  = (uint8_t)(a->d[3] >> 48);
    b[2]  = (uint8_t)(a->d[3] >> 40); b[3]  = (uint8_t)(a->d[3] >> 32);
    b[4]  = (uint8_t)(a->d[3] >> 24); b[5]  = (uint8_t)(a->d[3] >> 16);
    b[6]  = (uint8_t)(a->d[3] >> 8);  b[7]  = (uint8_t)(a->d[3]);
    b[8]  = (uint8_t)(a->d[2] >> 56); b[9]  = (uint8_t)(a->d[2] >> 48);
    b[10] = (uint8_t)(a->d[2] >> 40); b[11] = (uint8_t)(a->d[2] >> 32);
    b[12] = (uint8_t)(a->d[2] >> 24); b[13] = (uint8_t)(a->d[2] >> 16);
    b[14] = (uint8_t)(a->d[2] >> 8);  b[15] = (uint8_t)(a->d[2]);
    b[16] = (uint8_t)(a->d[1] >> 56); b[17] = (uint8_t)(a->d[1] >> 48);
    b[18] = (uint8_t)(a->d[1] >> 40); b[19] = (uint8_t)(a->d[1] >> 32);
    b[20] = (uint8_t)(a->d[1] >> 24); b[21] = (uint8_t)(a->d[1] >> 16);
    b[22] = (uint8_t)(a->d[1] >> 8);  b[23] = (uint8_t)(a->d[1]);
    b[24] = (uint8_t)(a->d[0] >> 56); b[25] = (uint8_t)(a->d[0] >> 48);
    b[26] = (uint8_t)(a->d[0] >> 40); b[27] = (uint8_t)(a->d[0] >> 32);
    b[28] = (uint8_t)(a->d[0] >> 24); b[29] = (uint8_t)(a->d[0] >> 16);
    b[30] = (uint8_t)(a->d[0] >> 8);  b[31] = (uint8_t)(a->d[0]);
}

VANITY_INLINE void fe_clear(fe_t *r) {
    r->d[0] = r->d[1] = r->d[2] = r->d[3] = 0;
}

VANITY_INLINE void fe_copy(fe_t *r, const fe_t *a) {
    r->d[0] = a->d[0];
    r->d[1] = a->d[1];
    r->d[2] = a->d[2];
    r->d[3] = a->d[3];
}

VANITY_INLINE int fe_is_zero(const fe_t *a) {
    return (a->d[0] | a->d[1] | a->d[2] | a->d[3]) == 0;
}

VANITY_INLINE int fe_equal(const fe_t *a, const fe_t *b) {
    return (a->d[0] == b->d[0]) && (a->d[1] == b->d[1]) &&
           (a->d[2] == b->d[2]) && (a->d[3] == b->d[3]);
}

VANITY_INLINE int fe_is_odd(const fe_t *a) {
    return (int)(a->d[0] & 1);
}

/* Compare a >= p */
VANITY_INLINE int fe_gte_p(const fe_t *a) {
    if (a->d[3] < FE_P.d[3]) return 0;
    if (a->d[3] > FE_P.d[3]) return 1;
    if (a->d[2] < FE_P.d[2]) return 0;
    if (a->d[2] > FE_P.d[2]) return 1;
    if (a->d[1] < FE_P.d[1]) return 0;
    if (a->d[1] > FE_P.d[1]) return 1;
    return a->d[0] >= FE_P.d[0];
}

VANITY_INLINE void fe_normalize(fe_t *r) {
    /* Subtract p if r >= p */
    if (fe_gte_p(r)) {
        uint64_t borrow = 0;
        r->d[0] = sbb(r->d[0], FE_P.d[0], &borrow);
        r->d[1] = sbb(r->d[1], FE_P.d[1], &borrow);
        r->d[2] = sbb(r->d[2], FE_P.d[2], &borrow);
        r->d[3] = sbb(r->d[3], FE_P.d[3], &borrow);
    }
}

VANITY_INLINE void fe_add(fe_t *r, const fe_t *a, const fe_t *b) {
    uint64_t carry = 0;
    r->d[0] = adc(a->d[0], b->d[0], &carry);
    r->d[1] = adc(a->d[1], b->d[1], &carry);
    r->d[2] = adc(a->d[2], b->d[2], &carry);
    r->d[3] = adc(a->d[3], b->d[3], &carry);

    /* Reduce if overflow or >= p */
    /* If carry or result >= p, subtract p */
    /* For secp256k1, subtracting p is equivalent to adding 2^32 + 977 */
    if (carry || fe_gte_p(r)) {
        uint64_t borrow = 0;
        r->d[0] = sbb(r->d[0], FE_P.d[0], &borrow);
        r->d[1] = sbb(r->d[1], FE_P.d[1], &borrow);
        r->d[2] = sbb(r->d[2], FE_P.d[2], &borrow);
        r->d[3] = sbb(r->d[3], FE_P.d[3], &borrow);
    }
}

VANITY_INLINE void fe_sub(fe_t *r, const fe_t *a, const fe_t *b) {
    uint64_t borrow = 0;
    r->d[0] = sbb(a->d[0], b->d[0], &borrow);
    r->d[1] = sbb(a->d[1], b->d[1], &borrow);
    r->d[2] = sbb(a->d[2], b->d[2], &borrow);
    r->d[3] = sbb(a->d[3], b->d[3], &borrow);

    /* If underflow, add p */
    if (borrow) {
        uint64_t carry = 0;
        r->d[0] = adc(r->d[0], FE_P.d[0], &carry);
        r->d[1] = adc(r->d[1], FE_P.d[1], &carry);
        r->d[2] = adc(r->d[2], FE_P.d[2], &carry);
        r->d[3] = adc(r->d[3], FE_P.d[3], &carry);
    }
}

VANITY_INLINE void fe_neg(fe_t *r, const fe_t *a) {
    if (fe_is_zero(a)) {
        fe_clear(r);
    } else {
        fe_sub(r, &FE_P, a);
    }
}

/*
 * Multiplication with secp256k1-specific reduction.
 *
 * For p = 2^256 - c where c = 2^32 + 977 (small constant),
 * reduction can be done efficiently:
 * a * 2^256 mod p = a * c mod p
 */
VANITY_INLINE void fe_mul(fe_t *r, const fe_t *a, const fe_t *b) {
    /* Full 512-bit product in 8 limbs */
    uint64_t t[8] = {0};
    uint64_t carry;

    /* Schoolbook multiplication */
    for (int i = 0; i < 4; i++) {
        carry = 0;
        for (int j = 0; j < 4; j++) {
            t[i + j] = mac(a->d[i], b->d[j], t[i + j], &carry);
        }
        t[i + 4] = carry;
    }

    /* Reduce mod p using: 2^256 = 2^32 + 977 (mod p) */
    /* t[4..7] * 2^256 = t[4..7] * (2^32 + 977) */

    /* c = 2^32 + 977 = 0x1000003D1 */
    const uint64_t c_low = 0x1000003D1ULL;

    /* Add t[4..7] * c to t[0..3] */
    uint64_t r0 = t[0], r1 = t[1], r2 = t[2], r3 = t[3];

    carry = 0;
    r0 = mac(t[4], c_low, r0, &carry);
    r1 = mac(t[5], c_low, r1, &carry);
    r2 = mac(t[6], c_low, r2, &carry);
    r3 = mac(t[7], c_low, r3, &carry);

    /* Handle final carry by multiplying by c again */
    uint64_t c2 = carry;
    carry = 0;
    r0 = mac(c2, c_low, r0, &carry);
    r1 = adc(r1, 0, &carry);
    r2 = adc(r2, 0, &carry);
    r3 = adc(r3, 0, &carry);

    /* One more round if needed */
    if (carry) {
        carry = 0;
        r0 = adc(r0, c_low, &carry);
        r1 = adc(r1, 0, &carry);
        r2 = adc(r2, 0, &carry);
        r3 = adc(r3, 0, &carry);
    }

    r->d[0] = r0;
    r->d[1] = r1;
    r->d[2] = r2;
    r->d[3] = r3;

    fe_normalize(r);
}

VANITY_INLINE void fe_sqr(fe_t *r, const fe_t *a) {
    /* For squaring, we could optimize, but for now just use multiply */
    fe_mul(r, a, a);
}

VANITY_INLINE void fe_cmov(fe_t *r, const fe_t *a, int flag) {
    uint64_t mask = (uint64_t)(-(int64_t)flag);
    r->d[0] = (r->d[0] & ~mask) | (a->d[0] & mask);
    r->d[1] = (r->d[1] & ~mask) | (a->d[1] & mask);
    r->d[2] = (r->d[2] & ~mask) | (a->d[2] & mask);
    r->d[3] = (r->d[3] & ~mask) | (a->d[3] & mask);
}

/*
 * Modular inverse using Fermat's little theorem:
 * a^-1 = a^(p-2) mod p
 *
 * p - 2 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2D
 */
void fe_inv(fe_t *r, const fe_t *a) {
    fe_t x2, x3, x6, x9, x11, x22, x44, x88, x176, x220, x223, t1;

    /* Compute a^(p-2) via addition chain */

    /* x2 = a^2 */
    fe_sqr(&x2, a);
    /* x2 = a^3 */
    fe_mul(&x3, &x2, a);
    /* x6 = a^6 */
    fe_sqr(&x6, &x3);
    /* x6 = a^7 */
    fe_mul(&x6, &x6, a);
    /* Continue building up the exponent... */

    /* For brevity, using a simpler (slower) square-and-multiply approach */
    /* This should be optimized with a proper addition chain for production */

    fe_copy(&t1, a);

    /* Square 256 times and multiply appropriately */
    /* p - 2 in binary: all 1s except specific positions */

    /* Simplified: use repeated squaring with accumulated multiplies */
    fe_t result;
    fe_copy(&result, &FE_ONE);
    fe_t base;
    fe_copy(&base, a);

    /* p - 2 = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2D */
    /* Process from LSB */

    /* Bits 0-3: 1101 = 0xD */
    fe_mul(&result, &result, &base); /* bit 0 */
    fe_sqr(&base, &base);
    fe_sqr(&base, &base);            /* bit 1 = 0 */
    fe_mul(&result, &result, &base); /* bit 2 */
    fe_sqr(&base, &base);
    fe_mul(&result, &result, &base); /* bit 3 */
    fe_sqr(&base, &base);

    /* Bits 4-7: 0010 = 0x2 */
    fe_sqr(&base, &base);            /* bit 4 = 0 */
    fe_mul(&result, &result, &base); /* bit 5 */
    fe_sqr(&base, &base);
    fe_sqr(&base, &base);            /* bit 6 = 0 */
    fe_sqr(&base, &base);            /* bit 7 = 0 */

    /* Continue for all 256 bits... */
    /* This is a simplified placeholder - production code needs full implementation */

    /* For now, compute using the standard method */
    fe_copy(&result, a);

    /* a^(2^2 - 1) */
    fe_sqr(&x2, a);
    fe_mul(&x2, &x2, a);

    /* a^(2^3 - 1) */
    fe_sqr(&x3, &x2);
    fe_mul(&x3, &x3, a);

    /* a^(2^6 - 1) */
    fe_sqr(&x6, &x3);
    for (int i = 1; i < 3; i++) fe_sqr(&x6, &x6);
    fe_mul(&x6, &x6, &x3);

    /* a^(2^9 - 1) */
    fe_sqr(&x9, &x6);
    for (int i = 1; i < 3; i++) fe_sqr(&x9, &x9);
    fe_mul(&x9, &x9, &x3);

    /* a^(2^11 - 1) */
    fe_sqr(&x11, &x9);
    for (int i = 1; i < 2; i++) fe_sqr(&x11, &x11);
    fe_mul(&x11, &x11, &x2);

    /* a^(2^22 - 1) */
    fe_sqr(&x22, &x11);
    for (int i = 1; i < 11; i++) fe_sqr(&x22, &x22);
    fe_mul(&x22, &x22, &x11);

    /* a^(2^44 - 1) */
    fe_sqr(&x44, &x22);
    for (int i = 1; i < 22; i++) fe_sqr(&x44, &x44);
    fe_mul(&x44, &x44, &x22);

    /* a^(2^88 - 1) */
    fe_sqr(&x88, &x44);
    for (int i = 1; i < 44; i++) fe_sqr(&x88, &x88);
    fe_mul(&x88, &x88, &x44);

    /* a^(2^176 - 1) */
    fe_sqr(&x176, &x88);
    for (int i = 1; i < 88; i++) fe_sqr(&x176, &x176);
    fe_mul(&x176, &x176, &x88);

    /* a^(2^220 - 1) */
    fe_sqr(&x220, &x176);
    for (int i = 1; i < 44; i++) fe_sqr(&x220, &x220);
    fe_mul(&x220, &x220, &x44);

    /* a^(2^223 - 1) */
    fe_sqr(&x223, &x220);
    for (int i = 1; i < 3; i++) fe_sqr(&x223, &x223);
    fe_mul(&x223, &x223, &x3);

    /* Final steps to get a^(p-2) */
    /* (2^223 - 1) * 2^23 + (2^22 - 1) * 2 + 1 * 2^5 + ... */
    fe_sqr(&t1, &x223);
    for (int i = 1; i < 23; i++) fe_sqr(&t1, &t1);
    fe_mul(&t1, &t1, &x22);
    for (int i = 0; i < 5; i++) fe_sqr(&t1, &t1);
    fe_mul(&t1, &t1, a);
    for (int i = 0; i < 3; i++) fe_sqr(&t1, &t1);
    fe_mul(&t1, &t1, &x2);
    for (int i = 0; i < 2; i++) fe_sqr(&t1, &t1);
    fe_mul(r, &t1, a);
}

/*
 * Square root using Tonelli-Shanks for p = 3 mod 4
 * For secp256k1, p mod 4 = 3, so sqrt(a) = a^((p+1)/4) if it exists
 */
int fe_sqrt(fe_t *r, const fe_t *a) {
    fe_t t1, t2;

    /* Compute a^((p+1)/4) */
    /* (p+1)/4 = (2^256 - 2^32 - 976) / 4 = 2^254 - 2^30 - 244 */

    /* Use similar exponentiation chain as inverse */
    fe_t x2, x3, x6, x9, x11, x22, x44, x88, x176, x220, x223;

    /* a^(2^2 - 1) */
    fe_sqr(&x2, a);
    fe_mul(&x2, &x2, a);

    /* a^(2^3 - 1) */
    fe_sqr(&x3, &x2);
    fe_mul(&x3, &x3, a);

    /* a^(2^6 - 1) */
    fe_sqr(&x6, &x3);
    for (int i = 1; i < 3; i++) fe_sqr(&x6, &x6);
    fe_mul(&x6, &x6, &x3);

    /* a^(2^9 - 1) */
    fe_sqr(&x9, &x6);
    for (int i = 1; i < 3; i++) fe_sqr(&x9, &x9);
    fe_mul(&x9, &x9, &x3);

    /* a^(2^11 - 1) */
    fe_sqr(&x11, &x9);
    for (int i = 1; i < 2; i++) fe_sqr(&x11, &x11);
    fe_mul(&x11, &x11, &x2);

    /* a^(2^22 - 1) */
    fe_sqr(&x22, &x11);
    for (int i = 1; i < 11; i++) fe_sqr(&x22, &x22);
    fe_mul(&x22, &x22, &x11);

    /* a^(2^44 - 1) */
    fe_sqr(&x44, &x22);
    for (int i = 1; i < 22; i++) fe_sqr(&x44, &x44);
    fe_mul(&x44, &x44, &x22);

    /* a^(2^88 - 1) */
    fe_sqr(&x88, &x44);
    for (int i = 1; i < 44; i++) fe_sqr(&x88, &x88);
    fe_mul(&x88, &x88, &x44);

    /* a^(2^176 - 1) */
    fe_sqr(&x176, &x88);
    for (int i = 1; i < 88; i++) fe_sqr(&x176, &x176);
    fe_mul(&x176, &x176, &x88);

    /* a^(2^220 - 1) */
    fe_sqr(&x220, &x176);
    for (int i = 1; i < 44; i++) fe_sqr(&x220, &x220);
    fe_mul(&x220, &x220, &x44);

    /* a^(2^223 - 1) */
    fe_sqr(&x223, &x220);
    for (int i = 1; i < 3; i++) fe_sqr(&x223, &x223);
    fe_mul(&x223, &x223, &x3);

    /* Continue to get a^((p+1)/4) */
    fe_sqr(&t1, &x223);
    for (int i = 1; i < 23; i++) fe_sqr(&t1, &t1);
    fe_mul(&t1, &t1, &x22);
    for (int i = 0; i < 6; i++) fe_sqr(&t1, &t1);
    fe_mul(&t1, &t1, &x2);
    fe_sqr(&t1, &t1);
    fe_sqr(&t1, &t1);

    fe_copy(r, &t1);

    /* Verify: r^2 should equal a */
    fe_sqr(&t2, r);
    return fe_equal(&t2, a);
}
