/*
 * field.h - secp256k1 field arithmetic (256-bit modular operations)
 *
 * All operations are performed modulo:
 * p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F
 *   = 2^256 - 2^32 - 977
 *
 * Field elements are represented as 4 x 64-bit limbs in little-endian order.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_SECP256K1_FIELD_H
#define VANITY_SECP256K1_FIELD_H

#include "../common.h"

/* Field element: 256 bits as 4 x 64-bit limbs (little-endian) */
typedef struct {
    uint64_t d[4];
} fe_t;

/* Field prime p in limb form */
extern const fe_t FE_P;

/* Zero and one */
extern const fe_t FE_ZERO;
extern const fe_t FE_ONE;

/* Initialize field element from 32-byte big-endian array */
VANITY_INLINE void fe_from_bytes(fe_t *r, const uint8_t b[32]);

/* Serialize field element to 32-byte big-endian array */
VANITY_INLINE void fe_to_bytes(uint8_t b[32], const fe_t *a);

/* Set r = 0 */
VANITY_INLINE void fe_clear(fe_t *r);

/* Set r = a */
VANITY_INLINE void fe_copy(fe_t *r, const fe_t *a);

/* Check if a == 0 */
VANITY_INLINE int fe_is_zero(const fe_t *a);

/* Check if a == b */
VANITY_INLINE int fe_equal(const fe_t *a, const fe_t *b);

/* Check if a is odd (least significant bit is 1) */
VANITY_INLINE int fe_is_odd(const fe_t *a);

/* r = a + b mod p */
VANITY_INLINE void fe_add(fe_t *r, const fe_t *a, const fe_t *b);

/* r = a - b mod p */
VANITY_INLINE void fe_sub(fe_t *r, const fe_t *a, const fe_t *b);

/* r = -a mod p */
VANITY_INLINE void fe_neg(fe_t *r, const fe_t *a);

/* r = a * b mod p */
VANITY_INLINE void fe_mul(fe_t *r, const fe_t *a, const fe_t *b);

/* r = a^2 mod p */
VANITY_INLINE void fe_sqr(fe_t *r, const fe_t *a);

/* r = a^-1 mod p (modular inverse using Fermat's little theorem) */
void fe_inv(fe_t *r, const fe_t *a);

/* r = sqrt(a) mod p if exists, returns 1 on success, 0 if no square root */
int fe_sqrt(fe_t *r, const fe_t *a);

/* Conditional move: r = flag ? a : r */
VANITY_INLINE void fe_cmov(fe_t *r, const fe_t *a, int flag);

/* Normalize field element to range [0, p) */
VANITY_INLINE void fe_normalize(fe_t *r);

#endif /* VANITY_SECP256K1_FIELD_H */
