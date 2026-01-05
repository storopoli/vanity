/*
 * scalar.h - secp256k1 scalar arithmetic (256-bit modular operations)
 *
 * All operations are performed modulo the curve order:
 * n = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_SECP256K1_SCALAR_H
#define VANITY_SECP256K1_SCALAR_H

#include "../common.h"

/* Scalar: 256 bits as 4 x 64-bit limbs (little-endian) */
typedef struct {
    uint64_t d[4];
} scalar_t;

/* Curve order n */
extern const scalar_t SCALAR_N;

/* Zero and one */
extern const scalar_t SCALAR_ZERO;
extern const scalar_t SCALAR_ONE;

/* Initialize scalar from 32-byte big-endian array */
VANITY_INLINE void scalar_from_bytes(scalar_t *r, const uint8_t b[32]);

/* Serialize scalar to 32-byte big-endian array */
VANITY_INLINE void scalar_to_bytes(uint8_t b[32], const scalar_t *a);

/* Set r = 0 */
VANITY_INLINE void scalar_clear(scalar_t *r);

/* Set r = a */
VANITY_INLINE void scalar_copy(scalar_t *r, const scalar_t *a);

/* Check if a == 0 */
VANITY_INLINE int scalar_is_zero(const scalar_t *a);

/* Check if scalar is valid (0 < a < n) */
VANITY_INLINE int scalar_is_valid(const scalar_t *a);

/* Reduce scalar to [0, n) - needed after generating random 256-bit value */
void scalar_reduce(scalar_t *r);

/* r = a + b mod n */
VANITY_INLINE void scalar_add(scalar_t *r, const scalar_t *a, const scalar_t *b);

/* r = -a mod n */
VANITY_INLINE void scalar_neg(scalar_t *r, const scalar_t *a);

/* r = a * b mod n */
void scalar_mul(scalar_t *r, const scalar_t *a, const scalar_t *b);

/* Split scalar for efficient multiplication (not needed for simple impl) */
/* void scalar_split_lambda(scalar_t *r1, scalar_t *r2, const scalar_t *a); */

#endif /* VANITY_SECP256K1_SCALAR_H */
