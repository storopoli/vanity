/*
 * group.h - secp256k1 group operations (elliptic curve point arithmetic)
 *
 * Points are represented in Jacobian projective coordinates (X, Y, Z)
 * where the affine point (x, y) corresponds to (X/Z^2, Y/Z^3).
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_SECP256K1_GROUP_H
#define VANITY_SECP256K1_GROUP_H

#include "field.h"

/* Affine point (x, y) */
typedef struct {
    fe_t x;
    fe_t y;
} ge_t;

/* Jacobian projective point (X, Y, Z) where (x, y) = (X/Z^2, Y/Z^3) */
typedef struct {
    fe_t x;
    fe_t y;
    fe_t z;
    int infinity;  /* Flag for point at infinity */
} gej_t;

/* Generator point G (affine) */
extern const ge_t GE_G;

/* Point at infinity */
extern const gej_t GEJ_INFINITY;

/* Set r to point at infinity */
VANITY_INLINE void gej_set_infinity(gej_t *r);

/* Check if point is at infinity */
VANITY_INLINE int gej_is_infinity(const gej_t *a);

/* Set Jacobian point from affine */
VANITY_INLINE void gej_set_ge(gej_t *r, const ge_t *a);

/* Copy Jacobian point */
VANITY_INLINE void gej_copy(gej_t *r, const gej_t *a);

/* Convert Jacobian to affine (requires field inversion) */
void gej_to_ge(ge_t *r, const gej_t *a);

/*
 * Point doubling: r = 2 * a
 * Uses complete formula for Jacobian coordinates.
 */
void gej_double(gej_t *r, const gej_t *a);

/*
 * Mixed addition: r = a + b
 * a is Jacobian, b is affine (Z = 1).
 * More efficient than general addition when one point is affine.
 */
void gej_add_ge(gej_t *r, const gej_t *a, const ge_t *b);

/*
 * General addition: r = a + b
 * Both points in Jacobian coordinates.
 */
void gej_add(gej_t *r, const gej_t *a, const gej_t *b);

/* Negate point: r = -a */
VANITY_INLINE void gej_neg(gej_t *r, const gej_t *a);

/* Negate affine point: r = -a */
VANITY_INLINE void ge_neg(ge_t *r, const ge_t *a);

/* Serialize public key to compressed format (33 bytes) */
void ge_serialize(uint8_t out[33], const ge_t *p);

/* Serialize public key to x-only format (32 bytes, for P2TR) */
void ge_serialize_xonly(uint8_t out[32], const ge_t *p);

/* Check if affine point is on curve: y^2 = x^3 + 7 */
int ge_is_valid(const ge_t *p);

#endif /* VANITY_SECP256K1_GROUP_H */
