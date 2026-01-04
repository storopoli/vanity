/*
 * group.c - secp256k1 group operations implementation
 *
 * Implements elliptic curve point arithmetic using Jacobian projective coordinates.
 * The curve equation is y^2 = x^3 + 7 over Fp.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "group.h"
#include "field.h"

/*
 * Generator point G for secp256k1
 * Gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
 * Gy = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8
 */
const ge_t GE_G = {
    .x = {{
        0x59F2815B16F81798ULL,
        0x029BFCDB2DCE28D9ULL,
        0x55A06295CE870B07ULL,
        0x79BE667EF9DCBBACULL
    }},
    .y = {{
        0x9C47D08FFB10D4B8ULL,
        0xFD17B448A6855419ULL,
        0x5DA4FBFC0E1108A8ULL,
        0x483ADA7726A3C465ULL
    }}
};

const gej_t GEJ_INFINITY = {
    .x = {{0, 0, 0, 0}},
    .y = {{1, 0, 0, 0}},  /* Y = 1 for infinity representation */
    .z = {{0, 0, 0, 0}},
    .infinity = 1
};

VANITY_INLINE void gej_set_infinity(gej_t *r) {
    fe_clear(&r->x);
    fe_copy(&r->y, &FE_ONE);
    fe_clear(&r->z);
    r->infinity = 1;
}

VANITY_INLINE int gej_is_infinity(const gej_t *a) {
    return a->infinity || fe_is_zero(&a->z);
}

VANITY_INLINE void gej_set_ge(gej_t *r, const ge_t *a) {
    fe_copy(&r->x, &a->x);
    fe_copy(&r->y, &a->y);
    fe_copy(&r->z, &FE_ONE);
    r->infinity = 0;
}

VANITY_INLINE void gej_copy(gej_t *r, const gej_t *a) {
    fe_copy(&r->x, &a->x);
    fe_copy(&r->y, &a->y);
    fe_copy(&r->z, &a->z);
    r->infinity = a->infinity;
}

/*
 * Convert Jacobian to affine: (x, y) = (X/Z^2, Y/Z^3)
 */
void gej_to_ge(ge_t *r, const gej_t *a) {
    if (gej_is_infinity(a)) {
        /* Return a sentinel value for infinity (shouldn't happen in normal use) */
        fe_clear(&r->x);
        fe_clear(&r->y);
        return;
    }

    fe_t z2, z3, z_inv;

    /* z_inv = Z^-1 */
    fe_inv(&z_inv, &a->z);

    /* z2 = Z^-2 */
    fe_sqr(&z2, &z_inv);

    /* z3 = Z^-3 */
    fe_mul(&z3, &z2, &z_inv);

    /* x = X * Z^-2 */
    fe_mul(&r->x, &a->x, &z2);

    /* y = Y * Z^-3 */
    fe_mul(&r->y, &a->y, &z3);
}

/*
 * Point doubling using Jacobian coordinates
 *
 * Formula (for a = 0 in y^2 = x^3 + ax + b):
 * S = 4 * X * Y^2
 * M = 3 * X^2
 * X' = M^2 - 2*S
 * Y' = M * (S - X') - 8 * Y^4
 * Z' = 2 * Y * Z
 *
 * Cost: 1M + 5S + 1*a + 7add + 2*2 + 1*3 + 1*8
 * For a=0: 1M + 5S + 7add + 2*2 + 1*3 + 1*8
 */
void gej_double(gej_t *r, const gej_t *a) {
    if (gej_is_infinity(a)) {
        gej_set_infinity(r);
        return;
    }

    fe_t s, m, t, x3, y3, z3;
    fe_t y2, y4;

    /* Y^2 */
    fe_sqr(&y2, &a->y);

    /* S = 4 * X * Y^2 */
    fe_mul(&s, &a->x, &y2);
    fe_add(&s, &s, &s);
    fe_add(&s, &s, &s);  /* S = 4 * X * Y^2 */

    /* M = 3 * X^2 (since a = 0 for secp256k1) */
    fe_sqr(&m, &a->x);
    fe_add(&t, &m, &m);
    fe_add(&m, &t, &m);  /* M = 3 * X^2 */

    /* X' = M^2 - 2*S */
    fe_sqr(&x3, &m);
    fe_sub(&x3, &x3, &s);
    fe_sub(&x3, &x3, &s);

    /* Y^4 */
    fe_sqr(&y4, &y2);

    /* Y' = M * (S - X') - 8 * Y^4 */
    fe_sub(&t, &s, &x3);
    fe_mul(&y3, &m, &t);
    fe_add(&t, &y4, &y4);
    fe_add(&t, &t, &t);
    fe_add(&t, &t, &t);  /* 8 * Y^4 */
    fe_sub(&y3, &y3, &t);

    /* Z' = 2 * Y * Z */
    fe_mul(&z3, &a->y, &a->z);
    fe_add(&z3, &z3, &z3);

    fe_copy(&r->x, &x3);
    fe_copy(&r->y, &y3);
    fe_copy(&r->z, &z3);
    r->infinity = 0;
}

/*
 * Mixed addition: Jacobian + Affine
 *
 * More efficient when second operand is affine (Z = 1).
 *
 * U1 = X1
 * U2 = X2 * Z1^2
 * S1 = Y1
 * S2 = Y2 * Z1^3
 * H = U2 - U1
 * R = S2 - S1
 * X3 = R^2 - H^3 - 2*U1*H^2
 * Y3 = R*(U1*H^2 - X3) - S1*H^3
 * Z3 = Z1 * H
 */
void gej_add_ge(gej_t *r, const gej_t *a, const ge_t *b) {
    if (gej_is_infinity(a)) {
        gej_set_ge(r, b);
        return;
    }

    fe_t z12, z13, u2, s2, h, r_val, h2, h3, u1h2;

    /* Z1^2 */
    fe_sqr(&z12, &a->z);

    /* Z1^3 */
    fe_mul(&z13, &z12, &a->z);

    /* U2 = X2 * Z1^2 */
    fe_mul(&u2, &b->x, &z12);

    /* S2 = Y2 * Z1^3 */
    fe_mul(&s2, &b->y, &z13);

    /* H = U2 - U1 (U1 = X1) */
    fe_sub(&h, &u2, &a->x);

    /* R = S2 - S1 (S1 = Y1) */
    fe_sub(&r_val, &s2, &a->y);

    /* Check for special cases */
    if (fe_is_zero(&h)) {
        if (fe_is_zero(&r_val)) {
            /* P + P = 2P (doubling) */
            gej_t temp;
            gej_set_ge(&temp, b);
            gej_double(r, &temp);
            return;
        } else {
            /* P + (-P) = infinity */
            gej_set_infinity(r);
            return;
        }
    }

    /* H^2 */
    fe_sqr(&h2, &h);

    /* H^3 */
    fe_mul(&h3, &h2, &h);

    /* U1 * H^2 */
    fe_mul(&u1h2, &a->x, &h2);

    /* X3 = R^2 - H^3 - 2*U1*H^2 */
    fe_t x3;
    fe_sqr(&x3, &r_val);
    fe_sub(&x3, &x3, &h3);
    fe_sub(&x3, &x3, &u1h2);
    fe_sub(&x3, &x3, &u1h2);

    /* Y3 = R*(U1*H^2 - X3) - S1*H^3 */
    fe_t y3, t;
    fe_sub(&t, &u1h2, &x3);
    fe_mul(&y3, &r_val, &t);
    fe_mul(&t, &a->y, &h3);
    fe_sub(&y3, &y3, &t);

    /* Z3 = Z1 * H */
    fe_t z3;
    fe_mul(&z3, &a->z, &h);

    fe_copy(&r->x, &x3);
    fe_copy(&r->y, &y3);
    fe_copy(&r->z, &z3);
    r->infinity = 0;
}

/*
 * General addition: Jacobian + Jacobian
 */
void gej_add(gej_t *r, const gej_t *a, const gej_t *b) {
    if (gej_is_infinity(a)) {
        gej_copy(r, b);
        return;
    }
    if (gej_is_infinity(b)) {
        gej_copy(r, a);
        return;
    }

    fe_t z12, z22, z13, z23;
    fe_t u1, u2, s1, s2, h, r_val;

    /* Z1^2 and Z2^2 */
    fe_sqr(&z12, &a->z);
    fe_sqr(&z22, &b->z);

    /* Z1^3 and Z2^3 */
    fe_mul(&z13, &z12, &a->z);
    fe_mul(&z23, &z22, &b->z);

    /* U1 = X1 * Z2^2, U2 = X2 * Z1^2 */
    fe_mul(&u1, &a->x, &z22);
    fe_mul(&u2, &b->x, &z12);

    /* S1 = Y1 * Z2^3, S2 = Y2 * Z1^3 */
    fe_mul(&s1, &a->y, &z23);
    fe_mul(&s2, &b->y, &z13);

    /* H = U2 - U1 */
    fe_sub(&h, &u2, &u1);

    /* R = S2 - S1 */
    fe_sub(&r_val, &s2, &s1);

    /* Check for special cases */
    if (fe_is_zero(&h)) {
        if (fe_is_zero(&r_val)) {
            /* a == b, double */
            gej_double(r, a);
            return;
        } else {
            /* a == -b */
            gej_set_infinity(r);
            return;
        }
    }

    fe_t h2, h3, u1h2;

    /* H^2 */
    fe_sqr(&h2, &h);

    /* H^3 */
    fe_mul(&h3, &h2, &h);

    /* U1 * H^2 */
    fe_mul(&u1h2, &u1, &h2);

    /* X3 = R^2 - H^3 - 2*U1*H^2 */
    fe_t x3;
    fe_sqr(&x3, &r_val);
    fe_sub(&x3, &x3, &h3);
    fe_sub(&x3, &x3, &u1h2);
    fe_sub(&x3, &x3, &u1h2);

    /* Y3 = R*(U1*H^2 - X3) - S1*H^3 */
    fe_t y3, t;
    fe_sub(&t, &u1h2, &x3);
    fe_mul(&y3, &r_val, &t);
    fe_mul(&t, &s1, &h3);
    fe_sub(&y3, &y3, &t);

    /* Z3 = Z1 * Z2 * H */
    fe_t z3;
    fe_mul(&z3, &a->z, &b->z);
    fe_mul(&z3, &z3, &h);

    fe_copy(&r->x, &x3);
    fe_copy(&r->y, &y3);
    fe_copy(&r->z, &z3);
    r->infinity = 0;
}

VANITY_INLINE void gej_neg(gej_t *r, const gej_t *a) {
    fe_copy(&r->x, &a->x);
    fe_neg(&r->y, &a->y);
    fe_copy(&r->z, &a->z);
    r->infinity = a->infinity;
}

VANITY_INLINE void ge_neg(ge_t *r, const ge_t *a) {
    fe_copy(&r->x, &a->x);
    fe_neg(&r->y, &a->y);
}

/*
 * Serialize public key to compressed format (33 bytes)
 * Format: [02/03][X coordinate in big-endian]
 * 02 if Y is even, 03 if Y is odd
 */
void ge_serialize(uint8_t out[33], const ge_t *p) {
    /* Prefix byte: 02 for even Y, 03 for odd Y */
    out[0] = fe_is_odd(&p->y) ? 0x03 : 0x02;

    /* X coordinate in big-endian */
    fe_to_bytes(out + 1, &p->x);
}

/*
 * Serialize public key to x-only format (32 bytes, for P2TR/Taproot)
 * Just the X coordinate in big-endian
 */
void ge_serialize_xonly(uint8_t out[32], const ge_t *p) {
    fe_to_bytes(out, &p->x);
}

/*
 * Verify point is on curve: y^2 = x^3 + 7
 */
int ge_is_valid(const ge_t *p) {
    fe_t y2, x3, rhs;

    /* y^2 */
    fe_sqr(&y2, &p->y);

    /* x^3 */
    fe_sqr(&x3, &p->x);
    fe_mul(&x3, &x3, &p->x);

    /* x^3 + 7 */
    fe_t seven = {{7, 0, 0, 0}};
    fe_add(&rhs, &x3, &seven);

    return fe_equal(&y2, &rhs);
}
