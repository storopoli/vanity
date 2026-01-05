/*
 * ecmult.c - secp256k1 scalar multiplication implementation
 *
 * Uses a precomputed table of generator multiples and wNAF representation
 * for efficient scalar multiplication.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "ecmult.h"

/*
 * Initialize precomputation context with odd multiples of G:
 * table[i] = (2*i + 1) * G for i = 0..7
 * So table = [G, 3G, 5G, 7G, 9G, 11G, 13G, 15G]
 */
void ecmult_gen_context_init(ecmult_gen_context *ctx) {
    gej_t gj, acc;
    ge_t g2;

    /* Start with G */
    gej_set_ge(&gj, &GE_G);

    /* 2G for stepping */
    gej_double(&acc, &gj);
    gej_to_ge(&g2, &acc);

    /* table[0] = G */
    ctx->table[0] = GE_G;

    /* Compute remaining odd multiples */
    gej_copy(&acc, &gj);
    for (int i = 1; i < ECMULT_TABLE_SIZE; i++) {
        /* acc = (2*i - 1) * G, add 2G to get (2*i + 1) * G */
        gej_add_ge(&acc, &acc, &g2);
        gej_add_ge(&acc, &acc, &g2);
        gej_to_ge(&ctx->table[i], &acc);
    }
}

/*
 * Scalar multiplication: r = G * scalar
 *
 * Uses fixed-window method with 4-bit windows.
 * Process scalar from most significant to least significant bits.
 */
void ecmult_gen(gej_t *r, const ecmult_gen_context *ctx, const uint8_t scalar[32]) {
    scalar_t s;
    scalar_from_bytes(&s, scalar);
    ecmult_gen_scalar(r, ctx, &s);
}

/*
 * Implementation using fixed-window scalar multiplication
 *
 * For each 4-bit window, we look up the appropriate point from the table
 * and add it (or its negation for signed representation).
 */
void ecmult_gen_scalar(gej_t *r, const ecmult_gen_context *ctx, const scalar_t *scalar) {
    /* Handle zero scalar */
    if (scalar_is_zero(scalar)) {
        gej_set_infinity(r);
        return;
    }

    /* Convert scalar to signed digit representation for better efficiency */
    /* Using simple 4-bit unsigned windows for now */

    /* Start with point at infinity */
    gej_set_infinity(r);

    /* Process scalar from MSB to LSB, 4 bits at a time */
    /* scalar is in little-endian limbs, so process d[3] first */

    for (int limb = 3; limb >= 0; limb--) {
        uint64_t d = scalar->d[limb];

        /* Process 16 windows of 4 bits each per limb */
        for (int j = 60; j >= 0; j -= 4) {
            /* Double 4 times (unless this is the very first non-zero window) */
            if (!gej_is_infinity(r)) {
                gej_double(r, r);
                gej_double(r, r);
                gej_double(r, r);
                gej_double(r, r);
            }

            /* Extract 4-bit window */
            int window = (d >> j) & 0xF;

            if (window != 0) {
                if (gej_is_infinity(r)) {
                    /* First non-zero: just set the point */
                    if (window & 1) {
                        /* Odd window: use table directly */
                        gej_set_ge(r, &ctx->table[window >> 1]);
                    } else {
                        /* Even window: table[(window-1)/2] + G */
                        /* Or handle by using window-1 which is odd and add G */
                        gej_set_ge(r, &ctx->table[(window - 1) >> 1]);
                        gej_add_ge(r, r, &GE_G);
                    }
                } else {
                    /* Add table point */
                    if (window & 1) {
                        /* Odd window */
                        gej_add_ge(r, r, &ctx->table[window >> 1]);
                    } else {
                        /* Even window: decompose as odd + 1 */
                        gej_add_ge(r, r, &ctx->table[(window - 1) >> 1]);
                        gej_add_ge(r, r, &GE_G);
                    }
                }
            }
        }
    }
}

/*
 * General scalar multiplication: r = P * scalar
 * Uses double-and-add algorithm for arbitrary base point
 */
void ecmult(gej_t *r, const gej_t *p, const scalar_t *scalar) {
    /* Handle zero scalar */
    if (scalar_is_zero(scalar)) {
        gej_set_infinity(r);
        return;
    }

    /* Handle point at infinity */
    if (gej_is_infinity(p)) {
        gej_set_infinity(r);
        return;
    }

    gej_t acc;
    gej_set_infinity(&acc);

    /* Double-and-add from MSB */
    for (int limb = 3; limb >= 0; limb--) {
        uint64_t d = scalar->d[limb];

        for (int j = 63; j >= 0; j--) {
            /* Double */
            gej_double(&acc, &acc);

            /* Add if bit is set */
            if ((d >> j) & 1) {
                gej_add(&acc, &acc, p);
            }
        }
    }

    gej_copy(r, &acc);
}

/*
 * Batch scalar multiplication
 * Process multiple scalars independently (can be parallelized on GPU)
 */
void ecmult_gen_batch(
    gej_t *r,
    const ecmult_gen_context *ctx,
    const uint8_t *scalars,
    size_t count
) {
    for (size_t i = 0; i < count; i++) {
        ecmult_gen(&r[i], ctx, scalars + i * 32);
    }
}
