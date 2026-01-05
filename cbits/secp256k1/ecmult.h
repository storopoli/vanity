/*
 * ecmult.h - secp256k1 scalar multiplication with precomputation
 *
 * Provides efficient scalar multiplication of the generator point G
 * using a precomputed table of multiples.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_SECP256K1_ECMULT_H
#define VANITY_SECP256K1_ECMULT_H

#include "group.h"
#include "scalar.h"

/*
 * Window size for precomputation
 * Using window size 4: table contains 2^3 = 8 points (1G, 2G, ..., 8G)
 * Trade-off between table size and number of additions
 */
#define ECMULT_WINDOW_SIZE 4
#define ECMULT_TABLE_SIZE (1 << (ECMULT_WINDOW_SIZE - 1))

/*
 * Precomputed table for generator point multiplication
 * Contains odd multiples: G, 3G, 5G, 7G, 9G, 11G, 13G, 15G
 * (Even multiples not needed due to signed digit representation)
 */
typedef struct {
    ge_t table[ECMULT_TABLE_SIZE];
} ecmult_gen_context;

/*
 * Initialize precomputation context
 * Must be called before ecmult_gen()
 */
void ecmult_gen_context_init(ecmult_gen_context *ctx);

/*
 * Scalar multiplication: r = G * scalar
 *
 * Uses wNAF (windowed Non-Adjacent Form) with precomputed table.
 * This is the primary function for deriving public keys.
 *
 * @param r      Output point in Jacobian coordinates
 * @param ctx    Precomputation context (initialized with ecmult_gen_context_init)
 * @param scalar 32-byte scalar in big-endian format
 */
void ecmult_gen(gej_t *r, const ecmult_gen_context *ctx, const uint8_t scalar[32]);

/*
 * Scalar multiplication with scalar_t type
 */
void ecmult_gen_scalar(gej_t *r, const ecmult_gen_context *ctx, const scalar_t *scalar);

/*
 * General scalar multiplication: r = P * scalar
 * (For arbitrary base point P, not just G)
 */
void ecmult(gej_t *r, const gej_t *p, const scalar_t *scalar);

/*
 * Batch scalar multiplication
 * Compute multiple r[i] = G * scalars[i] efficiently
 *
 * @param r       Output array of Jacobian points
 * @param ctx     Precomputation context
 * @param scalars Array of 32-byte scalars in big-endian format
 * @param count   Number of scalars to process
 */
void ecmult_gen_batch(
    gej_t *r,
    const ecmult_gen_context *ctx,
    const uint8_t *scalars,
    size_t count
);

#endif /* VANITY_SECP256K1_ECMULT_H */
