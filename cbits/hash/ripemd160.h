/*
 * ripemd160.h - RIPEMD-160 hash function
 *
 * Used in Bitcoin for Hash160 = RIPEMD160(SHA256(data))
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_HASH_RIPEMD160_H
#define VANITY_HASH_RIPEMD160_H

#include "../common.h"

/* RIPEMD-160 context for incremental hashing */
typedef struct {
    uint32_t state[5];      /* Hash state */
    uint64_t count;         /* Number of bits processed */
    uint8_t buffer[64];     /* Input buffer */
} ripemd160_ctx;

/* Initialize RIPEMD-160 context */
void ripemd160_init(ripemd160_ctx *ctx);

/* Update context with data */
void ripemd160_update(ripemd160_ctx *ctx, const uint8_t *data, size_t len);

/* Finalize and output 20-byte hash */
void ripemd160_final(ripemd160_ctx *ctx, uint8_t hash[20]);

/* Single-shot RIPEMD-160: hash data to 20-byte output */
void ripemd160(uint8_t hash[20], const uint8_t *data, size_t len);

/* Hash160: RIPEMD160(SHA256(data)) - Bitcoin's standard pubkey hash */
void hash160(uint8_t hash[20], const uint8_t *data, size_t len);

#endif /* VANITY_HASH_RIPEMD160_H */
