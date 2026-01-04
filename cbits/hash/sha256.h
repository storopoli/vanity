/*
 * sha256.h - SHA-256 hash function
 *
 * Provides SHA-256 hashing for Bitcoin address generation.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_HASH_SHA256_H
#define VANITY_HASH_SHA256_H

#include "../common.h"

/* SHA-256 context for incremental hashing */
typedef struct {
    uint32_t state[8];      /* Hash state */
    uint64_t count;         /* Number of bits processed */
    uint8_t buffer[64];     /* Input buffer */
} sha256_ctx;

/* Initialize SHA-256 context */
void sha256_init(sha256_ctx *ctx);

/* Update context with data */
void sha256_update(sha256_ctx *ctx, const uint8_t *data, size_t len);

/* Finalize and output 32-byte hash */
void sha256_final(sha256_ctx *ctx, uint8_t hash[32]);

/* Single-shot SHA-256: hash data to 32-byte output */
void sha256(uint8_t hash[32], const uint8_t *data, size_t len);

/* Double SHA-256: SHA256(SHA256(data)) - used in Bitcoin */
void sha256d(uint8_t hash[32], const uint8_t *data, size_t len);

#endif /* VANITY_HASH_SHA256_H */
