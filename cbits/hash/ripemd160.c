/*
 * ripemd160.c - RIPEMD-160 implementation
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "ripemd160.h"
#include "sha256.h"

/* Initial hash values */
static const uint32_t RIPEMD160_H0[5] = {
    0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0
};

/* Rotate left */
#define ROTL(x, n) (((x) << (n)) | ((x) >> (32 - (n))))

/* Round functions */
#define F(x, y, z) ((x) ^ (y) ^ (z))
#define G(x, y, z) (((x) & (y)) | (~(x) & (z)))
#define H(x, y, z) (((x) | ~(y)) ^ (z))
#define I(x, y, z) (((x) & (z)) | ((y) & ~(z)))
#define J(x, y, z) ((x) ^ ((y) | ~(z)))

/* Round constants */
#define K0 0x00000000
#define K1 0x5a827999
#define K2 0x6ed9eba1
#define K3 0x8f1bbcdc
#define K4 0xa953fd4e
#define KP0 0x50a28be6
#define KP1 0x5c4dd124
#define KP2 0x6d703ef3
#define KP3 0x7a6d76e9
#define KP4 0x00000000

/* Message schedule for left rounds */
static const int R[80] = {
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
    3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
    1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
    4, 0, 5, 9, 7, 12, 2, 10, 14, 1, 3, 8, 11, 6, 15, 13
};

/* Message schedule for right rounds */
static const int RP[80] = {
    5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
    6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
    15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
    8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
    12, 15, 10, 4, 1, 5, 8, 7, 6, 2, 13, 14, 0, 3, 9, 11
};

/* Rotation amounts for left rounds */
static const int S[80] = {
    11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
    7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
    11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
    11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
    9, 15, 5, 11, 6, 8, 13, 12, 5, 12, 13, 14, 11, 8, 5, 6
};

/* Rotation amounts for right rounds */
static const int SP[80] = {
    8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
    9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
    9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
    15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
    8, 5, 12, 9, 12, 5, 14, 6, 8, 13, 6, 5, 15, 13, 11, 11
};

/* Process one 64-byte block */
static void ripemd160_transform(uint32_t state[5], const uint8_t block[64]) {
    uint32_t x[16];
    uint32_t al, bl, cl, dl, el;
    uint32_t ar, br, cr, dr, er;
    uint32_t t;

    /* Parse block into 16 32-bit words (little-endian) */
    for (int i = 0; i < 16; i++) {
        x[i] = ((uint32_t)block[i * 4]) |
               ((uint32_t)block[i * 4 + 1] << 8) |
               ((uint32_t)block[i * 4 + 2] << 16) |
               ((uint32_t)block[i * 4 + 3] << 24);
    }

    /* Initialize working variables */
    al = ar = state[0];
    bl = br = state[1];
    cl = cr = state[2];
    dl = dr = state[3];
    el = er = state[4];

    /* 80 rounds in parallel */
    for (int j = 0; j < 80; j++) {
        uint32_t fl, fr, kl, kr;

        /* Select function and constant for left rounds */
        if (j < 16) {
            fl = F(bl, cl, dl); kl = K0;
        } else if (j < 32) {
            fl = G(bl, cl, dl); kl = K1;
        } else if (j < 48) {
            fl = H(bl, cl, dl); kl = K2;
        } else if (j < 64) {
            fl = I(bl, cl, dl); kl = K3;
        } else {
            fl = J(bl, cl, dl); kl = K4;
        }

        /* Select function and constant for right rounds */
        if (j < 16) {
            fr = J(br, cr, dr); kr = KP0;
        } else if (j < 32) {
            fr = I(br, cr, dr); kr = KP1;
        } else if (j < 48) {
            fr = H(br, cr, dr); kr = KP2;
        } else if (j < 64) {
            fr = G(br, cr, dr); kr = KP3;
        } else {
            fr = F(br, cr, dr); kr = KP4;
        }

        /* Left round */
        t = ROTL(al + fl + x[R[j]] + kl, S[j]) + el;
        al = el; el = dl; dl = ROTL(cl, 10); cl = bl; bl = t;

        /* Right round */
        t = ROTL(ar + fr + x[RP[j]] + kr, SP[j]) + er;
        ar = er; er = dr; dr = ROTL(cr, 10); cr = br; br = t;
    }

    /* Combine results */
    t = state[1] + cl + dr;
    state[1] = state[2] + dl + er;
    state[2] = state[3] + el + ar;
    state[3] = state[4] + al + br;
    state[4] = state[0] + bl + cr;
    state[0] = t;
}

void ripemd160_init(ripemd160_ctx *ctx) {
    memcpy(ctx->state, RIPEMD160_H0, sizeof(RIPEMD160_H0));
    ctx->count = 0;
    memset(ctx->buffer, 0, sizeof(ctx->buffer));
}

void ripemd160_update(ripemd160_ctx *ctx, const uint8_t *data, size_t len) {
    size_t buffer_fill = (ctx->count / 8) % 64;

    ctx->count += len * 8;

    /* Fill buffer first */
    if (buffer_fill > 0) {
        size_t to_copy = 64 - buffer_fill;
        if (to_copy > len) to_copy = len;
        memcpy(ctx->buffer + buffer_fill, data, to_copy);
        buffer_fill += to_copy;
        data += to_copy;
        len -= to_copy;

        if (buffer_fill == 64) {
            ripemd160_transform(ctx->state, ctx->buffer);
        }
    }

    /* Process full blocks */
    while (len >= 64) {
        ripemd160_transform(ctx->state, data);
        data += 64;
        len -= 64;
    }

    /* Store remaining */
    if (len > 0) {
        memcpy(ctx->buffer, data, len);
    }
}

void ripemd160_final(ripemd160_ctx *ctx, uint8_t hash[20]) {
    size_t buffer_fill = (ctx->count / 8) % 64;
    uint64_t bit_count = ctx->count;

    /* Padding: append 1 bit, then zeros, then 64-bit length */
    ctx->buffer[buffer_fill++] = 0x80;

    if (buffer_fill > 56) {
        /* Need another block */
        memset(ctx->buffer + buffer_fill, 0, 64 - buffer_fill);
        ripemd160_transform(ctx->state, ctx->buffer);
        buffer_fill = 0;
    }

    memset(ctx->buffer + buffer_fill, 0, 56 - buffer_fill);

    /* Append length in little-endian */
    ctx->buffer[56] = (uint8_t)(bit_count);
    ctx->buffer[57] = (uint8_t)(bit_count >> 8);
    ctx->buffer[58] = (uint8_t)(bit_count >> 16);
    ctx->buffer[59] = (uint8_t)(bit_count >> 24);
    ctx->buffer[60] = (uint8_t)(bit_count >> 32);
    ctx->buffer[61] = (uint8_t)(bit_count >> 40);
    ctx->buffer[62] = (uint8_t)(bit_count >> 48);
    ctx->buffer[63] = (uint8_t)(bit_count >> 56);

    ripemd160_transform(ctx->state, ctx->buffer);

    /* Output hash in little-endian */
    for (int i = 0; i < 5; i++) {
        hash[i * 4]     = (uint8_t)(ctx->state[i]);
        hash[i * 4 + 1] = (uint8_t)(ctx->state[i] >> 8);
        hash[i * 4 + 2] = (uint8_t)(ctx->state[i] >> 16);
        hash[i * 4 + 3] = (uint8_t)(ctx->state[i] >> 24);
    }
}

void ripemd160(uint8_t hash[20], const uint8_t *data, size_t len) {
    ripemd160_ctx ctx;
    ripemd160_init(&ctx);
    ripemd160_update(&ctx, data, len);
    ripemd160_final(&ctx, hash);
}

/* Hash160: RIPEMD160(SHA256(data)) */
void hash160(uint8_t hash[20], const uint8_t *data, size_t len) {
    uint8_t sha256_hash[32];
    sha256(sha256_hash, data, len);
    ripemd160(hash, sha256_hash, 32);
}
