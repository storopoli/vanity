/*
 * common.h - Common types and constants for vanity GPU backends
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_COMMON_H
#define VANITY_COMMON_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>

#ifdef __CUDACC__
#define VANITY_INLINE __device__ __host__ __forceinline__
#define VANITY_DEVICE __device__
#define VANITY_HOST __host__
#define VANITY_GLOBAL __global__
#define VANITY_CONSTANT __constant__
#else
#define VANITY_INLINE static inline
#define VANITY_DEVICE
#define VANITY_HOST
#define VANITY_GLOBAL
#define VANITY_CONSTANT
#endif

/* secp256k1 curve constants */

/* Field prime: p = 2^256 - 2^32 - 977 */
/* p = 0xFFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2F */

/* Curve order: n */
/* n = 0xFFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE BAAEDCE6 AF48A03B BFD25E8C D0364141 */

/* Address types */
#define VANITY_ADDR_P2PKH   0
#define VANITY_ADDR_P2WPKH  1
#define VANITY_ADDR_P2TR    2

/* Network types */
#define VANITY_NET_MAINNET  0
#define VANITY_NET_TESTNET  1

/* Error codes */
#define VANITY_OK           0
#define VANITY_ERR_INIT    -1
#define VANITY_ERR_MEMORY  -2
#define VANITY_ERR_LAUNCH  -3
#define VANITY_ERR_PATTERN -4

/* Maximum address length (Bech32 addresses are longest) */
#define VANITY_MAX_ADDR_LEN 74

/* Batch configuration defaults */
#define VANITY_DEFAULT_BATCH_SIZE (1 << 20)  /* 1M keys per batch */
#define VANITY_THREADS_PER_BLOCK  256
#define VANITY_MAX_RESULTS        1024

#endif /* VANITY_COMMON_H */
