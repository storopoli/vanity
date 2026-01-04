/*
 * kernels.cuh - CUDA kernel declarations for vanity address generation
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_CUDA_KERNELS_H
#define VANITY_CUDA_KERNELS_H

#include <cuda_runtime.h>
#include <curand_kernel.h>

#include "../common.h"
#include "../secp256k1/field.h"
#include "../secp256k1/group.h"
#include "../secp256k1/scalar.h"

/* Device-side field element */
typedef struct {
    uint64_t d[4];
} d_fe_t;

/* Device-side group element (Jacobian) */
typedef struct {
    d_fe_t x, y, z;
    int infinity;
} d_gej_t;

/* Device-side affine point */
typedef struct {
    d_fe_t x, y;
} d_ge_t;

/* Precomputed table in constant memory */
#define D_ECMULT_TABLE_SIZE 8
extern __constant__ d_ge_t d_precomp_table[D_ECMULT_TABLE_SIZE];

/* Generator point in constant memory */
extern __constant__ d_ge_t d_generator;

/* Match result for GPU -> CPU transfer */
typedef struct {
    uint8_t scalar[32];
    uint8_t pubkey[33];
    uint8_t address[74];
    uint8_t address_len;
    uint32_t thread_idx;
    uint32_t iteration;
} d_match_t;

/*
 * Initialize CUDA resources
 * Must be called before any kernel launches
 */
cudaError_t vanity_cuda_init_resources(int device_id);

/*
 * Copy precomputed table to constant memory
 */
cudaError_t vanity_cuda_init_precomp_table(void);

/*
 * Initialize per-thread RNG states
 */
__global__ void kernel_init_rng(
    curandState *states,
    uint64_t seed,
    uint32_t num_threads
);

/*
 * Full vanity search kernel
 * Each thread generates keys, derives pubkeys, encodes addresses, and matches
 */
__global__ void kernel_vanity_search(
    curandState *rng_states,
    const uint8_t *pattern,
    uint32_t pattern_len,
    uint8_t address_type,
    uint8_t network,
    int case_sensitive,
    d_match_t *matches,
    uint32_t *match_count,
    uint32_t max_matches,
    uint32_t iterations_per_thread
);

/*
 * Batch public key derivation kernel
 * For benchmarking/testing - derives pubkeys from provided scalars
 */
__global__ void kernel_derive_pubkeys(
    const uint8_t *scalars,
    uint8_t *pubkeys,
    uint32_t count
);

/*
 * Device-side functions (declared with __device__ in .cu file)
 */

#endif /* VANITY_CUDA_KERNELS_H */
