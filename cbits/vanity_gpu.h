/*
 * vanity_gpu.h - Public GPU API for Bitcoin vanity address generation
 *
 * This is the main header for FFI bindings from Haskell.
 * Provides unified interface for CUDA and WGPU backends.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_GPU_H
#define VANITY_GPU_H

#include "common.h"

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Match result structure - returned when a vanity address is found
 */
typedef struct {
    uint8_t scalar[32];         /* Secret key (32 bytes, big-endian) */
    uint8_t pubkey[33];         /* Compressed public key (33 bytes) */
    uint8_t address[74];        /* Address string (null-terminated) */
    uint8_t address_len;        /* Actual address length */
    uint32_t batch_idx;         /* Index within batch where match was found */
} vanity_match_t;

/*
 * GPU context (opaque handle)
 */
typedef struct vanity_gpu_ctx vanity_gpu_ctx_t;

/*
 * GPU device information
 */
typedef struct {
    char name[256];             /* Device name */
    uint64_t memory_total;      /* Total memory in bytes */
    uint64_t memory_free;       /* Free memory in bytes */
    int compute_capability;     /* CUDA compute capability (major * 10 + minor) */
    int multiprocessors;        /* Number of SMs/compute units */
} vanity_gpu_info_t;

/*
 * Search configuration
 */
typedef struct {
    uint8_t address_type;       /* VANITY_ADDR_P2PKH, P2WPKH, or P2TR */
    uint8_t network;            /* VANITY_NET_MAINNET or VANITY_NET_TESTNET */
    const char *pattern;        /* Prefix/suffix pattern to match */
    size_t pattern_len;         /* Pattern length */
    int case_sensitive;         /* 1 for case-sensitive matching */
    uint32_t batch_size;        /* Keys per batch (0 = use default) */
    uint32_t max_results;       /* Max matches to return per batch */
} vanity_search_config_t;

/*
 * Batch result
 */
typedef struct {
    vanity_match_t *matches;    /* Array of matches */
    uint32_t match_count;       /* Number of matches found */
    uint64_t keys_checked;      /* Total keys checked in this batch */
} vanity_batch_result_t;

/* ============================================================================
 * CUDA Backend API
 * ============================================================================ */

#ifdef CUDA_ENABLED

/*
 * Initialize CUDA backend
 *
 * @param ctx       Pointer to receive context handle
 * @param device_id CUDA device ID (use -1 for default)
 * @return          VANITY_OK on success, error code otherwise
 */
int vanity_cuda_init(vanity_gpu_ctx_t **ctx, int device_id);

/*
 * Get CUDA device information
 */
int vanity_cuda_get_info(vanity_gpu_ctx_t *ctx, vanity_gpu_info_t *info);

/*
 * Launch a batch search on CUDA
 *
 * @param ctx       GPU context
 * @param config    Search configuration
 * @param result    Output batch result
 * @return          VANITY_OK on success
 */
int vanity_cuda_search_batch(
    vanity_gpu_ctx_t *ctx,
    const vanity_search_config_t *config,
    vanity_batch_result_t *result
);

/*
 * Free batch result resources
 */
void vanity_cuda_free_result(vanity_batch_result_t *result);

/*
 * Cleanup CUDA backend
 */
void vanity_cuda_cleanup(vanity_gpu_ctx_t *ctx);

#endif /* CUDA_ENABLED */

/* ============================================================================
 * WGPU Backend API
 * ============================================================================ */

#ifdef WGPU_ENABLED

/*
 * Initialize WGPU backend
 *
 * @param ctx       Pointer to receive context handle
 * @param backend   Preferred backend: "vulkan", "metal", "dx12", or NULL for auto
 * @return          VANITY_OK on success, error code otherwise
 */
int vanity_wgpu_init(vanity_gpu_ctx_t **ctx, const char *backend);

/*
 * Get WGPU device information
 */
int vanity_wgpu_get_info(vanity_gpu_ctx_t *ctx, vanity_gpu_info_t *info);

/*
 * Launch a batch search on WGPU
 */
int vanity_wgpu_search_batch(
    vanity_gpu_ctx_t *ctx,
    const vanity_search_config_t *config,
    vanity_batch_result_t *result
);

/*
 * Free batch result resources
 */
void vanity_wgpu_free_result(vanity_batch_result_t *result);

/*
 * Cleanup WGPU backend
 */
void vanity_wgpu_cleanup(vanity_gpu_ctx_t *ctx);

#endif /* WGPU_ENABLED */

/* ============================================================================
 * CPU Backend API (for testing/comparison)
 * ============================================================================ */

/*
 * Initialize CPU backend (always available)
 */
int vanity_cpu_init(vanity_gpu_ctx_t **ctx);

/*
 * Launch a batch search on CPU
 */
int vanity_cpu_search_batch(
    vanity_gpu_ctx_t *ctx,
    const vanity_search_config_t *config,
    vanity_batch_result_t *result
);

/*
 * Free batch result resources
 */
void vanity_cpu_free_result(vanity_batch_result_t *result);

/*
 * Cleanup CPU backend
 */
void vanity_cpu_cleanup(vanity_gpu_ctx_t *ctx);

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

/*
 * Derive public key from secret key (CPU implementation)
 * Useful for verification
 */
int vanity_derive_pubkey(
    uint8_t pubkey[33],
    const uint8_t scalar[32]
);

/*
 * Encode address from public key (CPU implementation)
 */
int vanity_encode_address(
    char *out,
    size_t *out_len,
    const uint8_t pubkey[33],
    uint8_t address_type,
    uint8_t network
);

/*
 * Check if a pattern matches an address
 */
int vanity_match_pattern(
    const char *address,
    const char *pattern,
    size_t pattern_len,
    int case_sensitive
);

#ifdef __cplusplus
}
#endif

#endif /* VANITY_GPU_H */
