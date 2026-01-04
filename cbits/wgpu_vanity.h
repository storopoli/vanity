/**
 * wgpu_vanity.h - WGPU/Metal backend for vanity address generation
 *
 * Cross-platform GPU compute via wgpu-native.
 * Supports Metal (macOS), Vulkan (Linux/Windows), DirectX (Windows).
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef WGPU_VANITY_H
#define WGPU_VANITY_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Error codes */
typedef enum {
    WGPU_ERROR_NONE = 0,
    WGPU_ERROR_NO_ADAPTER = 1,
    WGPU_ERROR_NO_DEVICE = 2,
    WGPU_ERROR_SHADER_COMPILE = 3,
    WGPU_ERROR_PIPELINE_CREATE = 4,
    WGPU_ERROR_BUFFER_CREATE = 5,
    WGPU_ERROR_OUT_OF_MEMORY = 6,
    WGPU_ERROR_UNKNOWN = 7
} WGPUVanityError;

/* GPU search configuration */
typedef struct {
    uint32_t pattern_len;      /* Length of pattern to match */
    uint32_t address_type;     /* 0=P2PKH, 1=P2WPKH, 2=P2TR */
    uint32_t network;          /* 0=Mainnet, 1=Testnet */
    uint32_t batch_size;       /* Keys per GPU dispatch */
    uint32_t match_mode;       /* 0=prefix, 1=suffix, 2=contains */
    uint32_t _padding[3];      /* Alignment padding */
} VanityWGPUConfig;

/* Result from GPU search */
typedef struct {
    uint32_t scalar[8];        /* 256-bit scalar (private key) */
    uint32_t pubkey_x[8];      /* X coordinate of public key */
    uint32_t pubkey_y_parity;  /* 0 for even Y, 1 for odd Y */
    uint8_t  address[76];      /* Address string (max 74 + padding) */
    uint32_t address_len;      /* Actual length of address */
    uint32_t batch_idx;        /* Index within batch */
    uint32_t found;            /* 1 if this is a valid result */
    uint32_t _padding;         /* Alignment */
} VanityWGPUResult;

/* Device info */
typedef struct {
    char name[256];            /* Device name */
    char backend[32];          /* Backend type (Metal, Vulkan, etc.) */
    uint64_t memory_bytes;     /* Available memory in bytes */
    uint32_t max_workgroup_size;
    uint32_t max_compute_units;
} WGPUDeviceInfo;

/**
 * Initialize WGPU subsystem.
 * Must be called before any other WGPU functions.
 *
 * @return 0 on success, error code on failure
 */
int vanity_wgpu_init(void);

/**
 * Cleanup WGPU resources.
 * Call when done using WGPU.
 */
void vanity_wgpu_cleanup(void);

/**
 * Get the name of the selected GPU device.
 *
 * @param name_buf Buffer to receive device name
 * @param buf_size Size of buffer
 * @return 0 on success, error code on failure
 */
int vanity_wgpu_get_device_name(char* name_buf, size_t buf_size);

/**
 * Get detailed device info.
 *
 * @param info Pointer to device info struct to fill
 * @return 0 on success, error code on failure
 */
int vanity_wgpu_get_device_info(WGPUDeviceInfo* info);

/**
 * Create compute pipeline from WGSL shader code.
 *
 * @param shader_code WGSL shader source code
 * @param code_len Length of shader code
 * @return 0 on success, error code on failure
 */
int vanity_wgpu_create_pipeline(const char* shader_code, size_t code_len);

/**
 * Destroy the compute pipeline.
 */
void vanity_wgpu_destroy_pipeline(void);

/**
 * Launch vanity search computation on GPU.
 *
 * @param config Search configuration
 * @param pattern Pattern bytes to match
 * @param base_scalar Starting scalar (256-bit as 8 x uint32)
 * @return 0 on success, error code on failure
 */
int vanity_wgpu_launch(const VanityWGPUConfig* config,
                       const uint8_t* pattern,
                       const uint32_t* base_scalar);

/**
 * Get results from the last GPU computation.
 *
 * @param results Array to receive results
 * @param max_results Maximum number of results to retrieve
 * @return Number of results found
 */
uint32_t vanity_wgpu_get_results(VanityWGPUResult* results, uint32_t max_results);

/**
 * Get total number of keys tested.
 *
 * @return Total attempt count
 */
uint64_t vanity_wgpu_get_attempt_count(void);

/**
 * Get the last error code.
 *
 * @return Last error code
 */
int vanity_wgpu_get_last_error(void);

/**
 * Check if WGPU backend is available on this system.
 *
 * @return 1 if available, 0 if not
 */
int vanity_wgpu_is_available(void);

#ifdef __cplusplus
}
#endif

#endif /* WGPU_VANITY_H */
