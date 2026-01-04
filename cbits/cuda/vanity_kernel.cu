/*
 * vanity_kernel.cu - CUDA kernel implementation for vanity address generation
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "kernels.cuh"
#include "../vanity_gpu.h"

/* Constant memory for precomputed EC table */
__constant__ d_ge_t d_precomp_table[D_ECMULT_TABLE_SIZE];
__constant__ d_ge_t d_generator;

/* Field prime p = 2^256 - 2^32 - 977 */
__constant__ uint64_t d_field_p[4] = {
    0xFFFFFFFEFFFFFC2FULL,
    0xFFFFFFFFFFFFFFFFULL,
    0xFFFFFFFFFFFFFFFFULL,
    0xFFFFFFFFFFFFFFFFULL
};

/* Curve order n */
__constant__ uint64_t d_curve_n[4] = {
    0xBFD25E8CD0364141ULL,
    0xBAAEDCE6AF48A03BULL,
    0xFFFFFFFFFFFFFFFEULL,
    0xFFFFFFFFFFFFFFFFULL
};

/* ============================================================================
 * Device-side field arithmetic
 * ============================================================================ */

__device__ __forceinline__ uint64_t d_adc(uint64_t a, uint64_t b, uint64_t *carry) {
    uint64_t sum = a + b + *carry;
    *carry = (sum < a) || (sum == a && *carry) ? 1 : 0;
    return sum;
}

__device__ __forceinline__ uint64_t d_sbb(uint64_t a, uint64_t b, uint64_t *borrow) {
    uint64_t diff = a - b - *borrow;
    *borrow = (a < b + *borrow) ? 1 : 0;
    return diff;
}

__device__ __forceinline__ void d_fe_copy(d_fe_t *r, const d_fe_t *a) {
    r->d[0] = a->d[0]; r->d[1] = a->d[1];
    r->d[2] = a->d[2]; r->d[3] = a->d[3];
}

__device__ __forceinline__ int d_fe_is_zero(const d_fe_t *a) {
    return (a->d[0] | a->d[1] | a->d[2] | a->d[3]) == 0;
}

__device__ __forceinline__ int d_fe_is_odd(const d_fe_t *a) {
    return (int)(a->d[0] & 1);
}

__device__ __forceinline__ int d_fe_gte_p(const d_fe_t *a) {
    if (a->d[3] < d_field_p[3]) return 0;
    if (a->d[3] > d_field_p[3]) return 1;
    if (a->d[2] < d_field_p[2]) return 0;
    if (a->d[2] > d_field_p[2]) return 1;
    if (a->d[1] < d_field_p[1]) return 0;
    if (a->d[1] > d_field_p[1]) return 1;
    return a->d[0] >= d_field_p[0];
}

__device__ void d_fe_add(d_fe_t *r, const d_fe_t *a, const d_fe_t *b) {
    uint64_t carry = 0;
    r->d[0] = d_adc(a->d[0], b->d[0], &carry);
    r->d[1] = d_adc(a->d[1], b->d[1], &carry);
    r->d[2] = d_adc(a->d[2], b->d[2], &carry);
    r->d[3] = d_adc(a->d[3], b->d[3], &carry);

    if (carry || d_fe_gte_p(r)) {
        uint64_t borrow = 0;
        r->d[0] = d_sbb(r->d[0], d_field_p[0], &borrow);
        r->d[1] = d_sbb(r->d[1], d_field_p[1], &borrow);
        r->d[2] = d_sbb(r->d[2], d_field_p[2], &borrow);
        r->d[3] = d_sbb(r->d[3], d_field_p[3], &borrow);
    }
}

__device__ void d_fe_sub(d_fe_t *r, const d_fe_t *a, const d_fe_t *b) {
    uint64_t borrow = 0;
    r->d[0] = d_sbb(a->d[0], b->d[0], &borrow);
    r->d[1] = d_sbb(a->d[1], b->d[1], &borrow);
    r->d[2] = d_sbb(a->d[2], b->d[2], &borrow);
    r->d[3] = d_sbb(a->d[3], b->d[3], &borrow);

    if (borrow) {
        uint64_t carry = 0;
        r->d[0] = d_adc(r->d[0], d_field_p[0], &carry);
        r->d[1] = d_adc(r->d[1], d_field_p[1], &carry);
        r->d[2] = d_adc(r->d[2], d_field_p[2], &carry);
        r->d[3] = d_adc(r->d[3], d_field_p[3], &carry);
    }
}

__device__ void d_fe_neg(d_fe_t *r, const d_fe_t *a) {
    if (d_fe_is_zero(a)) {
        r->d[0] = r->d[1] = r->d[2] = r->d[3] = 0;
    } else {
        uint64_t borrow = 0;
        r->d[0] = d_sbb(d_field_p[0], a->d[0], &borrow);
        r->d[1] = d_sbb(d_field_p[1], a->d[1], &borrow);
        r->d[2] = d_sbb(d_field_p[2], a->d[2], &borrow);
        r->d[3] = d_sbb(d_field_p[3], a->d[3], &borrow);
    }
}

/* Multiplication using PTX intrinsics for 64-bit multiply-add */
__device__ void d_fe_mul(d_fe_t *r, const d_fe_t *a, const d_fe_t *b) {
    /* Schoolbook multiplication with secp256k1 reduction */
    uint64_t t[8] = {0};

    /* Use __umul64hi for high part of 64x64 multiply */
    #pragma unroll
    for (int i = 0; i < 4; i++) {
        uint64_t carry = 0;
        #pragma unroll
        for (int j = 0; j < 4; j++) {
            /* t[i+j] += a[i] * b[j] + carry */
            uint64_t lo = a->d[i] * b->d[j];
            uint64_t hi = __umul64hi(a->d[i], b->d[j]);

            uint64_t c1 = 0, c2 = 0;
            t[i + j] = d_adc(t[i + j], lo, &c1);
            t[i + j] = d_adc(t[i + j], carry, &c2);
            carry = hi + c1 + c2;
        }
        t[i + 4] = carry;
    }

    /* Reduce using 2^256 = 2^32 + 977 (mod p) */
    const uint64_t c_low = 0x1000003D1ULL;

    uint64_t carry = 0;
    uint64_t r0 = t[0], r1 = t[1], r2 = t[2], r3 = t[3];

    /* Add t[4..7] * c to r[0..3] */
    uint64_t lo, hi;

    lo = t[4] * c_low; hi = __umul64hi(t[4], c_low);
    r0 = d_adc(r0, lo, &carry); carry += hi;

    lo = t[5] * c_low; hi = __umul64hi(t[5], c_low);
    r1 = d_adc(r1, lo, &carry); carry += hi;

    lo = t[6] * c_low; hi = __umul64hi(t[6], c_low);
    r2 = d_adc(r2, lo, &carry); carry += hi;

    lo = t[7] * c_low; hi = __umul64hi(t[7], c_low);
    r3 = d_adc(r3, lo, &carry); carry += hi;

    /* Handle remaining carry */
    if (carry) {
        uint64_t c2 = 0;
        lo = carry * c_low;
        r0 = d_adc(r0, lo, &c2);
        r1 = d_adc(r1, 0, &c2);
        r2 = d_adc(r2, 0, &c2);
        r3 = d_adc(r3, 0, &c2);
    }

    r->d[0] = r0; r->d[1] = r1; r->d[2] = r2; r->d[3] = r3;

    /* Normalize */
    if (d_fe_gte_p(r)) {
        uint64_t borrow = 0;
        r->d[0] = d_sbb(r->d[0], d_field_p[0], &borrow);
        r->d[1] = d_sbb(r->d[1], d_field_p[1], &borrow);
        r->d[2] = d_sbb(r->d[2], d_field_p[2], &borrow);
        r->d[3] = d_sbb(r->d[3], d_field_p[3], &borrow);
    }
}

__device__ void d_fe_sqr(d_fe_t *r, const d_fe_t *a) {
    d_fe_mul(r, a, a);
}

/* Modular inverse using Fermat's little theorem */
__device__ void d_fe_inv(d_fe_t *r, const d_fe_t *a) {
    d_fe_t x2, x3, x6, x9, x11, x22, x44, x88, x176, x220, x223, t1;

    d_fe_sqr(&x2, a);
    d_fe_mul(&x2, &x2, a);

    d_fe_sqr(&x3, &x2);
    d_fe_mul(&x3, &x3, a);

    d_fe_sqr(&x6, &x3);
    for (int i = 1; i < 3; i++) d_fe_sqr(&x6, &x6);
    d_fe_mul(&x6, &x6, &x3);

    d_fe_sqr(&x9, &x6);
    for (int i = 1; i < 3; i++) d_fe_sqr(&x9, &x9);
    d_fe_mul(&x9, &x9, &x3);

    d_fe_sqr(&x11, &x9);
    for (int i = 1; i < 2; i++) d_fe_sqr(&x11, &x11);
    d_fe_mul(&x11, &x11, &x2);

    d_fe_sqr(&x22, &x11);
    for (int i = 1; i < 11; i++) d_fe_sqr(&x22, &x22);
    d_fe_mul(&x22, &x22, &x11);

    d_fe_sqr(&x44, &x22);
    for (int i = 1; i < 22; i++) d_fe_sqr(&x44, &x44);
    d_fe_mul(&x44, &x44, &x22);

    d_fe_sqr(&x88, &x44);
    for (int i = 1; i < 44; i++) d_fe_sqr(&x88, &x88);
    d_fe_mul(&x88, &x88, &x44);

    d_fe_sqr(&x176, &x88);
    for (int i = 1; i < 88; i++) d_fe_sqr(&x176, &x176);
    d_fe_mul(&x176, &x176, &x88);

    d_fe_sqr(&x220, &x176);
    for (int i = 1; i < 44; i++) d_fe_sqr(&x220, &x220);
    d_fe_mul(&x220, &x220, &x44);

    d_fe_sqr(&x223, &x220);
    for (int i = 1; i < 3; i++) d_fe_sqr(&x223, &x223);
    d_fe_mul(&x223, &x223, &x3);

    d_fe_sqr(&t1, &x223);
    for (int i = 1; i < 23; i++) d_fe_sqr(&t1, &t1);
    d_fe_mul(&t1, &t1, &x22);
    for (int i = 0; i < 5; i++) d_fe_sqr(&t1, &t1);
    d_fe_mul(&t1, &t1, a);
    for (int i = 0; i < 3; i++) d_fe_sqr(&t1, &t1);
    d_fe_mul(&t1, &t1, &x2);
    for (int i = 0; i < 2; i++) d_fe_sqr(&t1, &t1);
    d_fe_mul(r, &t1, a);
}

/* ============================================================================
 * Device-side group operations
 * ============================================================================ */

__device__ __forceinline__ void d_gej_set_infinity(d_gej_t *r) {
    r->x.d[0] = r->x.d[1] = r->x.d[2] = r->x.d[3] = 0;
    r->y.d[0] = 1; r->y.d[1] = r->y.d[2] = r->y.d[3] = 0;
    r->z.d[0] = r->z.d[1] = r->z.d[2] = r->z.d[3] = 0;
    r->infinity = 1;
}

__device__ __forceinline__ int d_gej_is_infinity(const d_gej_t *a) {
    return a->infinity || d_fe_is_zero(&a->z);
}

__device__ void d_gej_set_ge(d_gej_t *r, const d_ge_t *a) {
    d_fe_copy(&r->x, &a->x);
    d_fe_copy(&r->y, &a->y);
    r->z.d[0] = 1; r->z.d[1] = r->z.d[2] = r->z.d[3] = 0;
    r->infinity = 0;
}

__device__ void d_gej_double(d_gej_t *r, const d_gej_t *a) {
    if (d_gej_is_infinity(a)) {
        d_gej_set_infinity(r);
        return;
    }

    d_fe_t s, m, t, x3, y3, z3, y2, y4;

    d_fe_sqr(&y2, &a->y);
    d_fe_mul(&s, &a->x, &y2);
    d_fe_add(&s, &s, &s);
    d_fe_add(&s, &s, &s);

    d_fe_sqr(&m, &a->x);
    d_fe_add(&t, &m, &m);
    d_fe_add(&m, &t, &m);

    d_fe_sqr(&x3, &m);
    d_fe_sub(&x3, &x3, &s);
    d_fe_sub(&x3, &x3, &s);

    d_fe_sqr(&y4, &y2);

    d_fe_sub(&t, &s, &x3);
    d_fe_mul(&y3, &m, &t);
    d_fe_add(&t, &y4, &y4);
    d_fe_add(&t, &t, &t);
    d_fe_add(&t, &t, &t);
    d_fe_sub(&y3, &y3, &t);

    d_fe_mul(&z3, &a->y, &a->z);
    d_fe_add(&z3, &z3, &z3);

    d_fe_copy(&r->x, &x3);
    d_fe_copy(&r->y, &y3);
    d_fe_copy(&r->z, &z3);
    r->infinity = 0;
}

__device__ void d_gej_add_ge(d_gej_t *r, const d_gej_t *a, const d_ge_t *b) {
    if (d_gej_is_infinity(a)) {
        d_gej_set_ge(r, b);
        return;
    }

    d_fe_t z12, z13, u2, s2, h, rr, h2, h3, u1h2, x3, y3, z3, t;

    d_fe_sqr(&z12, &a->z);
    d_fe_mul(&z13, &z12, &a->z);
    d_fe_mul(&u2, &b->x, &z12);
    d_fe_mul(&s2, &b->y, &z13);
    d_fe_sub(&h, &u2, &a->x);
    d_fe_sub(&rr, &s2, &a->y);

    if (d_fe_is_zero(&h)) {
        if (d_fe_is_zero(&rr)) {
            d_gej_t temp;
            d_gej_set_ge(&temp, b);
            d_gej_double(r, &temp);
            return;
        } else {
            d_gej_set_infinity(r);
            return;
        }
    }

    d_fe_sqr(&h2, &h);
    d_fe_mul(&h3, &h2, &h);
    d_fe_mul(&u1h2, &a->x, &h2);

    d_fe_sqr(&x3, &rr);
    d_fe_sub(&x3, &x3, &h3);
    d_fe_sub(&x3, &x3, &u1h2);
    d_fe_sub(&x3, &x3, &u1h2);

    d_fe_sub(&t, &u1h2, &x3);
    d_fe_mul(&y3, &rr, &t);
    d_fe_mul(&t, &a->y, &h3);
    d_fe_sub(&y3, &y3, &t);

    d_fe_mul(&z3, &a->z, &h);

    d_fe_copy(&r->x, &x3);
    d_fe_copy(&r->y, &y3);
    d_fe_copy(&r->z, &z3);
    r->infinity = 0;
}

__device__ void d_gej_to_ge(d_ge_t *r, const d_gej_t *a) {
    if (d_gej_is_infinity(a)) {
        r->x.d[0] = r->x.d[1] = r->x.d[2] = r->x.d[3] = 0;
        r->y.d[0] = r->y.d[1] = r->y.d[2] = r->y.d[3] = 0;
        return;
    }

    d_fe_t z_inv, z2, z3;
    d_fe_inv(&z_inv, &a->z);
    d_fe_sqr(&z2, &z_inv);
    d_fe_mul(&z3, &z2, &z_inv);
    d_fe_mul(&r->x, &a->x, &z2);
    d_fe_mul(&r->y, &a->y, &z3);
}

/* ============================================================================
 * Device-side scalar multiplication
 * ============================================================================ */

__device__ void d_ecmult_gen(d_gej_t *r, const uint8_t scalar[32]) {
    d_gej_set_infinity(r);

    /* Process scalar from MSB to LSB, 4 bits at a time */
    for (int byte_idx = 0; byte_idx < 32; byte_idx++) {
        uint8_t byte_val = scalar[byte_idx];

        /* High nibble */
        if (!d_gej_is_infinity(r)) {
            d_gej_double(r, r);
            d_gej_double(r, r);
            d_gej_double(r, r);
            d_gej_double(r, r);
        }

        int window = (byte_val >> 4) & 0xF;
        if (window != 0) {
            if (window & 1) {
                d_gej_add_ge(r, r, &d_precomp_table[window >> 1]);
            } else {
                d_gej_add_ge(r, r, &d_precomp_table[(window - 1) >> 1]);
                d_gej_add_ge(r, r, &d_generator);
            }
        }

        /* Low nibble */
        if (!d_gej_is_infinity(r)) {
            d_gej_double(r, r);
            d_gej_double(r, r);
            d_gej_double(r, r);
            d_gej_double(r, r);
        }

        window = byte_val & 0xF;
        if (window != 0) {
            if (window & 1) {
                d_gej_add_ge(r, r, &d_precomp_table[window >> 1]);
            } else {
                d_gej_add_ge(r, r, &d_precomp_table[(window - 1) >> 1]);
                d_gej_add_ge(r, r, &d_generator);
            }
        }
    }
}

/* ============================================================================
 * Kernel implementations
 * ============================================================================ */

__global__ void kernel_init_rng(curandState *states, uint64_t seed, uint32_t num_threads) {
    uint32_t idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx < num_threads) {
        curand_init(seed, idx, 0, &states[idx]);
    }
}

__global__ void kernel_derive_pubkeys(const uint8_t *scalars, uint8_t *pubkeys, uint32_t count) {
    uint32_t idx = blockIdx.x * blockDim.x + threadIdx.x;
    if (idx >= count) return;

    d_gej_t pk_jac;
    d_ge_t pk;

    d_ecmult_gen(&pk_jac, scalars + idx * 32);
    d_gej_to_ge(&pk, &pk_jac);

    /* Serialize compressed pubkey */
    uint8_t *out = pubkeys + idx * 33;
    out[0] = d_fe_is_odd(&pk.y) ? 0x03 : 0x02;

    /* X coordinate big-endian */
    for (int i = 0; i < 4; i++) {
        uint64_t limb = pk.x.d[3 - i];
        for (int j = 0; j < 8; j++) {
            out[1 + i * 8 + j] = (uint8_t)(limb >> (56 - j * 8));
        }
    }
}

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
) {
    uint32_t idx = blockIdx.x * blockDim.x + threadIdx.x;
    curandState local_state = rng_states[idx];

    for (uint32_t iter = 0; iter < iterations_per_thread; iter++) {
        /* Generate random 256-bit scalar */
        uint8_t scalar[32];
        for (int i = 0; i < 8; i++) {
            uint32_t r = curand(&local_state);
            scalar[i * 4]     = (uint8_t)(r >> 24);
            scalar[i * 4 + 1] = (uint8_t)(r >> 16);
            scalar[i * 4 + 2] = (uint8_t)(r >> 8);
            scalar[i * 4 + 3] = (uint8_t)(r);
        }

        /* Derive public key */
        d_gej_t pk_jac;
        d_ge_t pk;
        d_ecmult_gen(&pk_jac, scalar);
        d_gej_to_ge(&pk, &pk_jac);

        /* Serialize pubkey */
        uint8_t pubkey[33];
        pubkey[0] = d_fe_is_odd(&pk.y) ? 0x03 : 0x02;
        for (int i = 0; i < 4; i++) {
            uint64_t limb = pk.x.d[3 - i];
            for (int j = 0; j < 8; j++) {
                pubkey[1 + i * 8 + j] = (uint8_t)(limb >> (56 - j * 8));
            }
        }

        /* TODO: Encode address and match pattern */
        /* For now, just check if first char matches first pattern char */
        /* Full implementation requires GPU-side hashing and encoding */

        /* Simplified prefix matching for demonstration */
        int match = 1;  /* Placeholder - real matching TBD */

        if (match && pattern_len > 0) {
            /* Check if we should record this match */
            uint32_t slot = atomicAdd(match_count, 1);
            if (slot < max_matches) {
                memcpy(matches[slot].scalar, scalar, 32);
                memcpy(matches[slot].pubkey, pubkey, 33);
                matches[slot].address_len = 0;  /* TBD */
                matches[slot].thread_idx = idx;
                matches[slot].iteration = iter;
            }
            /* Early exit on first match */
            break;
        }
    }

    rng_states[idx] = local_state;
}

/* ============================================================================
 * Host-side API implementation
 * ============================================================================ */

cudaError_t vanity_cuda_init_resources(int device_id) {
    if (device_id >= 0) {
        cudaError_t err = cudaSetDevice(device_id);
        if (err != cudaSuccess) return err;
    }
    return cudaSuccess;
}

cudaError_t vanity_cuda_init_precomp_table(void) {
    /* Initialize precomputation table on host */
    ecmult_gen_context ctx;
    ecmult_gen_context_init(&ctx);

    /* Copy to device constant memory */
    d_ge_t host_table[D_ECMULT_TABLE_SIZE];
    for (int i = 0; i < D_ECMULT_TABLE_SIZE; i++) {
        /* Convert host ge_t to d_ge_t */
        memcpy(&host_table[i], &ctx.table[i], sizeof(d_ge_t));
    }

    cudaError_t err = cudaMemcpyToSymbol(d_precomp_table, host_table,
                                          sizeof(host_table));
    if (err != cudaSuccess) return err;

    /* Copy generator point */
    d_ge_t host_gen;
    memcpy(&host_gen, &GE_G, sizeof(d_ge_t));
    err = cudaMemcpyToSymbol(d_generator, &host_gen, sizeof(host_gen));

    return err;
}
