// ecmult_only.wgsl - Minimal EC scalar multiplication shader
//
// Returns raw public key points to host for CPU-side hashing/encoding.
// This is the bottleneck (93% of compute) so we still get huge speedup.
//
// Copyright (c) 2025 Jose Storopoli
// MIT License

// ============================================================================
// Types
// ============================================================================

// 256-bit field element as 8x32-bit limbs (little-endian)
struct Fe {
    d: array<u32, 8>,
}

// Jacobian projective point
struct Gej {
    x: Fe,
    y: Fe,
    z: Fe,
    infinity: u32,
}

// Affine point
struct Ge {
    x: Fe,
    y: Fe,
}

// Result: compressed public key (33 bytes as 9 u32s)
struct PubKeyResult {
    prefix: u32,       // 0x02 or 0x03
    x: array<u32, 8>,  // X coordinate
    batch_idx: u32,    // Index in batch
}

// Configuration
struct Config {
    batch_size: u32,
    _pad0: u32,
    _pad1: u32,
    _pad2: u32,
}

// ============================================================================
// Bindings
// ============================================================================

@group(0) @binding(0) var<uniform> config: Config;
@group(0) @binding(1) var<storage, read> base_scalar: array<u32, 8>;
@group(0) @binding(2) var<storage, read_write> pubkeys: array<PubKeyResult>;

// ============================================================================
// Constants
// ============================================================================

// Field prime p = 2^256 - 2^32 - 977
const FE_P: array<u32, 8> = array<u32, 8>(
    0xFFFFFC2Fu, 0xFFFFFFFEu, 0xFFFFFFFFu, 0xFFFFFFFFu,
    0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu
);

// Generator point G
const GEN_X: array<u32, 8> = array<u32, 8>(
    0x16F81798u, 0x59F2815Bu, 0x2DCE28D9u, 0x029BFCDBu,
    0xCE870B07u, 0x55A06295u, 0xF9DCBBACu, 0x79BE667Eu
);

const GEN_Y: array<u32, 8> = array<u32, 8>(
    0xFB10D4B8u, 0x9C47D08Fu, 0xA6855419u, 0xFD17B448u,
    0x0E1108A8u, 0x5DA4FBFCu, 0x26A3C465u, 0x483ADA77u
);

// Curve order n
const SECP256K1_N: array<u32, 8> = array<u32, 8>(
    0xD0364141u, 0xBFD25E8Cu, 0xAF48A03Bu, 0xBAAEDCE6u,
    0xFFFFFFFEu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu
);

// ============================================================================
// Field Arithmetic
// ============================================================================

fn fe_zero() -> Fe {
    return Fe(array<u32, 8>(0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u));
}

fn fe_one() -> Fe {
    return Fe(array<u32, 8>(1u, 0u, 0u, 0u, 0u, 0u, 0u, 0u));
}

fn fe_from_array(a: array<u32, 8>) -> Fe {
    return Fe(a);
}

fn fe_add(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var carry: u32 = 0u;
    for (var i: u32 = 0u; i < 8u; i++) {
        let sum = a.d[i] + b.d[i] + carry;
        r.d[i] = sum;
        carry = select(0u, 1u, sum < a.d[i] || (carry > 0u && sum <= a.d[i]));
    }
    return fe_reduce(r);
}

fn fe_sub(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var borrow: u32 = 0u;
    for (var i: u32 = 0u; i < 8u; i++) {
        let diff = a.d[i] - b.d[i] - borrow;
        borrow = select(0u, 1u, a.d[i] < b.d[i] + borrow);
        r.d[i] = diff;
    }
    if (borrow > 0u) {
        // Add p back
        var carry: u32 = 0u;
        for (var i: u32 = 0u; i < 8u; i++) {
            let sum = r.d[i] + FE_P[i] + carry;
            carry = select(0u, 1u, sum < r.d[i]);
            r.d[i] = sum;
        }
    }
    return r;
}

fn fe_reduce(a: Fe) -> Fe {
    // Check if a >= p
    var ge_p: bool = true;
    for (var i: i32 = 7; i >= 0; i--) {
        let idx = u32(i);
        if (a.d[idx] < FE_P[idx]) {
            ge_p = false;
            break;
        }
        if (a.d[idx] > FE_P[idx]) {
            break;
        }
    }
    if (!ge_p) {
        return a;
    }
    // Subtract p
    return fe_sub(a, fe_from_array(FE_P));
}

fn fe_mul(a: Fe, b: Fe) -> Fe {
    // Schoolbook multiplication with reduction
    var t: array<u32, 16>;
    for (var i: u32 = 0u; i < 16u; i++) {
        t[i] = 0u;
    }

    for (var i: u32 = 0u; i < 8u; i++) {
        var carry: u32 = 0u;
        for (var j: u32 = 0u; j < 8u; j++) {
            let idx = i + j;
            let prod = u64(a.d[i]) * u64(b.d[j]) + u64(t[idx]) + u64(carry);
            t[idx] = u32(prod & 0xFFFFFFFFu);
            carry = u32(prod >> 32u);
        }
        t[i + 8u] = carry;
    }

    // Reduce mod p
    return fe_reduce_wide(t);
}

fn fe_reduce_wide(t: array<u32, 16>) -> Fe {
    // p = 2^256 - 2^32 - 977
    // So 2^256 ≡ 2^32 + 977 (mod p)
    // c = 0x1000003D1 = 2^32 + 977
    const C_LOW: u32 = 0x3D1u;

    var r: array<u32, 9>;
    for (var i: u32 = 0u; i < 8u; i++) {
        r[i] = t[i];
    }
    r[8] = 0u;

    // Reduce high limbs
    for (var i: u32 = 8u; i < 16u; i++) {
        if (t[i] == 0u) { continue; }
        let hi = t[i];

        // Multiply by c and add to result
        var carry: u64 = u64(hi) * u64(C_LOW);
        for (var j: u32 = 0u; j < 9u - (i - 8u); j++) {
            let idx = i - 8u + j;
            if (idx >= 9u) { break; }
            let sum = u64(r[idx]) + (carry & 0xFFFFFFFFu);
            r[idx] = u32(sum & 0xFFFFFFFFu);
            carry = (carry >> 32u) + (sum >> 32u);
            if (j == 0u) {
                carry += u64(hi);  // Add the 2^32 part
            }
        }
    }

    // Final reduction if needed
    if (r[8] > 0u) {
        var carry: u64 = u64(r[8]) * u64(C_LOW) + u64(r[8]);
        for (var i: u32 = 0u; i < 8u; i++) {
            let sum = u64(r[i]) + (carry & 0xFFFFFFFFu);
            r[i] = u32(sum & 0xFFFFFFFFu);
            carry = carry >> 32u;
            if (i == 0u && r[8] > 0u) {
                // Nothing extra
            }
        }
        r[8] = 0u;
    }

    var result: Fe;
    for (var i: u32 = 0u; i < 8u; i++) {
        result.d[i] = r[i];
    }
    return fe_reduce(result);
}

fn fe_sqr(a: Fe) -> Fe {
    return fe_mul(a, a);
}

fn fe_inv(a: Fe) -> Fe {
    // Fermat's little theorem: a^(-1) = a^(p-2) mod p
    var result = fe_one();
    var base = a;

    // p - 2 in binary (we need to compute a^(p-2))
    // This is a simplified version using square-and-multiply
    for (var i: u32 = 0u; i < 256u; i++) {
        let word_idx = i / 32u;
        let bit_idx = i % 32u;

        // p-2 bits (we compute this)
        var p_minus_2: array<u32, 8>;
        p_minus_2[0] = 0xFFFFFC2Du;
        p_minus_2[1] = 0xFFFFFFFEu;
        p_minus_2[2] = 0xFFFFFFFFu;
        p_minus_2[3] = 0xFFFFFFFFu;
        p_minus_2[4] = 0xFFFFFFFFu;
        p_minus_2[5] = 0xFFFFFFFFu;
        p_minus_2[6] = 0xFFFFFFFFu;
        p_minus_2[7] = 0xFFFFFFFFu;

        if ((p_minus_2[word_idx] >> bit_idx) & 1u) == 1u {
            result = fe_mul(result, base);
        }
        base = fe_sqr(base);
    }
    return result;
}

fn fe_is_odd(a: Fe) -> bool {
    return (a.d[0] & 1u) == 1u;
}

// ============================================================================
// EC Group Operations
// ============================================================================

fn gej_infinity() -> Gej {
    return Gej(fe_zero(), fe_one(), fe_zero(), 1u);
}

fn gej_from_ge(p: Ge) -> Gej {
    return Gej(p.x, p.y, fe_one(), 0u);
}

fn gej_double(a: Gej) -> Gej {
    if (a.infinity != 0u) {
        return a;
    }

    // t1 = x^2
    let t1 = fe_sqr(a.x);
    // t2 = y^2
    let t2 = fe_sqr(a.y);
    // t3 = y^2^2
    let t3 = fe_sqr(t2);
    // t4 = (x + y^2)^2 - x^2 - y^4 = 2*x*y^2
    let t4_a = fe_add(a.x, t2);
    let t4_b = fe_sqr(t4_a);
    let t4_c = fe_sub(t4_b, t1);
    let t4 = fe_sub(t4_c, t3);
    // S = 2 * t4
    let s = fe_add(t4, t4);
    // M = 3 * x^2 (a=0 for secp256k1)
    let m = fe_add(fe_add(t1, t1), t1);
    // x3 = M^2 - 2*S
    let x3_a = fe_sqr(m);
    let x3 = fe_sub(x3_a, fe_add(s, s));
    // y3 = M * (S - x3) - 8*y^4
    let y3_a = fe_sub(s, x3);
    let y3_b = fe_mul(m, y3_a);
    let t3_2 = fe_add(t3, t3);
    let t3_4 = fe_add(t3_2, t3_2);
    let t3_8 = fe_add(t3_4, t3_4);
    let y3 = fe_sub(y3_b, t3_8);
    // z3 = 2*y*z
    let z3 = fe_mul(fe_add(a.y, a.y), a.z);

    return Gej(x3, y3, z3, 0u);
}

fn gej_add_ge(a: Gej, b: Ge) -> Gej {
    if (a.infinity != 0u) {
        return gej_from_ge(b);
    }

    // u1 = x1 * z2^2 (z2=1 for affine)
    let u1 = a.x;
    // u2 = x2 * z1^2
    let z1_sqr = fe_sqr(a.z);
    let u2 = fe_mul(b.x, z1_sqr);
    // s1 = y1 * z2^3 (z2=1)
    let s1 = a.y;
    // s2 = y2 * z1^3
    let z1_cub = fe_mul(z1_sqr, a.z);
    let s2 = fe_mul(b.y, z1_cub);

    // h = u2 - u1
    let h = fe_sub(u2, u1);
    // r = s2 - s1
    let r = fe_sub(s2, s1);

    // Check if points are the same
    var h_is_zero = true;
    for (var i: u32 = 0u; i < 8u; i++) {
        if (h.d[i] != 0u) {
            h_is_zero = false;
            break;
        }
    }
    if (h_is_zero) {
        var r_is_zero = true;
        for (var i: u32 = 0u; i < 8u; i++) {
            if (r.d[i] != 0u) {
                r_is_zero = false;
                break;
            }
        }
        if (r_is_zero) {
            return gej_double(a);
        }
        return gej_infinity();
    }

    // h^2, h^3
    let h_sqr = fe_sqr(h);
    let h_cub = fe_mul(h_sqr, h);
    // u1 * h^2
    let u1_h_sqr = fe_mul(u1, h_sqr);

    // x3 = r^2 - h^3 - 2*u1*h^2
    let x3_a = fe_sqr(r);
    let x3_b = fe_sub(x3_a, h_cub);
    let x3 = fe_sub(x3_b, fe_add(u1_h_sqr, u1_h_sqr));

    // y3 = r*(u1*h^2 - x3) - s1*h^3
    let y3_a = fe_sub(u1_h_sqr, x3);
    let y3_b = fe_mul(r, y3_a);
    let y3_c = fe_mul(s1, h_cub);
    let y3 = fe_sub(y3_b, y3_c);

    // z3 = z1 * h
    let z3 = fe_mul(a.z, h);

    return Gej(x3, y3, z3, 0u);
}

fn gej_to_ge(a: Gej) -> Ge {
    if (a.infinity != 0u) {
        return Ge(fe_zero(), fe_zero());
    }

    let z_inv = fe_inv(a.z);
    let z_inv_2 = fe_sqr(z_inv);
    let z_inv_3 = fe_mul(z_inv_2, z_inv);

    let x = fe_mul(a.x, z_inv_2);
    let y = fe_mul(a.y, z_inv_3);

    return Ge(x, y);
}

// ============================================================================
// Scalar Multiplication
// ============================================================================

fn ecmult_gen(scalar: array<u32, 8>) -> Gej {
    let g = Ge(fe_from_array(GEN_X), fe_from_array(GEN_Y));
    var result = gej_infinity();

    // Simple double-and-add
    for (var i: i32 = 255; i >= 0; i--) {
        result = gej_double(result);

        let word_idx = u32(i) / 32u;
        let bit_idx = u32(i) % 32u;
        if ((scalar[word_idx] >> bit_idx) & 1u) == 1u {
            result = gej_add_ge(result, g);
        }
    }

    return result;
}

// ============================================================================
// Main Compute Shader
// ============================================================================

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
    let idx = gid.x;
    if (idx >= config.batch_size) {
        return;
    }

    // Compute scalar = base_scalar + idx
    var scalar: array<u32, 8>;
    var carry: u32 = idx;
    for (var i: u32 = 0u; i < 8u; i++) {
        let sum = base_scalar[i] + carry;
        scalar[i] = sum;
        carry = select(0u, 1u, sum < base_scalar[i]);
    }

    // EC scalar multiplication: P = G * scalar
    let point_j = ecmult_gen(scalar);
    let point = gej_to_ge(point_j);

    // Store result
    pubkeys[idx].prefix = select(0x02u, 0x03u, fe_is_odd(point.y));
    for (var i: u32 = 0u; i < 8u; i++) {
        pubkeys[idx].x[i] = point.x.d[i];
    }
    pubkeys[idx].batch_idx = idx;
}
