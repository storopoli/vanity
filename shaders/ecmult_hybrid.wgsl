// ecmult_hybrid.wgsl - EC scalar multiplication for hybrid GPU/CPU approach
//
// GPU does: scalar multiplication (93% of compute time)
// CPU does: hashing, encoding, pattern matching
//
// Note: Uses only u32 arithmetic (WGSL doesn't support u64 in all backends)
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

// Result: scalar and compressed public key
struct PubKeyResult {
    scalar: array<u32, 8>,   // The private key scalar
    pubkey_x: array<u32, 8>, // X coordinate (big-endian bytes as u32s)
    pubkey_y_parity: u32,    // 0 = even (0x02), 1 = odd (0x03)
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
@group(0) @binding(2) var<storage, read_write> results: array<PubKeyResult>;

// ============================================================================
// Constants - secp256k1 parameters
// ============================================================================

// Field prime p = 2^256 - 2^32 - 977
const P0: u32 = 0xFFFFFC2Fu;
const P1: u32 = 0xFFFFFFFEu;
const P2: u32 = 0xFFFFFFFFu;
const P3: u32 = 0xFFFFFFFFu;
const P4: u32 = 0xFFFFFFFFu;
const P5: u32 = 0xFFFFFFFFu;
const P6: u32 = 0xFFFFFFFFu;
const P7: u32 = 0xFFFFFFFFu;

// Generator point G (in little-endian limb order)
const GX0: u32 = 0x16F81798u; const GX1: u32 = 0x59F2815Bu;
const GX2: u32 = 0x2DCE28D9u; const GX3: u32 = 0x029BFCDBu;
const GX4: u32 = 0xCE870B07u; const GX5: u32 = 0x55A06295u;
const GX6: u32 = 0xF9DCBBACu; const GX7: u32 = 0x79BE667Eu;

const GY0: u32 = 0xFB10D4B8u; const GY1: u32 = 0x9C47D08Fu;
const GY2: u32 = 0xA6855419u; const GY3: u32 = 0xFD17B448u;
const GY4: u32 = 0x0E1108A8u; const GY5: u32 = 0x5DA4FBFCu;
const GY6: u32 = 0x26A3C465u; const GY7: u32 = 0x483ADA77u;

// ============================================================================
// 32-bit Arithmetic Helpers (avoiding u64)
// ============================================================================

// Add two u32s with carry in, return (sum, carry_out)
fn add32(a: u32, b: u32, c_in: u32) -> vec2<u32> {
    // Use the fact that (a + b) < a means overflow occurred
    let sum1 = a + b;
    let c1 = select(0u, 1u, sum1 < a);
    let sum2 = sum1 + c_in;
    let c2 = select(0u, 1u, sum2 < sum1);
    return vec2<u32>(sum2, c1 + c2);
}

// Multiply two u32s, return (lo, hi) parts of 64-bit result
fn mul32(a: u32, b: u32) -> vec2<u32> {
    // Split each into 16-bit parts
    let a_lo = a & 0xFFFFu;
    let a_hi = a >> 16u;
    let b_lo = b & 0xFFFFu;
    let b_hi = b >> 16u;

    // Partial products
    let p0 = a_lo * b_lo;           // bits 0-31
    let p1 = a_lo * b_hi;           // bits 16-47
    let p2 = a_hi * b_lo;           // bits 16-47
    let p3 = a_hi * b_hi;           // bits 32-63

    // Combine middle terms
    let mid = p1 + p2;
    let mid_carry = select(0u, 1u, mid < p1);

    // Add middle terms to result
    let lo = p0 + (mid << 16u);
    let lo_carry = select(0u, 1u, lo < p0);
    let hi = p3 + (mid >> 16u) + (mid_carry << 16u) + lo_carry;

    return vec2<u32>(lo, hi);
}

// ============================================================================
// Field Arithmetic (mod p)
// ============================================================================

fn fe_set(v: u32) -> Fe {
    var r: Fe;
    r.d[0] = v;
    for (var i = 1u; i < 8u; i++) { r.d[i] = 0u; }
    return r;
}

fn fe_is_zero(a: Fe) -> bool {
    for (var i = 0u; i < 8u; i++) {
        if (a.d[i] != 0u) { return false; }
    }
    return true;
}

fn fe_is_odd(a: Fe) -> bool {
    return (a.d[0] & 1u) == 1u;
}

fn fe_cmp(a: Fe, b: Fe) -> i32 {
    for (var i = 7; i >= 0; i--) {
        let idx = u32(i);
        if (a.d[idx] > b.d[idx]) { return 1; }
        if (a.d[idx] < b.d[idx]) { return -1; }
    }
    return 0;
}

fn fe_add(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var c: u32 = 0u;
    for (var i = 0u; i < 8u; i++) {
        let res = add32(a.d[i], b.d[i], c);
        r.d[i] = res.x;
        c = res.y;
    }
    return fe_reduce(r);
}

fn fe_sub(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var borrow: u32 = 0u;
    for (var i = 0u; i < 8u; i++) {
        let diff = a.d[i] - b.d[i] - borrow;
        // Borrow occurred if a.d[i] < b.d[i] + borrow
        if (a.d[i] < b.d[i] || (borrow != 0u && a.d[i] == b.d[i])) {
            borrow = 1u;
        } else {
            borrow = 0u;
        }
        r.d[i] = diff;
    }
    // If underflow, add p back
    if (borrow != 0u) {
        var c: u32 = 0u;
        let p = array<u32, 8>(P0, P1, P2, P3, P4, P5, P6, P7);
        for (var i = 0u; i < 8u; i++) {
            let res = add32(r.d[i], p[i], c);
            r.d[i] = res.x;
            c = res.y;
        }
    }
    return r;
}

fn fe_reduce(a: Fe) -> Fe {
    // Check if a >= p
    let p = array<u32, 8>(P0, P1, P2, P3, P4, P5, P6, P7);
    var ge = true;
    for (var i = 7; i >= 0; i--) {
        let idx = u32(i);
        if (a.d[idx] < p[idx]) { ge = false; break; }
        if (a.d[idx] > p[idx]) { break; }
    }
    if (!ge) { return a; }

    // Subtract p
    var r: Fe;
    var borrow: u32 = 0u;
    for (var i = 0u; i < 8u; i++) {
        let diff = a.d[i] - p[i] - borrow;
        if (a.d[i] < p[i] || (borrow != 0u && a.d[i] == p[i])) {
            borrow = 1u;
        } else {
            borrow = 0u;
        }
        r.d[i] = diff;
    }
    return r;
}

fn fe_mul(a: Fe, b: Fe) -> Fe {
    // Schoolbook multiplication to 512-bit result using 32-bit ops
    var t: array<u32, 17>;
    for (var i = 0u; i < 17u; i++) { t[i] = 0u; }

    for (var i = 0u; i < 8u; i++) {
        var carry: u32 = 0u;
        for (var j = 0u; j < 8u; j++) {
            let k = i + j;
            let prod = mul32(a.d[i], b.d[j]);
            // Add prod.x (lo) + carry + t[k]
            let sum1 = add32(t[k], prod.x, 0u);
            let sum2 = add32(sum1.x, carry, 0u);
            t[k] = sum2.x;
            // carry = prod.y (hi) + carries
            carry = prod.y + sum1.y + sum2.y;
        }
        t[i + 8u] = t[i + 8u] + carry;
    }

    // Reduce mod p using: 2^256 ≡ 2^32 + 977 (mod p)
    // Constant C = 0x1000003D1 = 2^32 + 977
    // hi * C = hi * 2^32 + hi * 977 = (hi shifted left) + hi * 977
    const C_LO: u32 = 0x3D1u;  // 977

    var r: array<u32, 9>;
    for (var i = 0u; i < 8u; i++) { r[i] = t[i]; }
    r[8] = 0u;

    // Process high limbs
    for (var i = 8u; i < 16u; i++) {
        if (t[i] == 0u) { continue; }
        let hi = t[i];
        let idx = i - 8u;

        // hi * C = hi * (2^32 + 977) = hi << 32 + hi * 977
        // hi << 32 goes to r[idx + 1]
        // hi * 977 goes to r[idx] and r[idx+1]
        let prod = mul32(hi, C_LO);

        // Add prod.x to r[idx]
        var c: u32 = 0u;
        let s1 = add32(r[idx], prod.x, 0u);
        r[idx] = s1.x;
        c = s1.y;

        // Add prod.y + hi (the << 32 part) + carry to r[idx+1]
        let s2 = add32(r[idx + 1u], prod.y, c);
        let s3 = add32(s2.x, hi, 0u);
        r[idx + 1u] = s3.x;
        c = s2.y + s3.y;

        // Propagate carries
        for (var j = idx + 2u; j <= 8u && c > 0u; j++) {
            let s = add32(r[j], c, 0u);
            r[j] = s.x;
            c = s.y;
        }
    }

    // Handle overflow in r[8]
    for (var iter = 0u; iter < 3u && r[8] > 0u; iter++) {
        let hi = r[8];
        r[8] = 0u;

        let prod = mul32(hi, C_LO);

        var c: u32 = 0u;
        let s1 = add32(r[0], prod.x, 0u);
        r[0] = s1.x;
        c = s1.y;

        let s2 = add32(r[1], prod.y, c);
        let s3 = add32(s2.x, hi, 0u);
        r[1] = s3.x;
        c = s2.y + s3.y;

        for (var j = 2u; j <= 8u && c > 0u; j++) {
            let s = add32(r[j], c, 0u);
            r[j] = s.x;
            c = s.y;
        }
    }

    var result: Fe;
    for (var i = 0u; i < 8u; i++) { result.d[i] = r[i]; }
    return fe_reduce(result);
}

fn fe_sqr(a: Fe) -> Fe {
    return fe_mul(a, a);
}

fn fe_inv(a: Fe) -> Fe {
    // Fermat: a^(-1) = a^(p-2) mod p
    var result = fe_set(1u);
    var base = a;

    // p-2 in binary, process bit by bit
    let pm2 = array<u32, 8>(
        0xFFFFFC2Du, 0xFFFFFFFEu, 0xFFFFFFFFu, 0xFFFFFFFFu,
        0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu
    );

    for (var i = 0u; i < 256u; i++) {
        let word = i / 32u;
        let bit = i % 32u;
        if ((pm2[word] >> bit) & 1u) == 1u {
            result = fe_mul(result, base);
        }
        base = fe_sqr(base);
    }
    return result;
}

// ============================================================================
// EC Group Operations (Jacobian coordinates)
// ============================================================================

fn gej_set_infinity() -> Gej {
    var r: Gej;
    r.x = fe_set(0u);
    r.y = fe_set(1u);
    r.z = fe_set(0u);
    r.infinity = 1u;
    return r;
}

fn ge_set_gej(a: Gej) -> Ge {
    if (a.infinity != 0u) {
        var r: Ge;
        r.x = fe_set(0u);
        r.y = fe_set(0u);
        return r;
    }

    let zinv = fe_inv(a.z);
    let zinv2 = fe_sqr(zinv);
    let zinv3 = fe_mul(zinv2, zinv);

    var r: Ge;
    r.x = fe_mul(a.x, zinv2);
    r.y = fe_mul(a.y, zinv3);
    return r;
}

fn gej_double(a: Gej) -> Gej {
    if (a.infinity != 0u) { return a; }
    if (fe_is_zero(a.y)) { return gej_set_infinity(); }

    // Standard doubling formulas for a=0 (secp256k1)
    let xx = fe_sqr(a.x);
    let yy = fe_sqr(a.y);
    let yyyy = fe_sqr(yy);

    let s_tmp = fe_add(a.x, yy);
    let s_tmp2 = fe_sqr(s_tmp);
    let s_tmp3 = fe_sub(s_tmp2, xx);
    let s = fe_add(fe_sub(s_tmp3, yyyy), fe_sub(s_tmp3, yyyy)); // 2*(x+yy)^2-xx-yyyy

    let m = fe_add(fe_add(xx, xx), xx); // 3*xx (a=0)

    let t = fe_sqr(m);
    let x3 = fe_sub(t, fe_add(s, s));

    let y3_tmp = fe_sub(s, x3);
    let y3_tmp2 = fe_mul(m, y3_tmp);
    let yyyy8 = fe_add(fe_add(fe_add(yyyy, yyyy), fe_add(yyyy, yyyy)),
                       fe_add(fe_add(yyyy, yyyy), fe_add(yyyy, yyyy)));
    let y3 = fe_sub(y3_tmp2, yyyy8);

    let z3 = fe_mul(fe_add(a.y, a.y), a.z);

    var r: Gej;
    r.x = x3;
    r.y = y3;
    r.z = z3;
    r.infinity = 0u;
    return r;
}

fn gej_add_ge(a: Gej, b: Ge) -> Gej {
    if (a.infinity != 0u) {
        var r: Gej;
        r.x = b.x;
        r.y = b.y;
        r.z = fe_set(1u);
        r.infinity = 0u;
        return r;
    }

    let z12 = fe_sqr(a.z);
    let u1 = a.x;
    let u2 = fe_mul(b.x, z12);
    let s1 = a.y;
    let s2 = fe_mul(fe_mul(b.y, z12), a.z);

    let h = fe_sub(u2, u1);
    let r_val = fe_sub(s2, s1);

    if (fe_is_zero(h)) {
        if (fe_is_zero(r_val)) {
            return gej_double(a);
        }
        return gej_set_infinity();
    }

    let hh = fe_sqr(h);
    let hhh = fe_mul(hh, h);
    let v = fe_mul(u1, hh);

    let x3_tmp = fe_sqr(r_val);
    let x3_tmp2 = fe_sub(x3_tmp, hhh);
    let x3 = fe_sub(x3_tmp2, fe_add(v, v));

    let y3_tmp = fe_sub(v, x3);
    let y3_tmp2 = fe_mul(r_val, y3_tmp);
    let y3 = fe_sub(y3_tmp2, fe_mul(s1, hhh));

    let z3 = fe_mul(a.z, h);

    var result: Gej;
    result.x = x3;
    result.y = y3;
    result.z = z3;
    result.infinity = 0u;
    return result;
}

// ============================================================================
// Scalar Multiplication: P = k * G
// ============================================================================

fn ecmult_gen(k: array<u32, 8>) -> Gej {
    // Generator point
    var g: Ge;
    g.x.d = array<u32, 8>(GX0, GX1, GX2, GX3, GX4, GX5, GX6, GX7);
    g.y.d = array<u32, 8>(GY0, GY1, GY2, GY3, GY4, GY5, GY6, GY7);

    var result = gej_set_infinity();

    // Double-and-add (MSB first)
    for (var i = 255; i >= 0; i--) {
        result = gej_double(result);

        let word = u32(i) / 32u;
        let bit = u32(i) % 32u;
        if ((k[word] >> bit) & 1u) == 1u {
            result = gej_add_ge(result, g);
        }
    }

    return result;
}

// ============================================================================
// Convert field element to big-endian bytes (as u32s)
// ============================================================================

fn fe_to_be32(a: Fe) -> array<u32, 8> {
    // Input: little-endian limbs, d[0] is least significant
    // Output: big-endian bytes packed as u32s
    var r: array<u32, 8>;
    for (var i = 0u; i < 8u; i++) {
        let le_idx = 7u - i;
        let val = a.d[le_idx];
        // Byte-swap the u32 for big-endian output
        r[i] = ((val & 0xFFu) << 24u) |
               ((val & 0xFF00u) << 8u) |
               ((val & 0xFF0000u) >> 8u) |
               ((val & 0xFF000000u) >> 24u);
    }
    return r;
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
    for (var i = 0u; i < 8u; i++) {
        let res = add32(base_scalar[i], carry, 0u);
        scalar[i] = res.x;
        carry = res.y;
    }

    // EC scalar multiplication: P = scalar * G
    let point_j = ecmult_gen(scalar);
    let point = ge_set_gej(point_j);

    // Store results
    results[idx].scalar = scalar;
    results[idx].pubkey_x = fe_to_be32(point.x);
    results[idx].pubkey_y_parity = select(0u, 1u, fe_is_odd(point.y));
}
