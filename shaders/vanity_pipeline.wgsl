// vanity_pipeline.wgsl - Full vanity address search pipeline
//
// WGSL compute shader for Bitcoin vanity address generation.
// Uses 8x32-bit limbs for 256-bit arithmetic (WGSL lacks u64 support).
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

// Jacobian projective point: (X, Y, Z) represents affine (X/Z^2, Y/Z^3)
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

// Match result to return to host
struct MatchResult {
    scalar: array<u32, 8>,
    pubkey_x: array<u32, 8>,
    pubkey_y_parity: u32,
    address: array<u32, 19>,  // Max 74 chars packed as u32s
    address_len: u32,
    batch_idx: u32,
    found: u32,
}

// Search configuration from host
struct Config {
    pattern_len: u32,
    address_type: u32,      // 0=P2PKH, 1=P2WPKH, 2=P2TR
    network: u32,           // 0=Mainnet, 1=Testnet
    iterations: u32,
    batch_size: u32,
    match_mode: u32,        // 0=prefix, 1=suffix, 2=contains
    _pad0: u32,
    _pad1: u32,
}

// ============================================================================
// Bindings
// ============================================================================

@group(0) @binding(0) var<uniform> config: Config;
@group(0) @binding(1) var<storage, read> pattern: array<u32, 19>;  // Pattern bytes packed
@group(0) @binding(2) var<storage, read> base_scalar: array<u32, 8>;  // Starting scalar
@group(0) @binding(3) var<storage, read_write> results: array<MatchResult>;
@group(0) @binding(4) var<storage, read_write> result_count: atomic<u32>;

// ============================================================================
// Constants
// ============================================================================

// Field prime p = 2^256 - 2^32 - 977
const FE_P: array<u32, 8> = array<u32, 8>(
    0xFFFFFC2Fu, 0xFFFFFFFEu, 0xFFFFFFFFu, 0xFFFFFFFFu,
    0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu
);

// Reduction constant
const FE_C_LOW: u32 = 0x3D1u;

// Generator point G
const GEN_X: array<u32, 8> = array<u32, 8>(
    0x16F81798u, 0x59F2815Bu, 0x2DCE28D9u, 0x029BFCDBu,
    0xCE870B07u, 0x55A06295u, 0xF9DCBBACu, 0x79BE667Eu
);

const GEN_Y: array<u32, 8> = array<u32, 8>(
    0xFB10D4B8u, 0x9C47D08Fu, 0xA6855419u, 0xFD17B448u,
    0x0E1108A8u, 0x5DA4FBFCu, 0x26A3C465u, 0x483ADA77u
);

// SHA-256 constants
const SHA256_K: array<u32, 64> = array<u32, 64>(
    0x428a2f98u, 0x71374491u, 0xb5c0fbcfu, 0xe9b5dba5u,
    0x3956c25bu, 0x59f111f1u, 0x923f82a4u, 0xab1c5ed5u,
    0xd807aa98u, 0x12835b01u, 0x243185beu, 0x550c7dc3u,
    0x72be5d74u, 0x80deb1feu, 0x9bdc06a7u, 0xc19bf174u,
    0xe49b69c1u, 0xefbe4786u, 0x0fc19dc6u, 0x240ca1ccu,
    0x2de92c6fu, 0x4a7484aau, 0x5cb0a9dcu, 0x76f988dau,
    0x983e5152u, 0xa831c66du, 0xb00327c8u, 0xbf597fc7u,
    0xc6e00bf3u, 0xd5a79147u, 0x06ca6351u, 0x14292967u,
    0x27b70a85u, 0x2e1b2138u, 0x4d2c6dfcu, 0x53380d13u,
    0x650a7354u, 0x766a0abbu, 0x81c2c92eu, 0x92722c85u,
    0xa2bfe8a1u, 0xa81a664bu, 0xc24b8b70u, 0xc76c51a3u,
    0xd192e819u, 0xd6990624u, 0xf40e3585u, 0x106aa070u,
    0x19a4c116u, 0x1e376c08u, 0x2748774cu, 0x34b0bcb5u,
    0x391c0cb3u, 0x4ed8aa4au, 0x5b9cca4fu, 0x682e6ff3u,
    0x748f82eeu, 0x78a5636fu, 0x84c87814u, 0x8cc70208u,
    0x90befffau, 0xa4506cebu, 0xbef9a3f7u, 0xc67178f2u
);

// Bech32 character set
const BECH32_CHARSET: array<u32, 32> = array<u32, 32>(
    0x71u, 0x70u, 0x7Au, 0x72u, 0x79u, 0x39u, 0x78u, 0x38u,
    0x67u, 0x66u, 0x32u, 0x74u, 0x76u, 0x64u, 0x77u, 0x30u,
    0x73u, 0x33u, 0x6Au, 0x6Eu, 0x35u, 0x34u, 0x6Bu, 0x68u,
    0x63u, 0x65u, 0x36u, 0x6Du, 0x75u, 0x61u, 0x37u, 0x6Cu
);

// Bech32 generator polynomial
const BECH32_GEN: array<u32, 5> = array<u32, 5>(
    0x3b6a57b2u, 0x26508e6du, 0x1ea119fau, 0x3d4233ddu, 0x2a1462b3u
);

const BECH32_CONST: u32 = 1u;
const BECH32M_CONST: u32 = 0x2bc830a3u;

// ============================================================================
// Field Arithmetic (256-bit mod p)
// ============================================================================

fn adc(a: u32, b: u32, carry_in: u32) -> vec2<u32> {
    let sum: u32 = a + b + carry_in;
    // Carry if sum < a (overflow) or if sum == a and carry_in was 1
    let carry_out: u32 = select(0u, 1u, sum < a || (carry_in != 0u && sum == a));
    return vec2<u32>(sum, carry_out);
}

fn sbb(a: u32, b: u32, borrow_in: u32) -> vec2<u32> {
    let diff: u32 = a - b - borrow_in;
    let borrow_out: u32 = select(0u, 1u, a < b || (borrow_in != 0u && a == b));
    return vec2<u32>(diff, borrow_out);
}

fn fe_zero() -> Fe {
    var r: Fe;
    for (var i: u32 = 0u; i < 8u; i++) {
        r.d[i] = 0u;
    }
    return r;
}

fn fe_one() -> Fe {
    var r: Fe;
    r.d[0] = 1u;
    for (var i: u32 = 1u; i < 8u; i++) {
        r.d[i] = 0u;
    }
    return r;
}

fn fe_from_array(arr: array<u32, 8>) -> Fe {
    var r: Fe;
    for (var i: u32 = 0u; i < 8u; i++) {
        r.d[i] = arr[7u - i];  // Big-endian to little-endian
    }
    return r;
}

fn fe_to_bytes(a: Fe) -> array<u32, 8> {
    var r: array<u32, 8>;
    for (var i: u32 = 0u; i < 8u; i++) {
        r[i] = a.d[7u - i];
    }
    return r;
}

fn fe_is_zero(a: Fe) -> bool {
    var or_val: u32 = 0u;
    for (var i: u32 = 0u; i < 8u; i++) {
        or_val |= a.d[i];
    }
    return or_val == 0u;
}

fn fe_is_odd(a: Fe) -> bool {
    return (a.d[0] & 1u) != 0u;
}

fn fe_gte_p(a: Fe) -> bool {
    for (var i: i32 = 7; i >= 0; i--) {
        if (a.d[i] < FE_P[i]) { return false; }
        if (a.d[i] > FE_P[i]) { return true; }
    }
    return true;
}

fn fe_add(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var carry: u32 = 0u;

    for (var i: u32 = 0u; i < 8u; i++) {
        let res = adc(a.d[i], b.d[i], carry);
        r.d[i] = res.x;
        carry = res.y;
    }

    if (carry != 0u || fe_gte_p(r)) {
        var borrow: u32 = 0u;
        for (var i: u32 = 0u; i < 8u; i++) {
            let res = sbb(r.d[i], FE_P[i], borrow);
            r.d[i] = res.x;
            borrow = res.y;
        }
    }

    return r;
}

fn fe_sub(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var borrow: u32 = 0u;

    for (var i: u32 = 0u; i < 8u; i++) {
        let res = sbb(a.d[i], b.d[i], borrow);
        r.d[i] = res.x;
        borrow = res.y;
    }

    if (borrow != 0u) {
        var carry: u32 = 0u;
        for (var i: u32 = 0u; i < 8u; i++) {
            let res = adc(r.d[i], FE_P[i], carry);
            r.d[i] = res.x;
            carry = res.y;
        }
    }

    return r;
}

// Multiply-add with carry for schoolbook multiplication
fn mac(a: u32, b: u32, c: u32, carry_in: u32) -> vec2<u32> {
    // a * b + c + carry_in
    let al = a & 0xFFFFu;
    let ah = a >> 16u;
    let bl = b & 0xFFFFu;
    let bh = b >> 16u;

    let ll = al * bl;
    let lh = al * bh;
    let hl = ah * bl;
    let hh = ah * bh;

    var mid = lh + hl;
    let mid_carry = select(0u, 0x10000u, mid < lh);

    var lo = ll + (mid << 16u);
    let lo_carry1 = select(0u, 1u, lo < ll);

    lo = lo + c;
    let lo_carry2 = select(0u, 1u, lo < c);

    lo = lo + carry_in;
    let lo_carry3 = select(0u, 1u, lo < carry_in);

    var hi = hh + (mid >> 16u) + mid_carry + lo_carry1 + lo_carry2 + lo_carry3;

    return vec2<u32>(lo, hi);
}

fn fe_mul(a: Fe, b: Fe) -> Fe {
    var t: array<u32, 16>;
    for (var i: u32 = 0u; i < 16u; i++) {
        t[i] = 0u;
    }

    // Schoolbook multiplication
    for (var i: u32 = 0u; i < 8u; i++) {
        var carry: u32 = 0u;
        for (var j: u32 = 0u; j < 8u; j++) {
            let res = mac(a.d[i], b.d[j], t[i + j], carry);
            t[i + j] = res.x;
            carry = res.y;
        }
        t[i + 8u] = carry;
    }

    // Reduce using 2^256 ≡ 2^32 + 977 (mod p)
    var r: Fe;
    var carry: u32 = 0u;

    // First pass
    let m0 = mac(t[8], FE_C_LOW, t[0], 0u);
    r.d[0] = m0.x;
    carry = m0.y;

    let m1 = mac(t[9], FE_C_LOW, t[1], carry);
    let a1 = adc(m1.x, t[8], 0u);
    r.d[1] = a1.x;
    carry = m1.y + a1.y;

    let m2 = mac(t[10], FE_C_LOW, t[2], carry);
    let a2 = adc(m2.x, t[9], 0u);
    r.d[2] = a2.x;
    carry = m2.y + a2.y;

    let m3 = mac(t[11], FE_C_LOW, t[3], carry);
    let a3 = adc(m3.x, t[10], 0u);
    r.d[3] = a3.x;
    carry = m3.y + a3.y;

    let m4 = mac(t[12], FE_C_LOW, t[4], carry);
    let a4 = adc(m4.x, t[11], 0u);
    r.d[4] = a4.x;
    carry = m4.y + a4.y;

    let m5 = mac(t[13], FE_C_LOW, t[5], carry);
    let a5 = adc(m5.x, t[12], 0u);
    r.d[5] = a5.x;
    carry = m5.y + a5.y;

    let m6 = mac(t[14], FE_C_LOW, t[6], carry);
    let a6 = adc(m6.x, t[13], 0u);
    r.d[6] = a6.x;
    carry = m6.y + a6.y;

    let m7 = mac(t[15], FE_C_LOW, t[7], carry);
    let a7 = adc(m7.x, t[14], 0u);
    r.d[7] = a7.x;
    carry = m7.y + a7.y + t[15];

    // Second reduction for overflow
    if (carry > 0u) {
        let mc = mac(carry, FE_C_LOW, r.d[0], 0u);
        r.d[0] = mc.x;
        var c2: u32 = mc.y;

        let ac = adc(r.d[1], carry, c2);
        r.d[1] = ac.x;
        c2 = ac.y;

        for (var i: u32 = 2u; i < 8u; i++) {
            if (c2 == 0u) { break; }
            let res = adc(r.d[i], 0u, c2);
            r.d[i] = res.x;
            c2 = res.y;
        }
    }

    // Final normalization
    if (fe_gte_p(r)) {
        var borrow: u32 = 0u;
        for (var i: u32 = 0u; i < 8u; i++) {
            let res = sbb(r.d[i], FE_P[i], borrow);
            r.d[i] = res.x;
            borrow = res.y;
        }
    }

    return r;
}

fn fe_sqr(a: Fe) -> Fe {
    return fe_mul(a, a);
}

// Modular inverse using Fermat's little theorem: a^-1 = a^(p-2)
fn fe_inv(a: Fe) -> Fe {
    var x2 = fe_sqr(a);
    x2 = fe_mul(x2, a);

    var x3 = fe_sqr(x2);
    x3 = fe_mul(x3, a);

    var x6 = x3;
    for (var i: u32 = 0u; i < 3u; i++) { x6 = fe_sqr(x6); }
    x6 = fe_mul(x6, x3);

    var x9 = x6;
    for (var i: u32 = 0u; i < 3u; i++) { x9 = fe_sqr(x9); }
    x9 = fe_mul(x9, x3);

    var x11 = x9;
    for (var i: u32 = 0u; i < 2u; i++) { x11 = fe_sqr(x11); }
    x11 = fe_mul(x11, x2);

    var x22 = x11;
    for (var i: u32 = 0u; i < 11u; i++) { x22 = fe_sqr(x22); }
    x22 = fe_mul(x22, x11);

    var x44 = x22;
    for (var i: u32 = 0u; i < 22u; i++) { x44 = fe_sqr(x44); }
    x44 = fe_mul(x44, x22);

    var x88 = x44;
    for (var i: u32 = 0u; i < 44u; i++) { x88 = fe_sqr(x88); }
    x88 = fe_mul(x88, x44);

    var x176 = x88;
    for (var i: u32 = 0u; i < 88u; i++) { x176 = fe_sqr(x176); }
    x176 = fe_mul(x176, x88);

    var x220 = x176;
    for (var i: u32 = 0u; i < 44u; i++) { x220 = fe_sqr(x220); }
    x220 = fe_mul(x220, x44);

    var x223 = x220;
    for (var i: u32 = 0u; i < 3u; i++) { x223 = fe_sqr(x223); }
    x223 = fe_mul(x223, x3);

    var t = x223;
    for (var i: u32 = 0u; i < 23u; i++) { t = fe_sqr(t); }
    t = fe_mul(t, x22);
    for (var i: u32 = 0u; i < 5u; i++) { t = fe_sqr(t); }
    t = fe_mul(t, a);
    for (var i: u32 = 0u; i < 3u; i++) { t = fe_sqr(t); }
    t = fe_mul(t, x2);
    for (var i: u32 = 0u; i < 2u; i++) { t = fe_sqr(t); }
    t = fe_mul(t, a);

    return t;
}

// ============================================================================
// EC Group Operations (Jacobian coordinates)
// ============================================================================

fn gej_infinity() -> Gej {
    var r: Gej;
    r.x = fe_zero();
    r.y = fe_one();
    r.z = fe_zero();
    r.infinity = 1u;
    return r;
}

fn gej_is_infinity(a: Gej) -> bool {
    return a.infinity != 0u || fe_is_zero(a.z);
}

fn gej_set_ge(a: Ge) -> Gej {
    var r: Gej;
    r.x = a.x;
    r.y = a.y;
    r.z = fe_one();
    r.infinity = 0u;
    return r;
}

fn ge_generator() -> Ge {
    var g: Ge;
    g.x = fe_from_array(GEN_X);
    g.y = fe_from_array(GEN_Y);
    return g;
}

// Point doubling: r = 2*a
fn gej_double(a: Gej) -> Gej {
    if (gej_is_infinity(a)) {
        return gej_infinity();
    }

    var y2 = fe_sqr(a.y);
    var s = fe_mul(a.x, y2);
    s = fe_add(s, s);
    s = fe_add(s, s);

    var m = fe_sqr(a.x);
    var t = fe_add(m, m);
    m = fe_add(t, m);

    var x3 = fe_sqr(m);
    x3 = fe_sub(x3, s);
    x3 = fe_sub(x3, s);

    var y4 = fe_sqr(y2);

    t = fe_sub(s, x3);
    var y3 = fe_mul(m, t);
    t = fe_add(y4, y4);
    t = fe_add(t, t);
    t = fe_add(t, t);
    y3 = fe_sub(y3, t);

    var z3 = fe_mul(a.y, a.z);
    z3 = fe_add(z3, z3);

    var r: Gej;
    r.x = x3;
    r.y = y3;
    r.z = z3;
    r.infinity = 0u;
    return r;
}

// Mixed addition: r = a + b (a is Jacobian, b is affine)
fn gej_add_ge(a: Gej, b: Ge) -> Gej {
    if (gej_is_infinity(a)) {
        return gej_set_ge(b);
    }

    var z12 = fe_sqr(a.z);
    var z13 = fe_mul(z12, a.z);

    var u2 = fe_mul(b.x, z12);
    var s2 = fe_mul(b.y, z13);

    var h = fe_sub(u2, a.x);
    var rr = fe_sub(s2, a.y);

    if (fe_is_zero(h)) {
        if (fe_is_zero(rr)) {
            return gej_double(gej_set_ge(b));
        } else {
            return gej_infinity();
        }
    }

    var h2 = fe_sqr(h);
    var h3 = fe_mul(h2, h);
    var u1h2 = fe_mul(a.x, h2);

    var x3 = fe_sqr(rr);
    x3 = fe_sub(x3, h3);
    x3 = fe_sub(x3, u1h2);
    x3 = fe_sub(x3, u1h2);

    var t = fe_sub(u1h2, x3);
    var y3 = fe_mul(rr, t);
    t = fe_mul(a.y, h3);
    y3 = fe_sub(y3, t);

    var z3 = fe_mul(a.z, h);

    var r: Gej;
    r.x = x3;
    r.y = y3;
    r.z = z3;
    r.infinity = 0u;
    return r;
}

// Convert Jacobian to affine
fn gej_to_ge(a: Gej) -> Ge {
    if (gej_is_infinity(a)) {
        var r: Ge;
        r.x = fe_zero();
        r.y = fe_zero();
        return r;
    }

    var z_inv = fe_inv(a.z);
    var z2 = fe_sqr(z_inv);
    var z3 = fe_mul(z2, z_inv);

    var r: Ge;
    r.x = fe_mul(a.x, z2);
    r.y = fe_mul(a.y, z3);
    return r;
}

// Scalar multiplication: r = G * scalar (double-and-add)
fn ecmult_gen(scalar: array<u32, 8>) -> Gej {
    var r = gej_infinity();
    let g = ge_generator();

    for (var limb: i32 = 7; limb >= 0; limb--) {
        var s = scalar[limb];

        for (var bit: i32 = 31; bit >= 0; bit--) {
            if (!gej_is_infinity(r)) {
                r = gej_double(r);
            }

            if (((s >> u32(bit)) & 1u) != 0u) {
                r = gej_add_ge(r, g);
            }
        }
    }

    return r;
}

// ============================================================================
// SHA-256
// ============================================================================

fn sha256_rotr(x: u32, n: u32) -> u32 {
    return (x >> n) | (x << (32u - n));
}

fn sha256_ch(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ (~x & z);
}

fn sha256_maj(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ (x & z) ^ (y & z);
}

fn sha256_sigma0(x: u32) -> u32 {
    return sha256_rotr(x, 2u) ^ sha256_rotr(x, 13u) ^ sha256_rotr(x, 22u);
}

fn sha256_sigma1(x: u32) -> u32 {
    return sha256_rotr(x, 6u) ^ sha256_rotr(x, 11u) ^ sha256_rotr(x, 25u);
}

fn sha256_gamma0(x: u32) -> u32 {
    return sha256_rotr(x, 7u) ^ sha256_rotr(x, 18u) ^ (x >> 3u);
}

fn sha256_gamma1(x: u32) -> u32 {
    return sha256_rotr(x, 17u) ^ sha256_rotr(x, 19u) ^ (x >> 10u);
}

// SHA-256 hash of 33 bytes (compressed pubkey)
fn sha256_33bytes(data: array<u32, 9>) -> array<u32, 8> {
    var w: array<u32, 64>;

    // First 8 words from data (32 bytes)
    for (var i: u32 = 0u; i < 8u; i++) {
        w[i] = data[i];
    }
    // Last byte + padding
    w[8] = (data[8] << 24u) | 0x800000u;
    for (var i: u32 = 9u; i < 15u; i++) {
        w[i] = 0u;
    }
    w[15] = 264u;  // 33 * 8 bits

    // Extend
    for (var i: u32 = 16u; i < 64u; i++) {
        w[i] = sha256_gamma1(w[i - 2u]) + w[i - 7u] + sha256_gamma0(w[i - 15u]) + w[i - 16u];
    }

    // Initialize
    var a: u32 = 0x6a09e667u;
    var b: u32 = 0xbb67ae85u;
    var c: u32 = 0x3c6ef372u;
    var d: u32 = 0xa54ff53au;
    var e: u32 = 0x510e527fu;
    var f: u32 = 0x9b05688cu;
    var g: u32 = 0x1f83d9abu;
    var h: u32 = 0x5be0cd19u;

    // Rounds
    for (var i: u32 = 0u; i < 64u; i++) {
        let t1 = h + sha256_sigma1(e) + sha256_ch(e, f, g) + SHA256_K[i] + w[i];
        let t2 = sha256_sigma0(a) + sha256_maj(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    var result: array<u32, 8>;
    result[0] = a + 0x6a09e667u;
    result[1] = b + 0xbb67ae85u;
    result[2] = c + 0x3c6ef372u;
    result[3] = d + 0xa54ff53au;
    result[4] = e + 0x510e527fu;
    result[5] = f + 0x9b05688cu;
    result[6] = g + 0x1f83d9abu;
    result[7] = h + 0x5be0cd19u;

    return result;
}

// ============================================================================
// RIPEMD-160 (for Hash160)
// ============================================================================

fn ripemd_f(j: u32, x: u32, y: u32, z: u32) -> u32 {
    if (j < 16u) { return x ^ y ^ z; }
    if (j < 32u) { return (x & y) | (~x & z); }
    if (j < 48u) { return (x | ~y) ^ z; }
    if (j < 64u) { return (x & z) | (y & ~z); }
    return x ^ (y | ~z);
}

fn ripemd_rotl(x: u32, n: u32) -> u32 {
    return (x << n) | (x >> (32u - n));
}

// RIPEMD-160 hash of 32 bytes (SHA-256 output)
fn ripemd160_32bytes(data: array<u32, 8>) -> array<u32, 5> {
    // Message schedule (32 bytes + padding)
    var x: array<u32, 16>;
    for (var i: u32 = 0u; i < 8u; i++) {
        // Byte swap for RIPEMD (little-endian)
        let w = data[i];
        x[i] = ((w & 0xFFu) << 24u) | ((w & 0xFF00u) << 8u) |
               ((w >> 8u) & 0xFF00u) | ((w >> 24u) & 0xFFu);
    }
    x[8] = 0x80u;  // Padding
    for (var i: u32 = 9u; i < 14u; i++) {
        x[i] = 0u;
    }
    x[14] = 256u;  // 32 * 8 bits (little-endian)
    x[15] = 0u;

    // RIPEMD-160 constants and permutations
    let r_left = array<u32, 80>(
        0u, 1u, 2u, 3u, 4u, 5u, 6u, 7u, 8u, 9u, 10u, 11u, 12u, 13u, 14u, 15u,
        7u, 4u, 13u, 1u, 10u, 6u, 15u, 3u, 12u, 0u, 9u, 5u, 2u, 14u, 11u, 8u,
        3u, 10u, 14u, 4u, 9u, 15u, 8u, 1u, 2u, 7u, 0u, 6u, 13u, 11u, 5u, 12u,
        1u, 9u, 11u, 10u, 0u, 8u, 12u, 4u, 13u, 3u, 7u, 15u, 14u, 5u, 6u, 2u,
        4u, 0u, 5u, 9u, 7u, 12u, 2u, 10u, 14u, 1u, 3u, 8u, 11u, 6u, 15u, 13u
    );

    let r_right = array<u32, 80>(
        5u, 14u, 7u, 0u, 9u, 2u, 11u, 4u, 13u, 6u, 15u, 8u, 1u, 10u, 3u, 12u,
        6u, 11u, 3u, 7u, 0u, 13u, 5u, 10u, 14u, 15u, 8u, 12u, 4u, 9u, 1u, 2u,
        15u, 5u, 1u, 3u, 7u, 14u, 6u, 9u, 11u, 8u, 12u, 2u, 10u, 0u, 4u, 13u,
        8u, 6u, 4u, 1u, 3u, 11u, 15u, 0u, 5u, 12u, 2u, 13u, 9u, 7u, 10u, 14u,
        12u, 15u, 10u, 4u, 1u, 5u, 8u, 7u, 6u, 2u, 13u, 14u, 0u, 3u, 9u, 11u
    );

    let s_left = array<u32, 80>(
        11u, 14u, 15u, 12u, 5u, 8u, 7u, 9u, 11u, 13u, 14u, 15u, 6u, 7u, 9u, 8u,
        7u, 6u, 8u, 13u, 11u, 9u, 7u, 15u, 7u, 12u, 15u, 9u, 11u, 7u, 13u, 12u,
        11u, 13u, 6u, 7u, 14u, 9u, 13u, 15u, 14u, 8u, 13u, 6u, 5u, 12u, 7u, 5u,
        11u, 12u, 14u, 15u, 14u, 15u, 9u, 8u, 9u, 14u, 5u, 6u, 8u, 6u, 5u, 12u,
        9u, 15u, 5u, 11u, 6u, 8u, 13u, 12u, 5u, 12u, 13u, 14u, 11u, 8u, 5u, 6u
    );

    let s_right = array<u32, 80>(
        8u, 9u, 9u, 11u, 13u, 15u, 15u, 5u, 7u, 7u, 8u, 11u, 14u, 14u, 12u, 6u,
        9u, 13u, 15u, 7u, 12u, 8u, 9u, 11u, 7u, 7u, 12u, 7u, 6u, 15u, 13u, 11u,
        9u, 7u, 15u, 11u, 8u, 6u, 6u, 14u, 12u, 13u, 5u, 14u, 13u, 13u, 7u, 5u,
        15u, 5u, 8u, 11u, 14u, 14u, 6u, 14u, 6u, 9u, 12u, 9u, 12u, 5u, 15u, 8u,
        8u, 5u, 12u, 9u, 12u, 5u, 14u, 6u, 8u, 13u, 6u, 5u, 15u, 13u, 11u, 11u
    );

    // Initialize
    var al: u32 = 0x67452301u;
    var bl: u32 = 0xefcdab89u;
    var cl: u32 = 0x98badcfeu;
    var dl: u32 = 0x10325476u;
    var el: u32 = 0xc3d2e1f0u;

    var ar: u32 = 0x67452301u;
    var br: u32 = 0xefcdab89u;
    var cr: u32 = 0x98badcfeu;
    var dr: u32 = 0x10325476u;
    var er: u32 = 0xc3d2e1f0u;

    // Constants
    let kl = array<u32, 5>(0x00000000u, 0x5a827999u, 0x6ed9eba1u, 0x8f1bbcdcu, 0xa953fd4eu);
    let kr = array<u32, 5>(0x50a28be6u, 0x5c4dd124u, 0x6d703ef3u, 0x7a6d76e9u, 0x00000000u);

    // 80 rounds
    for (var j: u32 = 0u; j < 80u; j++) {
        let round = j / 16u;

        // Left
        var tl = al + ripemd_f(j, bl, cl, dl) + x[r_left[j]] + kl[round];
        tl = ripemd_rotl(tl, s_left[j]) + el;
        al = el; el = dl; dl = ripemd_rotl(cl, 10u); cl = bl; bl = tl;

        // Right
        var tr = ar + ripemd_f(79u - j, br, cr, dr) + x[r_right[j]] + kr[round];
        tr = ripemd_rotl(tr, s_right[j]) + er;
        ar = er; er = dr; dr = ripemd_rotl(cr, 10u); cr = br; br = tr;
    }

    let t = 0xefcdab89u + cl + dr;

    var result: array<u32, 5>;
    result[0] = 0x98badcfeu + dl + er;
    result[1] = 0x10325476u + el + ar;
    result[2] = 0xc3d2e1f0u + al + br;
    result[3] = 0x67452301u + bl + cr;
    result[4] = t;

    return result;
}

// Hash160 = RIPEMD160(SHA256(data))
fn hash160(pubkey: array<u32, 9>) -> array<u32, 5> {
    let sha = sha256_33bytes(pubkey);
    return ripemd160_32bytes(sha);
}

// ============================================================================
// Bech32 Encoding
// ============================================================================

fn bech32_polymod(values: array<u32, 64>, len: u32) -> u32 {
    var chk: u32 = 1u;
    for (var i: u32 = 0u; i < len; i++) {
        let top = chk >> 25u;
        chk = ((chk & 0x1ffffffu) << 5u) ^ values[i];
        for (var j: u32 = 0u; j < 5u; j++) {
            if (((top >> j) & 1u) != 0u) {
                chk ^= BECH32_GEN[j];
            }
        }
    }
    return chk;
}

// Convert 8-bit to 5-bit groups
fn convert_bits_8to5(data: array<u32, 8>, data_len: u32) -> array<u32, 52> {
    var out: array<u32, 52>;
    var acc: u32 = 0u;
    var bits: u32 = 0u;
    var out_idx: u32 = 0u;

    for (var i: u32 = 0u; i < data_len; i++) {
        let byte_idx = i / 4u;
        let byte_pos = 3u - (i % 4u);
        let byte_val = (data[byte_idx] >> (byte_pos * 8u)) & 0xFFu;

        acc = (acc << 8u) | byte_val;
        bits += 8u;

        while (bits >= 5u) {
            bits -= 5u;
            out[out_idx] = (acc >> bits) & 0x1Fu;
            out_idx++;
        }
    }

    if (bits > 0u) {
        out[out_idx] = (acc << (5u - bits)) & 0x1Fu;
    }

    return out;
}

struct Bech32Result {
    chars: array<u32, 16>,
    len: u32,
}

// Encode P2WPKH address (bc1q...)
fn encode_p2wpkh(h160: array<u32, 5>, mainnet: bool) -> Bech32Result {
    var result: Bech32Result;

    let hrp0: u32 = select(0x74u, 0x62u, mainnet);  // 't' or 'b'
    let hrp1: u32 = select(0x62u, 0x63u, mainnet);  // 'b' or 'c'

    // Convert hash160 to bytes
    var hash_bytes: array<u32, 8>;
    for (var i: u32 = 0u; i < 5u; i++) {
        let word = h160[i];
        hash_bytes[i] = ((word & 0xFFu) << 24u) |
                        ((word >> 8u) & 0xFF0000u) |
                        ((word >> 8u) & 0xFF00u) |
                        ((word >> 24u) & 0xFFu);
    }

    let data5 = convert_bits_8to5(hash_bytes, 20u);

    // Build checksum input
    var checksum_input: array<u32, 64>;
    checksum_input[0] = hrp0 >> 5u;
    checksum_input[1] = hrp1 >> 5u;
    checksum_input[2] = 0u;
    checksum_input[3] = hrp0 & 0x1Fu;
    checksum_input[4] = hrp1 & 0x1Fu;
    checksum_input[5] = 0u;  // Witness version 0

    for (var i: u32 = 0u; i < 32u; i++) {
        checksum_input[6u + i] = data5[i];
    }

    for (var i: u32 = 0u; i < 6u; i++) {
        checksum_input[38u + i] = 0u;
    }

    let polymod = bech32_polymod(checksum_input, 44u) ^ BECH32_CONST;

    // Build output: hrp + '1' + 'q' + data + checksum
    result.chars[0] = (hrp0 << 24u) | (hrp1 << 16u) | (0x31u << 8u) | BECH32_CHARSET[0];
    var out_idx: u32 = 4u;

    for (var i: u32 = 0u; i < 32u; i++) {
        let char_idx = out_idx / 4u;
        let char_pos = 3u - (out_idx % 4u);
        result.chars[char_idx] |= BECH32_CHARSET[data5[i]] << (char_pos * 8u);
        out_idx++;
    }

    for (var i: u32 = 0u; i < 6u; i++) {
        let char_idx = out_idx / 4u;
        let char_pos = 3u - (out_idx % 4u);
        let c = (polymod >> (5u * (5u - i))) & 0x1Fu;
        result.chars[char_idx] |= BECH32_CHARSET[c] << (char_pos * 8u);
        out_idx++;
    }

    result.len = 42u;
    return result;
}

// Encode P2TR address (bc1p...)
fn encode_p2tr(xonly: array<u32, 8>, mainnet: bool) -> Bech32Result {
    var result: Bech32Result;

    let hrp0: u32 = select(0x74u, 0x62u, mainnet);
    let hrp1: u32 = select(0x62u, 0x63u, mainnet);

    let data5 = convert_bits_8to5(xonly, 32u);

    var checksum_input: array<u32, 64>;
    checksum_input[0] = hrp0 >> 5u;
    checksum_input[1] = hrp1 >> 5u;
    checksum_input[2] = 0u;
    checksum_input[3] = hrp0 & 0x1Fu;
    checksum_input[4] = hrp1 & 0x1Fu;
    checksum_input[5] = 1u;  // Witness version 1

    for (var i: u32 = 0u; i < 52u; i++) {
        checksum_input[6u + i] = data5[i];
    }

    for (var i: u32 = 0u; i < 6u; i++) {
        checksum_input[58u + i] = 0u;
    }

    let polymod = bech32_polymod(checksum_input, 64u) ^ BECH32M_CONST;

    result.chars[0] = (hrp0 << 24u) | (hrp1 << 16u) | (0x31u << 8u) | BECH32_CHARSET[1];
    var out_idx: u32 = 4u;

    for (var i: u32 = 0u; i < 52u; i++) {
        let char_idx = out_idx / 4u;
        let char_pos = 3u - (out_idx % 4u);
        result.chars[char_idx] |= BECH32_CHARSET[data5[i]] << (char_pos * 8u);
        out_idx++;
    }

    for (var i: u32 = 0u; i < 6u; i++) {
        let char_idx = out_idx / 4u;
        let char_pos = 3u - (out_idx % 4u);
        let c = (polymod >> (5u * (5u - i))) & 0x1Fu;
        result.chars[char_idx] |= BECH32_CHARSET[c] << (char_pos * 8u);
        out_idx++;
    }

    result.len = 62u;
    return result;
}

// ============================================================================
// Pattern Matching
// ============================================================================

fn match_prefix(addr: Bech32Result, pat_len: u32) -> bool {
    // Compare pattern bytes against address bytes
    for (var i: u32 = 0u; i < pat_len; i++) {
        let addr_idx = i / 4u;
        let addr_pos = 3u - (i % 4u);
        let addr_byte = (addr.chars[addr_idx] >> (addr_pos * 8u)) & 0xFFu;

        let pat_idx = i / 4u;
        let pat_pos = 3u - (i % 4u);
        let pat_byte = (pattern[pat_idx] >> (pat_pos * 8u)) & 0xFFu;

        if (addr_byte != pat_byte) {
            return false;
        }
    }
    return true;
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

    // Serialize public key (compressed)
    var pubkey: array<u32, 9>;
    pubkey[0] = select(0x02u, 0x03u, fe_is_odd(point.y));
    let x_bytes = fe_to_bytes(point.x);
    for (var i: u32 = 0u; i < 8u; i++) {
        pubkey[i + 1u] = x_bytes[i];
    }

    // Generate address based on type
    var addr: Bech32Result;
    let mainnet = config.network == 0u;

    if (config.address_type == 0u) {
        // P2PKH - would need Base58 encoding (not implemented in this shader)
        // For now, skip P2PKH on GPU
        return;
    } else if (config.address_type == 1u) {
        // P2WPKH
        let h160 = hash160(pubkey);
        addr = encode_p2wpkh(h160, mainnet);
    } else {
        // P2TR
        let xonly = fe_to_bytes(point.x);
        addr = encode_p2tr(xonly, mainnet);
    }

    // Check pattern match
    if (match_prefix(addr, config.pattern_len)) {
        // Found a match! Store result
        let result_idx = atomicAdd(&result_count, 1u);
        if (result_idx < 64u) {  // Max 64 results per batch
            results[result_idx].found = 1u;
            results[result_idx].batch_idx = idx;
            for (var i: u32 = 0u; i < 8u; i++) {
                results[result_idx].scalar[i] = scalar[i];
                results[result_idx].pubkey_x[i] = x_bytes[i];
            }
            results[result_idx].pubkey_y_parity = select(0u, 1u, fe_is_odd(point.y));
            for (var i: u32 = 0u; i < 16u; i++) {
                results[result_idx].address[i] = addr.chars[i];
            }
            results[result_idx].address_len = addr.len;
        }
    }
}
