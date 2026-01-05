// field.wgsl - 256-bit field arithmetic for secp256k1
//
// Uses 8x32-bit limbs in little-endian order.
// All operations are modulo p = 2^256 - 2^32 - 977
//
// Copyright (c) 2025 Jose Storopoli
// MIT License

// 256-bit field element as 8x32-bit limbs (little-endian)
struct Fe {
    d: array<u32, 8>,
}

// Field prime p = 2^256 - 2^32 - 977
const FE_P: array<u32, 8> = array<u32, 8>(
    0xFFFFFC2Fu, 0xFFFFFFFEu, 0xFFFFFFFFu, 0xFFFFFFFFu,
    0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu, 0xFFFFFFFFu
);

// Zero
const FE_ZERO: array<u32, 8> = array<u32, 8>(0u, 0u, 0u, 0u, 0u, 0u, 0u, 0u);

// One
const FE_ONE: array<u32, 8> = array<u32, 8>(1u, 0u, 0u, 0u, 0u, 0u, 0u, 0u);

// Reduction constant: 2^256 mod p = 2^32 + 977 = 0x1000003D1
const FE_C_LOW: u32 = 0x3D1u;
const FE_C_HIGH: u32 = 1u;

// ============================================================================
// Helper functions
// ============================================================================

// Add with carry: returns (sum, carry)
fn adc(a: u32, b: u32, carry_in: u32) -> vec2<u32> {
    let sum64: u64 = u64(a) + u64(b) + u64(carry_in);
    return vec2<u32>(u32(sum64 & 0xFFFFFFFFu), u32(sum64 >> 32u));
}

// Subtract with borrow: returns (diff, borrow)
fn sbb(a: u32, b: u32, borrow_in: u32) -> vec2<u32> {
    let a64: u64 = u64(a);
    let b64: u64 = u64(b) + u64(borrow_in);
    let diff: u32 = u32((a64 - b64) & 0xFFFFFFFFu);
    let borrow: u32 = select(0u, 1u, a64 < b64);
    return vec2<u32>(diff, borrow);
}

// Multiply-add with carry: returns (result, carry)
fn mac(a: u32, b: u32, c: u32, carry_in: u32) -> vec2<u32> {
    let prod: u64 = u64(a) * u64(b) + u64(c) + u64(carry_in);
    return vec2<u32>(u32(prod & 0xFFFFFFFFu), u32(prod >> 32u));
}

// ============================================================================
// Field operations
// ============================================================================

fn fe_copy(a: Fe) -> Fe {
    var r: Fe;
    for (var i: u32 = 0u; i < 8u; i++) {
        r.d[i] = a.d[i];
    }
    return r;
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

fn fe_eq(a: Fe, b: Fe) -> bool {
    var eq: u32 = 0u;
    for (var i: u32 = 0u; i < 8u; i++) {
        eq |= a.d[i] ^ b.d[i];
    }
    return eq == 0u;
}

// Compare a >= p
fn fe_gte_p(a: Fe) -> bool {
    for (var i: i32 = 7; i >= 0; i--) {
        if (a.d[i] < FE_P[i]) { return false; }
        if (a.d[i] > FE_P[i]) { return true; }
    }
    return true;
}

// r = a + b mod p
fn fe_add(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var carry: u32 = 0u;

    for (var i: u32 = 0u; i < 8u; i++) {
        let res = adc(a.d[i], b.d[i], carry);
        r.d[i] = res.x;
        carry = res.y;
    }

    // Reduce if >= p or overflow
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

// r = a - b mod p
fn fe_sub(a: Fe, b: Fe) -> Fe {
    var r: Fe;
    var borrow: u32 = 0u;

    for (var i: u32 = 0u; i < 8u; i++) {
        let res = sbb(a.d[i], b.d[i], borrow);
        r.d[i] = res.x;
        borrow = res.y;
    }

    // Add p if underflow
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

// r = -a mod p
fn fe_neg(a: Fe) -> Fe {
    if (fe_is_zero(a)) {
        return fe_zero();
    }
    var r: Fe;
    var borrow: u32 = 0u;
    for (var i: u32 = 0u; i < 8u; i++) {
        let res = sbb(FE_P[i], a.d[i], borrow);
        r.d[i] = res.x;
        borrow = res.y;
    }
    return r;
}

// r = a * b mod p
// Uses schoolbook multiplication with secp256k1-specific reduction
fn fe_mul(a: Fe, b: Fe) -> Fe {
    // Full 512-bit product in 16 limbs
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

    // Reduce using 2^256 = 2^32 + 977 (mod p)
    // t[8..15] * 2^256 = t[8..15] * (2^32 + 977)

    var r: Fe;

    // First pass: add t[8..15] * (2^32 + 977) to t[0..7]
    var carry: u32 = 0u;

    // t[8] * 977
    let m0 = mac(t[8], FE_C_LOW, t[0], 0u);
    r.d[0] = m0.x;
    carry = m0.y;

    // t[8] * 2^32 + t[9] * 977
    let m1 = mac(t[9], FE_C_LOW, t[1], carry);
    let a1 = adc(m1.x, t[8], 0u);
    r.d[1] = a1.x;
    carry = m1.y + a1.y;

    // Continue pattern for remaining limbs
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

    // Second reduction pass for remaining carry
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

// r = a^2 mod p
fn fe_sqr(a: Fe) -> Fe {
    return fe_mul(a, a);
}

// r = a^n mod p (square and multiply)
fn fe_pow(base: Fe, exp: array<u32, 8>) -> Fe {
    var result = fe_one();
    var b = base;

    for (var limb: u32 = 0u; limb < 8u; limb++) {
        var e = exp[limb];
        for (var bit: u32 = 0u; bit < 32u; bit++) {
            if ((e & 1u) != 0u) {
                result = fe_mul(result, b);
            }
            b = fe_sqr(b);
            e >>= 1u;
        }
    }

    return result;
}

// r = a^-1 mod p using Fermat's little theorem: a^-1 = a^(p-2)
fn fe_inv(a: Fe) -> Fe {
    // p - 2 = 0xFFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFE FFFFFC2D
    // Use addition chain for efficiency

    var x2 = fe_sqr(a);
    x2 = fe_mul(x2, a);  // a^3

    var x3 = fe_sqr(x2);
    x3 = fe_mul(x3, a);  // a^7

    var x6 = x3;
    for (var i: u32 = 0u; i < 3u; i++) { x6 = fe_sqr(x6); }
    x6 = fe_mul(x6, x3);  // a^63

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

    // Final exponentiation
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

// Convert 32-byte big-endian to Fe
fn fe_from_bytes(b: array<u32, 8>) -> Fe {
    var r: Fe;
    // Input is big-endian u32s, output is little-endian limbs
    for (var i: u32 = 0u; i < 8u; i++) {
        r.d[i] = b[7u - i];
    }
    return r;
}

// Convert Fe to 32-byte big-endian
fn fe_to_bytes(a: Fe) -> array<u32, 8> {
    var r: array<u32, 8>;
    for (var i: u32 = 0u; i < 8u; i++) {
        r[i] = a.d[7u - i];
    }
    return r;
}
