// secp256k1.wgsl - Elliptic curve group operations
//
// Implements point addition and scalar multiplication for secp256k1.
// Uses Jacobian projective coordinates for efficiency.
//
// Copyright (c) 2025 Jose Storopoli
// MIT License

// Include field arithmetic (in actual WGSL, these would be in same file or imported)
// For now, assume Fe type and fe_* functions are available

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

// Generator point G
const GEN_X: array<u32, 8> = array<u32, 8>(
    0x16F81798u, 0x59F2815Bu, 0x2DCE28D9u, 0x029BFCDBu,
    0xCE870B07u, 0x55A06295u, 0xF9DCBBACu, 0x79BE667Eu
);

const GEN_Y: array<u32, 8> = array<u32, 8>(
    0xFB10D4B8u, 0x9C47D08Fu, 0xA6855419u, 0xFD17B448u,
    0x0E1108A8u, 0x5DA4FBFCu, 0x26A3C465u, 0x483ADA77u
);

// ============================================================================
// Group operations
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
    g.x = fe_from_bytes(GEN_X);
    g.y = fe_from_bytes(GEN_Y);
    return g;
}

// Point doubling: r = 2*a
// Uses formula for a=0 (secp256k1: y^2 = x^3 + 7)
fn gej_double(a: Gej) -> Gej {
    if (gej_is_infinity(a)) {
        return gej_infinity();
    }

    // S = 4*X*Y^2
    var y2 = fe_sqr(a.y);
    var s = fe_mul(a.x, y2);
    s = fe_add(s, s);
    s = fe_add(s, s);

    // M = 3*X^2 (since a=0)
    var m = fe_sqr(a.x);
    var t = fe_add(m, m);
    m = fe_add(t, m);

    // X' = M^2 - 2*S
    var x3 = fe_sqr(m);
    x3 = fe_sub(x3, s);
    x3 = fe_sub(x3, s);

    // Y^4
    var y4 = fe_sqr(y2);

    // Y' = M*(S - X') - 8*Y^4
    t = fe_sub(s, x3);
    var y3 = fe_mul(m, t);
    t = fe_add(y4, y4);
    t = fe_add(t, t);
    t = fe_add(t, t);
    y3 = fe_sub(y3, t);

    // Z' = 2*Y*Z
    var z3 = fe_mul(a.y, a.z);
    z3 = fe_add(z3, z3);

    var r: Gej;
    r.x = x3;
    r.y = y3;
    r.z = z3;
    r.infinity = 0u;
    return r;
}

// Mixed addition: r = a + b where a is Jacobian, b is affine
fn gej_add_ge(a: Gej, b: Ge) -> Gej {
    if (gej_is_infinity(a)) {
        return gej_set_ge(b);
    }

    // Z1^2 and Z1^3
    var z12 = fe_sqr(a.z);
    var z13 = fe_mul(z12, a.z);

    // U2 = X2 * Z1^2
    var u2 = fe_mul(b.x, z12);

    // S2 = Y2 * Z1^3
    var s2 = fe_mul(b.y, z13);

    // H = U2 - X1
    var h = fe_sub(u2, a.x);

    // R = S2 - Y1
    var rr = fe_sub(s2, a.y);

    // Check for special cases
    if (fe_is_zero(h)) {
        if (fe_is_zero(rr)) {
            // a == b, double
            return gej_double(gej_set_ge(b));
        } else {
            // a == -b
            return gej_infinity();
        }
    }

    // H^2, H^3, U1*H^2
    var h2 = fe_sqr(h);
    var h3 = fe_mul(h2, h);
    var u1h2 = fe_mul(a.x, h2);

    // X3 = R^2 - H^3 - 2*U1*H^2
    var x3 = fe_sqr(rr);
    x3 = fe_sub(x3, h3);
    x3 = fe_sub(x3, u1h2);
    x3 = fe_sub(x3, u1h2);

    // Y3 = R*(U1*H^2 - X3) - Y1*H^3
    var t = fe_sub(u1h2, x3);
    var y3 = fe_mul(rr, t);
    t = fe_mul(a.y, h3);
    y3 = fe_sub(y3, t);

    // Z3 = Z1 * H
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

// Scalar multiplication: r = G * scalar
// Uses double-and-add with 4-bit windows
fn ecmult_gen(scalar: array<u32, 8>) -> Gej {
    var r = gej_infinity();
    let g = ge_generator();

    // Process from MSB to LSB
    for (var limb: i32 = 7; limb >= 0; limb--) {
        var s = scalar[limb];

        for (var bit: i32 = 31; bit >= 0; bit--) {
            // Double
            if (!gej_is_infinity(r)) {
                r = gej_double(r);
            }

            // Add if bit is set
            if (((s >> u32(bit)) & 1u) != 0u) {
                r = gej_add_ge(r, g);
            }
        }
    }

    return r;
}

// Serialize public key to compressed format (33 bytes as prefix + 8 u32s)
fn ge_serialize(p: Ge) -> array<u32, 9> {
    var out: array<u32, 9>;

    // Prefix: 0x02 for even Y, 0x03 for odd Y
    out[0] = select(0x02u, 0x03u, fe_is_odd(p.y));

    // X coordinate in big-endian
    let x_bytes = fe_to_bytes(p.x);
    for (var i: u32 = 0u; i < 8u; i++) {
        out[i + 1u] = x_bytes[i];
    }

    return out;
}

// Serialize to x-only (32 bytes as 8 u32s) for Taproot
fn ge_serialize_xonly(p: Ge) -> array<u32, 8> {
    return fe_to_bytes(p.x);
}
