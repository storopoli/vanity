// hash.wgsl - SHA256 and RIPEMD160 for Bitcoin address generation
//
// Copyright (c) 2025 Jose Storopoli
// MIT License

// ============================================================================
// SHA-256
// ============================================================================

// SHA-256 initial hash values
const SHA256_H: array<u32, 8> = array<u32, 8>(
    0x6a09e667u, 0xbb67ae85u, 0x3c6ef372u, 0xa54ff53au,
    0x510e527fu, 0x9b05688cu, 0x1f83d9abu, 0x5be0cd19u
);

// SHA-256 round constants
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

fn rotr(x: u32, n: u32) -> u32 {
    return (x >> n) | (x << (32u - n));
}

fn sha256_ch(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ (~x & z);
}

fn sha256_maj(x: u32, y: u32, z: u32) -> u32 {
    return (x & y) ^ (x & z) ^ (y & z);
}

fn sha256_ep0(x: u32) -> u32 {
    return rotr(x, 2u) ^ rotr(x, 13u) ^ rotr(x, 22u);
}

fn sha256_ep1(x: u32) -> u32 {
    return rotr(x, 6u) ^ rotr(x, 11u) ^ rotr(x, 25u);
}

fn sha256_sig0(x: u32) -> u32 {
    return rotr(x, 7u) ^ rotr(x, 18u) ^ (x >> 3u);
}

fn sha256_sig1(x: u32) -> u32 {
    return rotr(x, 17u) ^ rotr(x, 19u) ^ (x >> 10u);
}

// SHA-256 compression function for a single 512-bit block (16 u32s)
fn sha256_transform(state: array<u32, 8>, block: array<u32, 16>) -> array<u32, 8> {
    var w: array<u32, 64>;

    // Prepare message schedule
    for (var i: u32 = 0u; i < 16u; i++) {
        w[i] = block[i];
    }
    for (var i: u32 = 16u; i < 64u; i++) {
        w[i] = sha256_sig1(w[i - 2u]) + w[i - 7u] + sha256_sig0(w[i - 15u]) + w[i - 16u];
    }

    var a = state[0]; var b = state[1]; var c = state[2]; var d = state[3];
    var e = state[4]; var f = state[5]; var g = state[6]; var h = state[7];

    // 64 rounds
    for (var i: u32 = 0u; i < 64u; i++) {
        let t1 = h + sha256_ep1(e) + sha256_ch(e, f, g) + SHA256_K[i] + w[i];
        let t2 = sha256_ep0(a) + sha256_maj(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    var result: array<u32, 8>;
    result[0] = state[0] + a; result[1] = state[1] + b;
    result[2] = state[2] + c; result[3] = state[3] + d;
    result[4] = state[4] + e; result[5] = state[5] + f;
    result[6] = state[6] + g; result[7] = state[7] + h;

    return result;
}

// SHA-256 hash of 33 bytes (compressed pubkey)
// Returns 8 u32s (32 bytes) in big-endian
fn sha256_33(data: array<u32, 9>) -> array<u32, 8> {
    // Pad to 512 bits (64 bytes)
    // data is prefix (1 byte) + x coord (32 bytes) = 33 bytes
    var block: array<u32, 16>;

    // First u32: prefix byte in high position + first 3 bytes of x
    block[0] = (data[0] << 24u) | (data[1] >> 8u);
    block[1] = (data[1] << 24u) | (data[2] >> 8u);
    block[2] = (data[2] << 24u) | (data[3] >> 8u);
    block[3] = (data[3] << 24u) | (data[4] >> 8u);
    block[4] = (data[4] << 24u) | (data[5] >> 8u);
    block[5] = (data[5] << 24u) | (data[6] >> 8u);
    block[6] = (data[6] << 24u) | (data[7] >> 8u);
    block[7] = (data[7] << 24u) | (data[8] >> 8u);
    block[8] = (data[8] << 24u) | 0x800000u;  // Last byte + padding bit

    // Zero padding
    for (var i: u32 = 9u; i < 14u; i++) {
        block[i] = 0u;
    }

    // Length in bits (33 * 8 = 264 = 0x108)
    block[14] = 0u;
    block[15] = 264u;

    return sha256_transform(SHA256_H, block);
}

// ============================================================================
// RIPEMD-160
// ============================================================================

const RIPEMD160_H: array<u32, 5> = array<u32, 5>(
    0x67452301u, 0xefcdab89u, 0x98badcfeu, 0x10325476u, 0xc3d2e1f0u
);

fn rotl(x: u32, n: u32) -> u32 {
    return (x << n) | (x >> (32u - n));
}

fn ripemd160_f(x: u32, y: u32, z: u32) -> u32 { return x ^ y ^ z; }
fn ripemd160_g(x: u32, y: u32, z: u32) -> u32 { return (x & y) | (~x & z); }
fn ripemd160_h(x: u32, y: u32, z: u32) -> u32 { return (x | ~y) ^ z; }
fn ripemd160_i(x: u32, y: u32, z: u32) -> u32 { return (x & z) | (y & ~z); }
fn ripemd160_j(x: u32, y: u32, z: u32) -> u32 { return x ^ (y | ~z); }

// Message schedule indices for left rounds
const R_L: array<u32, 80> = array<u32, 80>(
    0u, 1u, 2u, 3u, 4u, 5u, 6u, 7u, 8u, 9u, 10u, 11u, 12u, 13u, 14u, 15u,
    7u, 4u, 13u, 1u, 10u, 6u, 15u, 3u, 12u, 0u, 9u, 5u, 2u, 14u, 11u, 8u,
    3u, 10u, 14u, 4u, 9u, 15u, 8u, 1u, 2u, 7u, 0u, 6u, 13u, 11u, 5u, 12u,
    1u, 9u, 11u, 10u, 0u, 8u, 12u, 4u, 13u, 3u, 7u, 15u, 14u, 5u, 6u, 2u,
    4u, 0u, 5u, 9u, 7u, 12u, 2u, 10u, 14u, 1u, 3u, 8u, 11u, 6u, 15u, 13u
);

// Message schedule indices for right rounds
const R_R: array<u32, 80> = array<u32, 80>(
    5u, 14u, 7u, 0u, 9u, 2u, 11u, 4u, 13u, 6u, 15u, 8u, 1u, 10u, 3u, 12u,
    6u, 11u, 3u, 7u, 0u, 13u, 5u, 10u, 14u, 15u, 8u, 12u, 4u, 9u, 1u, 2u,
    15u, 5u, 1u, 3u, 7u, 14u, 6u, 9u, 11u, 8u, 12u, 2u, 10u, 0u, 4u, 13u,
    8u, 6u, 4u, 1u, 3u, 11u, 15u, 0u, 5u, 12u, 2u, 13u, 9u, 7u, 10u, 14u,
    12u, 15u, 10u, 4u, 1u, 5u, 8u, 7u, 6u, 2u, 13u, 14u, 0u, 3u, 9u, 11u
);

// Rotation amounts for left rounds
const S_L: array<u32, 80> = array<u32, 80>(
    11u, 14u, 15u, 12u, 5u, 8u, 7u, 9u, 11u, 13u, 14u, 15u, 6u, 7u, 9u, 8u,
    7u, 6u, 8u, 13u, 11u, 9u, 7u, 15u, 7u, 12u, 15u, 9u, 11u, 7u, 13u, 12u,
    11u, 13u, 6u, 7u, 14u, 9u, 13u, 15u, 14u, 8u, 13u, 6u, 5u, 12u, 7u, 5u,
    11u, 12u, 14u, 15u, 14u, 15u, 9u, 8u, 9u, 14u, 5u, 6u, 8u, 6u, 5u, 12u,
    9u, 15u, 5u, 11u, 6u, 8u, 13u, 12u, 5u, 12u, 13u, 14u, 11u, 8u, 5u, 6u
);

// Rotation amounts for right rounds
const S_R: array<u32, 80> = array<u32, 80>(
    8u, 9u, 9u, 11u, 13u, 15u, 15u, 5u, 7u, 7u, 8u, 11u, 14u, 14u, 12u, 6u,
    9u, 13u, 15u, 7u, 12u, 8u, 9u, 11u, 7u, 7u, 12u, 7u, 6u, 15u, 13u, 11u,
    9u, 7u, 15u, 11u, 8u, 6u, 6u, 14u, 12u, 13u, 5u, 14u, 13u, 13u, 7u, 5u,
    15u, 5u, 8u, 11u, 14u, 14u, 6u, 14u, 6u, 9u, 12u, 9u, 12u, 5u, 15u, 8u,
    8u, 5u, 12u, 9u, 12u, 5u, 14u, 6u, 8u, 13u, 6u, 5u, 15u, 13u, 11u, 11u
);

// RIPEMD-160 compression for SHA-256 output (32 bytes = 8 u32s big-endian)
// Returns 5 u32s (20 bytes) in little-endian
fn ripemd160_32(data: array<u32, 8>) -> array<u32, 5> {
    // Convert big-endian input to little-endian for RIPEMD
    var x: array<u32, 16>;
    for (var i: u32 = 0u; i < 8u; i++) {
        let be = data[i];
        // Byte swap
        x[i] = ((be & 0xFFu) << 24u) | ((be & 0xFF00u) << 8u) |
               ((be >> 8u) & 0xFF00u) | ((be >> 24u) & 0xFFu);
    }

    // Padding: 0x80, zeros, then length (32*8 = 256 bits = 0x100)
    x[8] = 0x80u;
    for (var i: u32 = 9u; i < 14u; i++) {
        x[i] = 0u;
    }
    x[14] = 256u;  // Length in bits (little-endian)
    x[15] = 0u;

    var al = RIPEMD160_H[0]; var bl = RIPEMD160_H[1]; var cl = RIPEMD160_H[2];
    var dl = RIPEMD160_H[3]; var el = RIPEMD160_H[4];
    var ar = RIPEMD160_H[0]; var br = RIPEMD160_H[1]; var cr = RIPEMD160_H[2];
    var dr = RIPEMD160_H[3]; var er = RIPEMD160_H[4];

    // 80 rounds
    for (var j: u32 = 0u; j < 80u; j++) {
        var fl: u32; var kl: u32;
        var fr: u32; var kr: u32;

        if (j < 16u) {
            fl = ripemd160_f(bl, cl, dl); kl = 0x00000000u;
            fr = ripemd160_j(br, cr, dr); kr = 0x50a28be6u;
        } else if (j < 32u) {
            fl = ripemd160_g(bl, cl, dl); kl = 0x5a827999u;
            fr = ripemd160_i(br, cr, dr); kr = 0x5c4dd124u;
        } else if (j < 48u) {
            fl = ripemd160_h(bl, cl, dl); kl = 0x6ed9eba1u;
            fr = ripemd160_h(br, cr, dr); kr = 0x6d703ef3u;
        } else if (j < 64u) {
            fl = ripemd160_i(bl, cl, dl); kl = 0x8f1bbcdcu;
            fr = ripemd160_g(br, cr, dr); kr = 0x7a6d76e9u;
        } else {
            fl = ripemd160_j(bl, cl, dl); kl = 0xa953fd4eu;
            fr = ripemd160_f(br, cr, dr); kr = 0x00000000u;
        }

        var t = rotl(al + fl + x[R_L[j]] + kl, S_L[j]) + el;
        al = el; el = dl; dl = rotl(cl, 10u); cl = bl; bl = t;

        t = rotl(ar + fr + x[R_R[j]] + kr, S_R[j]) + er;
        ar = er; er = dr; dr = rotl(cr, 10u); cr = br; br = t;
    }

    var result: array<u32, 5>;
    let t = RIPEMD160_H[1] + cl + dr;
    result[0] = RIPEMD160_H[2] + dl + er;
    result[1] = RIPEMD160_H[3] + el + ar;
    result[2] = RIPEMD160_H[4] + al + br;
    result[3] = RIPEMD160_H[0] + bl + cr;
    result[4] = t;

    return result;
}

// Hash160: RIPEMD160(SHA256(data))
// Input: 33-byte compressed pubkey (9 u32s)
// Output: 20-byte hash (5 u32s in little-endian)
fn hash160(pubkey: array<u32, 9>) -> array<u32, 5> {
    let sha = sha256_33(pubkey);
    return ripemd160_32(sha);
}
