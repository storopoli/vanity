// encoding.wgsl - Base58Check and Bech32/Bech32m encoding
//
// Copyright (c) 2025 Jose Storopoli
// MIT License

// ============================================================================
// Base58 Encoding (for P2PKH addresses)
// ============================================================================

// Base58 alphabet (no 0, O, I, l)
const BASE58_ALPHABET: array<u32, 58> = array<u32, 58>(
    0x31u, 0x32u, 0x33u, 0x34u, 0x35u, 0x36u, 0x37u, 0x38u, 0x39u,  // 1-9
    0x41u, 0x42u, 0x43u, 0x44u, 0x45u, 0x46u, 0x47u, 0x48u,          // A-H
    0x4Au, 0x4Bu, 0x4Cu, 0x4Du, 0x4Eu,                                // J-N
    0x50u, 0x51u, 0x52u, 0x53u, 0x54u, 0x55u, 0x56u, 0x57u, 0x58u, 0x59u, 0x5Au,  // P-Z
    0x61u, 0x62u, 0x63u, 0x64u, 0x65u, 0x66u, 0x67u, 0x68u, 0x69u,  // a-i
    0x6Au, 0x6Bu,                                                    // j-k
    0x6Du, 0x6Eu, 0x6Fu, 0x70u, 0x71u, 0x72u, 0x73u, 0x74u, 0x75u, 0x76u, 0x77u, 0x78u, 0x79u, 0x7Au  // m-z
);

// Encode P2PKH address (version + hash160 + checksum = 25 bytes -> ~34 chars)
// Input: version (1 byte), hash160 (5 u32s little-endian)
// Output: up to 35 characters as packed bytes
struct Base58Result {
    chars: array<u32, 9>,  // 35 chars packed as u32s
    len: u32,
}

fn base58_encode_p2pkh(version: u32, hash160: array<u32, 5>) -> Base58Result {
    // Build 25-byte input: version + hash160 + checksum
    var data: array<u32, 7>;  // 25 bytes as 7 u32s (with padding)

    // Version byte (high byte of first u32)
    // Hash160 is little-endian, need to convert to big-endian bytes
    data[0] = (version << 24u) |
              ((hash160[0] & 0xFFu) << 16u) |
              ((hash160[0] >> 8u) & 0xFF00u) |
              ((hash160[0] >> 24u) & 0xFFu);

    // Continue packing hash160 bytes
    data[1] = ((hash160[0] >> 16u) & 0xFF00u) |
              ((hash160[1] & 0xFFu) << 16u) |
              ((hash160[1] >> 8u) & 0xFF00u) |
              ((hash160[1] >> 24u) & 0xFFu);

    // ... more byte packing needed for full implementation

    // For GPU efficiency, use simplified division approach
    var result: Base58Result;

    // Placeholder: return empty result
    // Full implementation requires multi-precision division by 58
    result.len = 0u;

    return result;
}

// ============================================================================
// Bech32/Bech32m Encoding (for P2WPKH and P2TR addresses)
// ============================================================================

// Bech32 character set
const BECH32_CHARSET: array<u32, 32> = array<u32, 32>(
    0x71u, 0x70u, 0x7Au, 0x72u, 0x79u, 0x39u, 0x78u, 0x38u,  // qpzry9x8
    0x67u, 0x66u, 0x32u, 0x74u, 0x76u, 0x64u, 0x77u, 0x30u,  // gf2tvdw0
    0x73u, 0x33u, 0x6Au, 0x6Eu, 0x35u, 0x34u, 0x6Bu, 0x68u,  // s3jn54kh
    0x63u, 0x65u, 0x36u, 0x6Du, 0x75u, 0x61u, 0x37u, 0x6Cu   // ce6mua7l
);

// Bech32 generator polynomial
const BECH32_GEN: array<u32, 5> = array<u32, 5>(
    0x3b6a57b2u, 0x26508e6du, 0x1ea119fau, 0x3d4233ddu, 0x2a1462b3u
);

// Bech32 checksum constant
const BECH32_CONST: u32 = 1u;
// Bech32m checksum constant
const BECH32M_CONST: u32 = 0x2bc830a3u;

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

// Convert 8-bit data to 5-bit groups
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
        out_idx++;
    }

    return out;
}

struct Bech32Result {
    chars: array<u32, 16>,  // 62 chars max packed as u32s
    len: u32,
}

// Encode P2WPKH address (bc1q...)
// Input: hash160 (5 u32s little-endian)
// Output: bech32 encoded address
fn encode_p2wpkh(hash160: array<u32, 5>, mainnet: bool) -> Bech32Result {
    var result: Bech32Result;

    // HRP: "bc" for mainnet, "tb" for testnet
    let hrp0: u32 = select(0x74u, 0x62u, mainnet);  // 't' or 'b'
    let hrp1: u32 = select(0x62u, 0x63u, mainnet);  // 'b' or 'c'

    // Convert hash160 to bytes (little-endian to bytes)
    var hash_bytes: array<u32, 8>;
    for (var i: u32 = 0u; i < 5u; i++) {
        let word = hash160[i];
        hash_bytes[i] = ((word & 0xFFu) << 24u) |
                        ((word >> 8u) & 0xFF0000u) |
                        ((word >> 8u) & 0xFF00u) |
                        ((word >> 24u) & 0xFFu);
    }

    // Convert 20 bytes to 5-bit groups (32 groups)
    let data5 = convert_bits_8to5(hash_bytes, 20u);

    // Build checksum input: hrp_expand + data + [0,0,0,0,0,0]
    var checksum_input: array<u32, 64>;
    // HRP expansion: high bits, 0, low bits
    checksum_input[0] = hrp0 >> 5u;
    checksum_input[1] = hrp1 >> 5u;
    checksum_input[2] = 0u;
    checksum_input[3] = hrp0 & 0x1Fu;
    checksum_input[4] = hrp1 & 0x1Fu;

    // Witness version 0
    checksum_input[5] = 0u;

    // Data (32 groups)
    for (var i: u32 = 0u; i < 32u; i++) {
        checksum_input[6u + i] = data5[i];
    }

    // Padding for checksum
    for (var i: u32 = 0u; i < 6u; i++) {
        checksum_input[38u + i] = 0u;
    }

    // Compute checksum
    let polymod = bech32_polymod(checksum_input, 44u) ^ BECH32_CONST;

    // Build output: hrp + '1' + witness_version + data + checksum
    var out_idx: u32 = 0u;

    // HRP + separator
    result.chars[0] = (hrp0 << 24u) | (hrp1 << 16u) | (0x31u << 8u);  // "bc1" or "tb1"

    // Witness version (q = 0)
    result.chars[0] |= BECH32_CHARSET[0];  // 'q'
    out_idx = 4u;

    // Data characters
    for (var i: u32 = 0u; i < 32u; i++) {
        let char_idx = out_idx / 4u;
        let char_pos = 3u - (out_idx % 4u);
        result.chars[char_idx] |= BECH32_CHARSET[data5[i]] << (char_pos * 8u);
        out_idx++;
    }

    // Checksum characters (6)
    for (var i: u32 = 0u; i < 6u; i++) {
        let char_idx = out_idx / 4u;
        let char_pos = 3u - (out_idx % 4u);
        let c = (polymod >> (5u * (5u - i))) & 0x1Fu;
        result.chars[char_idx] |= BECH32_CHARSET[c] << (char_pos * 8u);
        out_idx++;
    }

    result.len = 42u;  // "bc1q" + 32 data + 6 checksum = 42 chars

    return result;
}

// Encode P2TR address (bc1p...)
// Input: x-only pubkey (8 u32s big-endian)
fn encode_p2tr(xonly: array<u32, 8>, mainnet: bool) -> Bech32Result {
    var result: Bech32Result;

    // Similar to P2WPKH but:
    // - Witness version 1 (p instead of q)
    // - 32 bytes of data (52 five-bit groups)
    // - Use BECH32M_CONST instead of BECH32_CONST

    let hrp0: u32 = select(0x74u, 0x62u, mainnet);
    let hrp1: u32 = select(0x62u, 0x63u, mainnet);

    // Convert 32 bytes to 5-bit groups (52 groups)
    let data5 = convert_bits_8to5(xonly, 32u);

    // Build checksum input
    var checksum_input: array<u32, 64>;
    checksum_input[0] = hrp0 >> 5u;
    checksum_input[1] = hrp1 >> 5u;
    checksum_input[2] = 0u;
    checksum_input[3] = hrp0 & 0x1Fu;
    checksum_input[4] = hrp1 & 0x1Fu;

    // Witness version 1
    checksum_input[5] = 1u;

    // Data (52 groups)
    for (var i: u32 = 0u; i < 52u; i++) {
        checksum_input[6u + i] = data5[i];
    }

    // Padding
    for (var i: u32 = 0u; i < 6u; i++) {
        checksum_input[58u + i] = 0u;
    }

    let polymod = bech32_polymod(checksum_input, 64u) ^ BECH32M_CONST;

    // Build output
    result.chars[0] = (hrp0 << 24u) | (hrp1 << 16u) | (0x31u << 8u) | BECH32_CHARSET[1];  // "bc1p"

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

    result.len = 62u;  // "bc1p" + 52 data + 6 checksum = 62 chars

    return result;
}
