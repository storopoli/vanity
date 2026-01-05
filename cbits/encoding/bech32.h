/*
 * bech32.h - Bech32 and Bech32m encoding for Bitcoin SegWit addresses
 *
 * Bech32 is used for P2WPKH (SegWit v0) addresses
 * Bech32m is used for P2TR (Taproot/SegWit v1) addresses
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_ENCODING_BECH32_H
#define VANITY_ENCODING_BECH32_H

#include "../common.h"

/* Bech32 encoding variant */
typedef enum {
    BECH32_ENCODING,   /* BIP-173 (SegWit v0) */
    BECH32M_ENCODING   /* BIP-350 (Taproot/SegWit v1+) */
} bech32_encoding;

/*
 * Encode data to Bech32/Bech32m
 *
 * @param out       Output buffer
 * @param out_len   Pointer to output buffer size, updated with actual length
 * @param hrp       Human-readable part (e.g., "bc" for mainnet, "tb" for testnet)
 * @param data      Witness program (already converted to 5-bit values)
 * @param data_len  Length of data in 5-bit values
 * @param encoding  BECH32_ENCODING or BECH32M_ENCODING
 * @return          0 on success, -1 on error
 */
int bech32_encode(
    char *out,
    size_t *out_len,
    const char *hrp,
    const uint8_t *data,
    size_t data_len,
    bech32_encoding encoding
);

/*
 * Convert 8-bit data to 5-bit groups for Bech32 encoding
 *
 * @param out       Output buffer (must be at least len * 8 / 5 + 1 bytes)
 * @param out_len   Pointer to output length (updated with actual length)
 * @param data      Input 8-bit data
 * @param len       Input data length
 * @return          0 on success, -1 on error
 */
int convert_bits_8to5(uint8_t *out, size_t *out_len, const uint8_t *data, size_t len);

/*
 * Encode P2WPKH address (SegWit v0, Bech32)
 *
 * @param out       Output buffer (at least 63 bytes for mainnet)
 * @param out_len   Pointer to output length
 * @param hash160   20-byte pubkey hash (RIPEMD160(SHA256(pubkey)))
 * @param mainnet   1 for mainnet ("bc1q..."), 0 for testnet ("tb1q...")
 * @return          0 on success
 */
int encode_p2wpkh(char *out, size_t *out_len, const uint8_t hash160[20], int mainnet);

/*
 * Encode P2TR address (Taproot/SegWit v1, Bech32m)
 *
 * @param out       Output buffer (at least 63 bytes for mainnet)
 * @param out_len   Pointer to output length
 * @param xonly     32-byte x-only public key
 * @param mainnet   1 for mainnet ("bc1p..."), 0 for testnet ("tb1p...")
 * @return          0 on success
 */
int encode_p2tr(char *out, size_t *out_len, const uint8_t xonly[32], int mainnet);

#endif /* VANITY_ENCODING_BECH32_H */
