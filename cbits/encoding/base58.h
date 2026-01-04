/*
 * base58.h - Base58Check encoding for Bitcoin P2PKH addresses
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#ifndef VANITY_ENCODING_BASE58_H
#define VANITY_ENCODING_BASE58_H

#include "../common.h"

/*
 * Encode data with Base58Check
 *
 * @param out     Output buffer (must be large enough, max ~138 chars for 100 bytes)
 * @param out_len Pointer to output length (updated with actual length)
 * @param data    Input data
 * @param len     Input data length
 * @return        0 on success, -1 on error
 */
int base58check_encode(char *out, size_t *out_len, const uint8_t *data, size_t len);

/*
 * Encode P2PKH address
 *
 * @param out      Output buffer (at least 35 bytes)
 * @param out_len  Pointer to output length
 * @param hash160  20-byte pubkey hash (RIPEMD160(SHA256(pubkey)))
 * @param mainnet  1 for mainnet (prefix '1'), 0 for testnet (prefix 'm' or 'n')
 * @return         0 on success
 */
int encode_p2pkh(char *out, size_t *out_len, const uint8_t hash160[20], int mainnet);

#endif /* VANITY_ENCODING_BASE58_H */
