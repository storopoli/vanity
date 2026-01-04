/*
 * base58.c - Base58Check encoding implementation
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "base58.h"
#include "../hash/sha256.h"

/* Base58 alphabet (Bitcoin variant - no 0, O, I, l) */
static const char BASE58_ALPHABET[] =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

/*
 * Encode bytes to Base58
 * Uses repeated division by 58
 */
static int base58_encode(char *out, size_t *out_len, const uint8_t *data, size_t len) {
    /* Count leading zeros */
    size_t leading_zeros = 0;
    while (leading_zeros < len && data[leading_zeros] == 0) {
        leading_zeros++;
    }

    /* Allocate enough space (log(256)/log(58) ≈ 1.37, so 138% + 1) */
    size_t max_out = len * 138 / 100 + 1;
    uint8_t *buf = (uint8_t *)out;  /* Temporary use output buffer */

    /* Copy input for modification */
    uint8_t *input = (uint8_t *)malloc(len);
    if (!input) return -1;
    memcpy(input, data, len);

    size_t buf_len = 0;

    /* Process input from left to right */
    for (size_t i = leading_zeros; i < len; ) {
        /* Divide input by 58, storing remainder */
        uint32_t carry = 0;
        int non_zero = 0;

        for (size_t j = i; j < len; j++) {
            uint32_t val = carry * 256 + input[j];
            input[j] = (uint8_t)(val / 58);
            carry = val % 58;
            if (input[j] != 0) non_zero = 1;
        }

        buf[buf_len++] = (uint8_t)carry;

        /* Skip leading zeros in input */
        while (i < len && input[i] == 0 && non_zero) {
            i++;
        }
        if (!non_zero) break;
    }

    free(input);

    /* Add leading '1's for leading zeros in input */
    size_t total_len = leading_zeros + buf_len;
    if (total_len > *out_len) {
        *out_len = total_len;
        return -1;
    }

    /* Output: leading '1's + reversed encoded digits */
    for (size_t i = 0; i < leading_zeros; i++) {
        out[i] = '1';
    }

    for (size_t i = 0; i < buf_len; i++) {
        out[leading_zeros + i] = BASE58_ALPHABET[buf[buf_len - 1 - i]];
    }

    out[total_len] = '\0';
    *out_len = total_len;

    return 0;
}

int base58check_encode(char *out, size_t *out_len, const uint8_t *data, size_t len) {
    /* Compute checksum: first 4 bytes of double SHA256 */
    uint8_t checksum[32];
    sha256d(checksum, data, len);

    /* Create data + checksum */
    size_t total = len + 4;
    uint8_t *with_checksum = (uint8_t *)malloc(total);
    if (!with_checksum) return -1;

    memcpy(with_checksum, data, len);
    memcpy(with_checksum + len, checksum, 4);

    /* Encode */
    int result = base58_encode(out, out_len, with_checksum, total);

    free(with_checksum);
    return result;
}

int encode_p2pkh(char *out, size_t *out_len, const uint8_t hash160[20], int mainnet) {
    /* Version prefix + hash160 */
    uint8_t data[21];
    data[0] = mainnet ? 0x00 : 0x6F;  /* 0x00 = mainnet, 0x6F = testnet */
    memcpy(data + 1, hash160, 20);

    *out_len = 50;  /* Max P2PKH address length */
    return base58check_encode(out, out_len, data, 21);
}
