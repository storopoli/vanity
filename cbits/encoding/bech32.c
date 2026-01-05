/*
 * bech32.c - Bech32 and Bech32m encoding implementation
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "bech32.h"

/* Bech32 character set */
static const char BECH32_CHARSET[] = "qpzry9x8gf2tvdw0s3jn54khce6mua7l";

/* Bech32 generator polynomial coefficients */
static const uint32_t BECH32_GEN[5] = {
    0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3
};

/* Bech32 final constant */
#define BECH32_CONST 1

/* Bech32m final constant (BIP-350) */
#define BECH32M_CONST 0x2bc830a3

/*
 * Expand human-readable part for checksum computation
 */
static void bech32_hrp_expand(uint8_t *out, const char *hrp, size_t hrp_len) {
    for (size_t i = 0; i < hrp_len; i++) {
        out[i] = hrp[i] >> 5;
    }
    out[hrp_len] = 0;
    for (size_t i = 0; i < hrp_len; i++) {
        out[hrp_len + 1 + i] = hrp[i] & 0x1f;
    }
}

/*
 * Compute Bech32 polymod
 */
static uint32_t bech32_polymod(const uint8_t *values, size_t len) {
    uint32_t chk = 1;
    for (size_t i = 0; i < len; i++) {
        uint8_t top = chk >> 25;
        chk = ((chk & 0x1ffffff) << 5) ^ values[i];
        for (int j = 0; j < 5; j++) {
            if ((top >> j) & 1) {
                chk ^= BECH32_GEN[j];
            }
        }
    }
    return chk;
}

/*
 * Create checksum for Bech32/Bech32m
 */
static void bech32_create_checksum(
    uint8_t checksum[6],
    const char *hrp,
    const uint8_t *data,
    size_t data_len,
    bech32_encoding encoding
) {
    size_t hrp_len = strlen(hrp);
    size_t total_len = hrp_len * 2 + 1 + data_len + 6;

    uint8_t *values = (uint8_t *)malloc(total_len);
    if (!values) return;

    /* HRP expansion */
    bech32_hrp_expand(values, hrp, hrp_len);

    /* Data */
    memcpy(values + hrp_len * 2 + 1, data, data_len);

    /* Padding for checksum computation */
    memset(values + hrp_len * 2 + 1 + data_len, 0, 6);

    /* Compute polymod */
    uint32_t target = (encoding == BECH32M_ENCODING) ? BECH32M_CONST : BECH32_CONST;
    uint32_t polymod = bech32_polymod(values, total_len) ^ target;

    free(values);

    /* Extract checksum */
    for (int i = 0; i < 6; i++) {
        checksum[i] = (polymod >> (5 * (5 - i))) & 0x1f;
    }
}

int bech32_encode(
    char *out,
    size_t *out_len,
    const char *hrp,
    const uint8_t *data,
    size_t data_len,
    bech32_encoding encoding
) {
    size_t hrp_len = strlen(hrp);
    size_t total_len = hrp_len + 1 + data_len + 6;  /* hrp + '1' + data + checksum */

    if (total_len + 1 > *out_len) {
        *out_len = total_len + 1;
        return -1;
    }

    /* Compute checksum */
    uint8_t checksum[6];
    bech32_create_checksum(checksum, hrp, data, data_len, encoding);

    /* Build output string */
    size_t pos = 0;

    /* HRP (lowercase) */
    for (size_t i = 0; i < hrp_len; i++) {
        out[pos++] = (char)(hrp[i] | 0x20);  /* Force lowercase */
    }

    /* Separator */
    out[pos++] = '1';

    /* Data */
    for (size_t i = 0; i < data_len; i++) {
        out[pos++] = BECH32_CHARSET[data[i]];
    }

    /* Checksum */
    for (int i = 0; i < 6; i++) {
        out[pos++] = BECH32_CHARSET[checksum[i]];
    }

    out[pos] = '\0';
    *out_len = pos;

    return 0;
}

int convert_bits_8to5(uint8_t *out, size_t *out_len, const uint8_t *data, size_t len) {
    uint32_t acc = 0;
    int bits = 0;
    size_t out_pos = 0;
    size_t max_out = *out_len;

    for (size_t i = 0; i < len; i++) {
        acc = (acc << 8) | data[i];
        bits += 8;

        while (bits >= 5) {
            bits -= 5;
            if (out_pos >= max_out) {
                *out_len = out_pos;
                return -1;
            }
            out[out_pos++] = (acc >> bits) & 0x1f;
        }
    }

    /* Pad with zeros if needed */
    if (bits > 0) {
        if (out_pos >= max_out) {
            *out_len = out_pos;
            return -1;
        }
        out[out_pos++] = (acc << (5 - bits)) & 0x1f;
    }

    *out_len = out_pos;
    return 0;
}

int encode_p2wpkh(char *out, size_t *out_len, const uint8_t hash160[20], int mainnet) {
    const char *hrp = mainnet ? "bc" : "tb";

    /* Convert 20 bytes (160 bits) to 5-bit groups */
    /* 160 / 5 = 32, but we need witness version prefix */
    uint8_t data[33];  /* 1 (version) + 32 (converted hash) */
    size_t data_len = 33;

    /* Witness version 0 */
    data[0] = 0;

    /* Convert hash160 to 5-bit groups */
    size_t converted_len = 32;
    if (convert_bits_8to5(data + 1, &converted_len, hash160, 20) != 0) {
        return -1;
    }
    data_len = 1 + converted_len;

    return bech32_encode(out, out_len, hrp, data, data_len, BECH32_ENCODING);
}

int encode_p2tr(char *out, size_t *out_len, const uint8_t xonly[32], int mainnet) {
    const char *hrp = mainnet ? "bc" : "tb";

    /* Convert 32 bytes (256 bits) to 5-bit groups */
    /* 256 / 5 = 51.2, so we need 52 groups, plus witness version */
    uint8_t data[53];  /* 1 (version) + 52 (converted key) */
    size_t data_len = 53;

    /* Witness version 1 (Taproot) */
    data[0] = 1;

    /* Convert x-only pubkey to 5-bit groups */
    size_t converted_len = 52;
    if (convert_bits_8to5(data + 1, &converted_len, xonly, 32) != 0) {
        return -1;
    }
    data_len = 1 + converted_len;

    return bech32_encode(out, out_len, hrp, data, data_len, BECH32M_ENCODING);
}
