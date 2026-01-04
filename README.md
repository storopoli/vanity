# vanity

![](https://img.shields.io/badge/license-MIT-brightgreen)

A pure Haskell Bitcoin vanity address generator.

## Features

- **Address Types**: P2PKH (Base58), P2WPKH (Bech32), P2TR (Bech32m/Taproot)
- **Networks**: Mainnet and Testnet
- **Pattern Matching**: POSIX regex with case-insensitive option
- **Performance**: Multi-threaded CPU backend using all available cores
- **Pure Haskell**: Uses [ppad](https://github.com/ppad-tech) cryptographic libraries

## Usage

```console
$ vanity --help
vanity - Bitcoin vanity address generator

Usage: vanity PATTERN [-n|--network NETWORK] [-t|--type TYPE]
              [-b|--backend BACKEND] [-j|--threads N] [-i|--ignore-case]
              [-q|--quiet] [--show-private]

  Generate Bitcoin vanity addresses matching a regex pattern

Available options:
  PATTERN                  Regex pattern to match in the address
  -n,--network NETWORK     Bitcoin network: mainnet or testnet (default: mainnet)
  -t,--type TYPE           Address type: p2pkh, p2wpkh, or p2tr (default: p2pkh)
  -j,--threads N           Number of threads (default: all cores)
  -i,--ignore-case         Case-insensitive pattern matching
  -q,--quiet               Suppress progress output
  --show-private           Display the private key (WIF and hex) in output
  -h,--help                Show this help text
```

### Examples

Find a P2PKH address starting with "1ABC":

```console
$ vanity '^1ABC' --show-private
Searching for pattern: ^1ABC
Network: mainnet
Address type: p2pkh
Case sensitive: True

=== FOUND ===
Address: 1ABCxyz...
Attempts: 12345

=== PRIVATE KEY (KEEP SECRET!) ===
WIF: L2FP4Lx46qxT8JrBXG9z32okmNfAzDgDP1N33bfXkkg8sUSw2gTT
Hex: 960bd4ea50d05df54046586616be557afad9d053f6bc5847d57ce3a1e8ae3b98
```

Find a Bech32 SegWit address (case-insensitive):

```console
$ vanity 'abc' -t p2wpkh -i --show-private
```

Find a Taproot address:

```console
$ vanity '^bc1p.*cool' -t p2tr --show-private
```

## Benchmarks (CPU)

Benchmarks run on Apple M4 Max (ARM64):

| Operation | Time |
|-----------|------|
| Generate secret key | 9 μs |
| Derive public key | 1.73 ms |
| Serialize public key (compressed) | 66 μs |
| Encode P2PKH (Base58) | 73 μs |
| Encode P2WPKH (Bech32) | 71 μs |
| Encode P2TR (Bech32m) | 69 μs |
| Match simple pattern (`^1`) | 17 ns |
| Match complex regex (`^1[A-Z]{3}`) | 76 ns |
| **Full pipeline (generate + derive + encode + match)** | **1.85 ms** |

This gives approximately **540 addresses/second** per thread on the full pipeline.

## Documentation

Haddocks (API documentation, etc.) are hosted at [docs.ppad.tech][docs].

## Security

This library uses cryptographically secure random number generation via the
`entropy` package and implements constant-time elliptic curve operations via
`ppad-secp256k1`.

**Warning**: Keep your private keys secret! Only use `--show-private` when you
need the key, and never share the output.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```console
$ nix develop
```

Then do e.g.:

```console
$ cabal repl vanity
```

to get a REPL for the main library.

Run tests:

```console
$ cabal test
```

Run benchmarks:

```console
$ cabal bench
```

[docs]: https://docs.ppad.tech/vanity
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
