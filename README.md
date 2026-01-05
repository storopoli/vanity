# vanity

![](https://img.shields.io/badge/license-MIT-brightgreen)

A pure Haskell Bitcoin vanity address generator with GPU acceleration.

## Features

- **Address Types**: P2PKH (Base58), P2WPKH (Bech32), P2TR (Bech32m/Taproot)
- **Networks**: Mainnet and Testnet
- **Pattern Matching**: POSIX regex with case-insensitive option
- **Backends**:
  - **CPU**: Multi-threaded using all available cores
  - **GPU (Hybrid)**: WGPU backend (Metal/Vulkan/DirectX) with EC on GPU, hashing on CPU
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
  -b,--backend BACKEND     Backend: cpu or wgpu (default: cpu)
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

Find a Bech32 SegWit address using GPU backend:

```console
$ vanity 'abc' -t p2wpkh -b wgpu --show-private
Using GPU (hybrid): Apple M4 Max
...
```

Find a Taproot address (case-insensitive):

```console
$ vanity '^bc1p.*cool' -t p2tr -i --show-private
```

## Building with GPU Support

GPU support requires the `wgpu` flag:

```console
$ cabal build -f wgpu
$ cabal run -f wgpu vanity -- "bc1q" -b wgpu -t p2wpkh
```

The WGPU backend uses [wgpu-native](https://github.com/gfx-rs/wgpu-native) for
cross-platform GPU compute:
- **macOS**: Metal backend
- **Linux**: Vulkan backend
- **Windows**: DirectX 12 or Vulkan backend

## Benchmarks

Benchmarks run on Apple M4 Max (ARM64):

### CPU Backend (Single Thread)

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

**CPU Throughput**: ~560 addresses/second per thread

### GPU Hybrid Backend (WGPU/Metal)

| Metric | Value |
|--------|-------|
| Batch size | 16,384 keys |
| Keys per second | ~88,000 |
| Speedup vs single CPU thread | **~157x** |
| Speedup vs 16-core CPU | **~10x** |

The GPU hybrid approach offloads secp256k1 EC scalar multiplication (93% of compute time)
to the GPU while keeping hashing and pattern matching on the CPU for reliability.

### Backend Comparison

| Backend | Throughput | Best For |
|---------|------------|----------|
| CPU (1 thread) | ~560 keys/sec | Simple patterns |
| CPU (16 cores) | ~9,000 keys/sec | Medium patterns |
| GPU (WGPU) | ~88,000 keys/sec | Complex patterns, long searches |

**Note**: P2PKH addresses are only supported on the CPU backend due to Base58
encoding complexity on GPU.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                        vanity CLI                           │
├─────────────────────────────────────────────────────────────┤
│                     Worker Dispatcher                       │
├──────────────────────────┬──────────────────────────────────┤
│      CPU Backend         │        GPU Hybrid Backend        │
│  ┌────────────────────┐  │  ┌────────────────────────────┐  │
│  │ Secret Key Gen     │  │  │ GPU (WGSL/Metal):          │  │
│  │ EC Multiplication  │  │  │   - EC Scalar Multiply     │  │
│  │ Hash160            │  │  │   - Return Public Keys     │  │
│  │ Address Encoding   │  │  ├────────────────────────────┤  │
│  │ Pattern Matching   │  │  │ CPU:                       │  │
│  └────────────────────┘  │  │   - Hash160 (SHA256+RMD)   │  │
│                          │  │   - Bech32/Bech32m Encode  │  │
│                          │  │   - Regex Pattern Match    │  │
│                          │  └────────────────────────────┘  │
└──────────────────────────┴──────────────────────────────────┘
```

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

Build with GPU support:

```console
$ cabal build -f wgpu
```

[docs]: https://docs.ppad.tech/vanity
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
