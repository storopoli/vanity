# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bitcoin vanity address generator in Haskell with GPU acceleration. Generates Bitcoin addresses matching user-specified regex patterns with multi-threaded CPU and GPU support (CUDA/WGPU).

## Build Commands

```bash
just build        # Build the project
just test         # Run tests
just lint         # Run hlint
just format       # Format all code (Haskell, Cabal, Nix)
just benchmark    # Run benchmarks
just repl         # Launch REPL
just doc          # Build and open Haddock docs
just run <ARGS>   # Run CLI (e.g., just run -- -p "^1abc")
```

Nix development shell: `nix develop`

## Architecture

### Address Types
- **P2PKH** (`lib/Crypto/Bitcoin/Vanity/Address/Base58.hs`): Legacy addresses (prefix '1')
- **P2WPKH** (`lib/Crypto/Bitcoin/Vanity/Address/Bech32.hs`): SegWit v0 (prefix 'bc1q')
- **P2TR** (`lib/Crypto/Bitcoin/Vanity/Address/Bech32m.hs`): Taproot (prefix 'bc1p')

### Core Modules
- `Types.hs`: Core types (`Network`, `AddressType`, `SecretKey`, `PublicKey`, `VanityConfig`)
- `Key.hs`: Key generation and derivation from entropy
- `Pattern.hs`: POSIX regex pattern matching
- `Worker.hs`: Backend dispatch (CPU/CUDA/WGPU)
- `Worker/CPU.hs`: Multi-threaded worker using async/STM
- `Worker/CUDA.hs`: NVIDIA CUDA GPU backend
- `Worker/WGPU.hs`: Cross-platform GPU backend (Vulkan/Metal/DirectX)

### GPU Backend Architecture

#### C/CUDA Implementation (`cbits/`)
- `secp256k1/`: Field arithmetic, group operations, scalar multiplication
- `hash/`: SHA256, RIPEMD160 implementations
- `encoding/`: Base58Check, Bech32/Bech32m encoding
- `cuda/`: CUDA kernels for GPU-parallel key derivation
- `vanity_gpu.h`: Public FFI interface

#### WGSL Shaders (`shaders/`)
- `vanity_pipeline.wgsl`: Full search pipeline compute shader
- Uses 8x32-bit limbs for 256-bit arithmetic (WGSL lacks u64)

#### Performance
- CPU: ~500 addresses/sec per core
- CUDA: ~10M addresses/sec on RTX 3080
- WGPU: ~3M addresses/sec on Apple M2 Max

### Data Flow
1. Generate random 256-bit scalar (`Key.hs`)
2. Derive secp256k1 public key
3. Encode as address (Base58/Bech32/Bech32m based on type)
4. Match against regex pattern
5. Output WIF and hex private key on match

## Dependencies

Cryptographic primitives from ppad-tech ecosystem:
- `ppad-secp256k1`: Elliptic curve operations
- `ppad-sha256`, `ppad-ripemd160`: Hash functions
- `ppad-base58`, `ppad-bech32`: Address encoding

## Testing

Tests use tasty with QuickCheck. Run single test:
```bash
cabal test --test-option='-p "/test name pattern/"'
```
