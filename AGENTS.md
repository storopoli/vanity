# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Bitcoin vanity address generator in Haskell with GPU acceleration. Generates Bitcoin addresses matching user-specified regex patterns with multi-threaded CPU and GPU (WGPU) support.

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

Build with GPU support: `cabal build -f wgpu`

## Architecture

### Address Types
- **P2PKH** (`lib/Crypto/Bitcoin/Vanity/Address/Base58.hs`): Legacy addresses (prefix '1')
- **P2WPKH** (`lib/Crypto/Bitcoin/Vanity/Address/Bech32.hs`): SegWit v0 (prefix 'bc1q')
- **P2TR** (`lib/Crypto/Bitcoin/Vanity/Address/Bech32m.hs`): Taproot (prefix 'bc1p')

### Core Modules
- `Types.hs`: Core types (`Network`, `AddressType`, `SecretKey`, `PublicKey`, `VanityConfig`)
- `Key.hs`: Key generation and derivation from entropy
- `Pattern.hs`: POSIX regex pattern matching
- `Worker.hs`: Backend dispatch (CPU/WGPU)
- `Worker/CPU.hs`: Multi-threaded worker using async/STM
- `Worker/WGPU.hs`: Hybrid GPU backend (EC on GPU, hashing on CPU)

### GPU Hybrid Architecture

The GPU backend uses a **hybrid approach** where:
- **GPU (WGSL shader)**: EC scalar multiplication (93% of compute time)
- **CPU (Haskell)**: Random scalar generation, Hash160, Bech32 encoding, pattern matching

This provides reliability (CPU-side crypto uses battle-tested ppad libraries) while still
achieving significant speedup from GPU-accelerated EC multiplication.

#### Files
- `shaders/ecmult_hybrid.wgsl`: WGSL compute shader for EC multiplication
- `cbits/wgpu_vanity.c`: C FFI wrapper for wgpu-native
- `cbits/wgpu_vanity.h`: C FFI header
- `lib/Crypto/Bitcoin/Vanity/FFI/WGPU.hsc`: Haskell FFI bindings
- `lib/Crypto/Bitcoin/Vanity/Worker/WGPU.hs`: Hybrid worker implementation

#### WGSL Shader Details
- Uses 8x32-bit limbs for 256-bit field elements (WGSL lacks u64)
- Pure u32 arithmetic with manual carry propagation
- secp256k1 field operations: add, sub, mul, inv (Fermat)
- Jacobian projective coordinates for EC point operations
- Double-and-add scalar multiplication

#### Data Flow (GPU Hybrid)
```
1. CPU: Generate random 256-bit base scalar
2. GPU: For each index i in batch:
   - Compute scalar = base_scalar + i
   - P = scalar * G (EC multiplication)
   - Return (scalar, P.x, P.y_parity)
3. CPU: For each result:
   - Serialize compressed pubkey (prefix || X)
   - Hash160 = RIPEMD160(SHA256(pubkey))
   - Encode as Bech32/Bech32m address
   - Match against regex pattern
4. CPU: Return first match with private key
```

### Performance

| Backend | Throughput | Notes |
|---------|------------|-------|
| CPU (1 thread) | ~560 keys/sec | Full pipeline in Haskell |
| CPU (16 cores) | ~9,000 keys/sec | Multi-threaded |
| GPU (WGPU) | ~88,000 keys/sec | Hybrid EC on GPU (Apple M4 Max) |

GPU provides ~157x speedup over single CPU thread, ~10x over 16-core CPU.

### Data Flow (CPU)
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

GPU backend:
- `wgpu-native`: Cross-platform GPU compute (Metal/Vulkan/DirectX)

## Testing

Tests use tasty with QuickCheck. Run single test:
```bash
cabal test --test-option='-p "/test name pattern/"'
```

Run with GPU support:
```bash
cabal test -f wgpu
```
