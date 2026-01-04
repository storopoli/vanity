// test_pipeline.wgsl - Minimal test shader
// Just writes known values to output buffer to verify the pipeline works

struct MatchResult {
    scalar: array<u32, 8>,
    pubkey_x: array<u32, 8>,
    pubkey_y_parity: u32,
    address: array<u32, 19>,
    address_len: u32,
    batch_idx: u32,
    found: u32,
}

struct Config {
    pattern_len: u32,
    address_type: u32,
    network: u32,
    iterations: u32,
    batch_size: u32,
    match_mode: u32,
    _pad0: u32,
    _pad1: u32,
}

@group(0) @binding(0) var<uniform> config: Config;
@group(0) @binding(1) var<storage, read> pattern: array<u32, 19>;
@group(0) @binding(2) var<storage, read> base_scalar: array<u32, 8>;
@group(0) @binding(3) var<storage, read_write> results: array<MatchResult>;
@group(0) @binding(4) var<storage, read_write> result_count: atomic<u32>;

@compute @workgroup_size(64)
fn main(@builtin(global_invocation_id) gid: vec3<u32>) {
    let idx = gid.x;

    // Only first thread writes a result (for testing)
    if (idx == 0u) {
        let result_idx = atomicAdd(&result_count, 1u);
        if (result_idx < 64u) {
            results[result_idx].found = 1u;
            results[result_idx].batch_idx = 42u;

            // Write test scalar (known value)
            results[result_idx].scalar[0] = 0xDEADBEEFu;
            results[result_idx].scalar[1] = 0xCAFEBABEu;
            for (var i: u32 = 2u; i < 8u; i++) {
                results[result_idx].scalar[i] = i;
            }

            // Write test pubkey
            for (var i: u32 = 0u; i < 8u; i++) {
                results[result_idx].pubkey_x[i] = 0x11111111u * (i + 1u);
            }
            results[result_idx].pubkey_y_parity = 0u;

            // Write test address: "bc1qtest" packed as big-endian u32s
            results[result_idx].address[0] = 0x62633171u; // 'b','c','1','q'
            results[result_idx].address[1] = 0x74657374u; // 't','e','s','t'
            for (var i: u32 = 2u; i < 19u; i++) {
                results[result_idx].address[i] = 0u;
            }
            results[result_idx].address_len = 8u;
        }
    }
}
