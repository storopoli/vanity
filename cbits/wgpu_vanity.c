/**
 * wgpu_vanity.c - WGPU/Metal backend implementation
 *
 * Uses wgpu-native for cross-platform GPU compute.
 * Updated for wgpu-native 27.x API.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "wgpu_vanity.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

/* Check if wgpu-native header is available */
#if __has_include(<webgpu.h>)
#include <webgpu.h>
#define WGPU_AVAILABLE 1
#else
#define WGPU_AVAILABLE 0
#endif

/* Global state */
static struct {
#if WGPU_AVAILABLE
    WGPUInstance instance;
    WGPUAdapter adapter;
    WGPUDevice device;
    WGPUQueue queue;
    WGPUShaderModule shader;
    WGPUComputePipeline pipeline;
    WGPUBindGroupLayout bind_group_layout;
    WGPUBuffer config_buffer;
    WGPUBuffer pattern_buffer;
    WGPUBuffer scalar_buffer;
    WGPUBuffer results_buffer;
    WGPUBuffer result_count_buffer;
    WGPUBuffer results_staging;      /* For readback */
    WGPUBuffer count_staging;        /* For readback */
    volatile int adapter_ready;
    volatile int device_ready;
    volatile int work_done;
    volatile int map_done;
#endif
    int initialized;
    int pipeline_created;
    WGPUVanityError last_error;
    uint64_t total_attempts;
    char device_name[256];
    VanityWGPUResult results[64];
    uint32_t result_count;
} g_wgpu = {0};

#if WGPU_AVAILABLE

/* Helper to create WGPUStringView from C string */
static WGPUStringView make_string_view(const char* str) {
    WGPUStringView sv;
    sv.data = str;
    sv.length = str ? strlen(str) : 0;
    return sv;
}

/* Callback for adapter request (new API) */
static void adapter_callback(WGPURequestAdapterStatus status,
                            WGPUAdapter adapter,
                            WGPUStringView message,
                            void* userdata1,
                            void* userdata2) {
    (void)message;
    (void)userdata1;
    (void)userdata2;
    if (status == WGPURequestAdapterStatus_Success) {
        g_wgpu.adapter = adapter;
    }
    g_wgpu.adapter_ready = 1;
}

/* Callback for device request (new API) */
static void device_callback(WGPURequestDeviceStatus status,
                           WGPUDevice device,
                           WGPUStringView message,
                           void* userdata1,
                           void* userdata2) {
    (void)message;
    (void)userdata1;
    (void)userdata2;
    if (status == WGPURequestDeviceStatus_Success) {
        g_wgpu.device = device;
    }
    g_wgpu.device_ready = 1;
}

/* Callback for queue work done (new API) */
static void work_done_callback(WGPUQueueWorkDoneStatus status,
                              void* userdata1,
                              void* userdata2) {
    (void)status;
    (void)userdata1;
    (void)userdata2;
    g_wgpu.work_done = 1;
}

/* Callback for buffer map (new API) */
static void map_callback(WGPUMapAsyncStatus status,
                        WGPUStringView message,
                        void* userdata1,
                        void* userdata2) {
    (void)status;
    (void)message;
    (void)userdata1;
    (void)userdata2;
    g_wgpu.map_done = 1;
}

#endif /* WGPU_AVAILABLE */

int vanity_wgpu_init(void) {
#if !WGPU_AVAILABLE
    g_wgpu.last_error = WGPU_ERROR_NO_ADAPTER;
    return WGPU_ERROR_NO_ADAPTER;
#else
    if (g_wgpu.initialized) {
        return WGPU_ERROR_NONE;
    }

    /* Create instance */
    WGPUInstanceDescriptor instance_desc = {0};
    g_wgpu.instance = wgpuCreateInstance(&instance_desc);
    if (!g_wgpu.instance) {
        g_wgpu.last_error = WGPU_ERROR_NO_ADAPTER;
        return WGPU_ERROR_NO_ADAPTER;
    }

    /* Request adapter using new callback API */
    WGPURequestAdapterOptions adapter_opts = {0};
    adapter_opts.powerPreference = WGPUPowerPreference_HighPerformance;

    WGPURequestAdapterCallbackInfo callback_info = {0};
    callback_info.callback = adapter_callback;
    callback_info.mode = WGPUCallbackMode_AllowProcessEvents;

    g_wgpu.adapter_ready = 0;
    wgpuInstanceRequestAdapter(g_wgpu.instance, &adapter_opts, callback_info);

    /* Poll until adapter is ready */
    while (!g_wgpu.adapter_ready) {
        wgpuInstanceProcessEvents(g_wgpu.instance);
    }

    if (!g_wgpu.adapter) {
        g_wgpu.last_error = WGPU_ERROR_NO_ADAPTER;
        return WGPU_ERROR_NO_ADAPTER;
    }

    /* Get adapter info using new API */
    WGPUAdapterInfo info = {0};
    if (wgpuAdapterGetInfo(g_wgpu.adapter, &info) == WGPUStatus_Success) {
        if (info.device.data && info.device.length > 0) {
            size_t len = info.device.length;
            if (len >= sizeof(g_wgpu.device_name)) {
                len = sizeof(g_wgpu.device_name) - 1;
            }
            memcpy(g_wgpu.device_name, info.device.data, len);
            g_wgpu.device_name[len] = '\0';
        } else {
            strcpy(g_wgpu.device_name, "Unknown GPU");
        }
        wgpuAdapterInfoFreeMembers(info);
    } else {
        strcpy(g_wgpu.device_name, "Unknown GPU");
    }

    /* Request device using new callback API */
    WGPUDeviceDescriptor device_desc = {0};
    device_desc.label = make_string_view("vanity_device");

    WGPURequestDeviceCallbackInfo device_callback_info = {0};
    device_callback_info.callback = device_callback;
    device_callback_info.mode = WGPUCallbackMode_AllowProcessEvents;

    g_wgpu.device_ready = 0;
    wgpuAdapterRequestDevice(g_wgpu.adapter, &device_desc, device_callback_info);

    while (!g_wgpu.device_ready) {
        wgpuInstanceProcessEvents(g_wgpu.instance);
    }

    if (!g_wgpu.device) {
        g_wgpu.last_error = WGPU_ERROR_NO_DEVICE;
        return WGPU_ERROR_NO_DEVICE;
    }

    /* Get queue */
    g_wgpu.queue = wgpuDeviceGetQueue(g_wgpu.device);

    g_wgpu.initialized = 1;
    g_wgpu.last_error = WGPU_ERROR_NONE;
    return WGPU_ERROR_NONE;
#endif /* WGPU_AVAILABLE */
}

void vanity_wgpu_cleanup(void) {
#if WGPU_AVAILABLE
    if (!g_wgpu.initialized) return;

    vanity_wgpu_destroy_pipeline();

    if (g_wgpu.queue) {
        wgpuQueueRelease(g_wgpu.queue);
        g_wgpu.queue = NULL;
    }
    if (g_wgpu.device) {
        wgpuDeviceRelease(g_wgpu.device);
        g_wgpu.device = NULL;
    }
    if (g_wgpu.adapter) {
        wgpuAdapterRelease(g_wgpu.adapter);
        g_wgpu.adapter = NULL;
    }
    if (g_wgpu.instance) {
        wgpuInstanceRelease(g_wgpu.instance);
        g_wgpu.instance = NULL;
    }

    g_wgpu.initialized = 0;
#endif
}

int vanity_wgpu_get_device_name(char* name_buf, size_t buf_size) {
    if (!g_wgpu.initialized) {
        return WGPU_ERROR_NO_DEVICE;
    }
    strncpy(name_buf, g_wgpu.device_name, buf_size - 1);
    name_buf[buf_size - 1] = '\0';
    return WGPU_ERROR_NONE;
}

int vanity_wgpu_get_device_info(WGPUDeviceInfo* info) {
#if !WGPU_AVAILABLE
    return WGPU_ERROR_NO_ADAPTER;
#else
    if (!g_wgpu.initialized || !info) {
        return WGPU_ERROR_NO_DEVICE;
    }

    strncpy(info->name, g_wgpu.device_name, sizeof(info->name) - 1);

#ifdef __APPLE__
    strcpy(info->backend, "Metal");
#elif defined(_WIN32)
    strcpy(info->backend, "D3D12");
#else
    strcpy(info->backend, "Vulkan");
#endif

    info->max_workgroup_size = 256;
    info->max_compute_units = 64;
    info->memory_bytes = 0;

    return WGPU_ERROR_NONE;
#endif
}

int vanity_wgpu_create_pipeline(const char* shader_code, size_t code_len) {
#if !WGPU_AVAILABLE
    (void)shader_code;
    (void)code_len;
    return WGPU_ERROR_NO_ADAPTER;
#else
    if (!g_wgpu.initialized) {
        return WGPU_ERROR_NO_DEVICE;
    }

    if (g_wgpu.pipeline_created) {
        vanity_wgpu_destroy_pipeline();
    }

    /* Create shader module using new API */
    WGPUShaderSourceWGSL wgsl_source = {0};
    wgsl_source.chain.sType = WGPUSType_ShaderSourceWGSL;
    wgsl_source.code.data = shader_code;
    wgsl_source.code.length = code_len;

    WGPUShaderModuleDescriptor shader_desc = {0};
    shader_desc.nextInChain = (WGPUChainedStruct*)&wgsl_source;
    shader_desc.label = make_string_view("vanity_shader");

    g_wgpu.shader = wgpuDeviceCreateShaderModule(g_wgpu.device, &shader_desc);
    if (!g_wgpu.shader) {
        g_wgpu.last_error = WGPU_ERROR_SHADER_COMPILE;
        return WGPU_ERROR_SHADER_COMPILE;
    }

    /* Create bind group layout */
    WGPUBindGroupLayoutEntry layout_entries[5] = {0};

    /* @binding(0): config uniform */
    layout_entries[0].binding = 0;
    layout_entries[0].visibility = WGPUShaderStage_Compute;
    layout_entries[0].buffer.type = WGPUBufferBindingType_Uniform;
    layout_entries[0].buffer.minBindingSize = sizeof(VanityWGPUConfig);

    /* @binding(1): pattern storage */
    layout_entries[1].binding = 1;
    layout_entries[1].visibility = WGPUShaderStage_Compute;
    layout_entries[1].buffer.type = WGPUBufferBindingType_ReadOnlyStorage;
    layout_entries[1].buffer.minBindingSize = 76;

    /* @binding(2): base_scalar storage */
    layout_entries[2].binding = 2;
    layout_entries[2].visibility = WGPUShaderStage_Compute;
    layout_entries[2].buffer.type = WGPUBufferBindingType_ReadOnlyStorage;
    layout_entries[2].buffer.minBindingSize = 32;

    /* @binding(3): results storage */
    layout_entries[3].binding = 3;
    layout_entries[3].visibility = WGPUShaderStage_Compute;
    layout_entries[3].buffer.type = WGPUBufferBindingType_Storage;
    layout_entries[3].buffer.minBindingSize = sizeof(VanityWGPUResult) * 64;

    /* @binding(4): result_count atomic */
    layout_entries[4].binding = 4;
    layout_entries[4].visibility = WGPUShaderStage_Compute;
    layout_entries[4].buffer.type = WGPUBufferBindingType_Storage;
    layout_entries[4].buffer.minBindingSize = 4;

    WGPUBindGroupLayoutDescriptor layout_desc = {0};
    layout_desc.label = make_string_view("vanity_bind_group_layout");
    layout_desc.entryCount = 5;
    layout_desc.entries = layout_entries;

    g_wgpu.bind_group_layout = wgpuDeviceCreateBindGroupLayout(
        g_wgpu.device, &layout_desc);
    if (!g_wgpu.bind_group_layout) {
        g_wgpu.last_error = WGPU_ERROR_PIPELINE_CREATE;
        return WGPU_ERROR_PIPELINE_CREATE;
    }

    /* Create pipeline layout */
    WGPUPipelineLayoutDescriptor pipeline_layout_desc = {0};
    pipeline_layout_desc.label = make_string_view("vanity_pipeline_layout");
    pipeline_layout_desc.bindGroupLayoutCount = 1;
    pipeline_layout_desc.bindGroupLayouts = &g_wgpu.bind_group_layout;

    WGPUPipelineLayout pipeline_layout = wgpuDeviceCreatePipelineLayout(
        g_wgpu.device, &pipeline_layout_desc);

    /* Create compute pipeline */
    WGPUComputePipelineDescriptor pipeline_desc = {0};
    pipeline_desc.label = make_string_view("vanity_pipeline");
    pipeline_desc.layout = pipeline_layout;
    pipeline_desc.compute.module = g_wgpu.shader;
    pipeline_desc.compute.entryPoint = make_string_view("main");

    g_wgpu.pipeline = wgpuDeviceCreateComputePipeline(
        g_wgpu.device, &pipeline_desc);

    wgpuPipelineLayoutRelease(pipeline_layout);

    if (!g_wgpu.pipeline) {
        g_wgpu.last_error = WGPU_ERROR_PIPELINE_CREATE;
        return WGPU_ERROR_PIPELINE_CREATE;
    }

    /* Create buffers */
    WGPUBufferDescriptor buf_desc = {0};

    /* Config buffer */
    buf_desc.label = make_string_view("config_buffer");
    buf_desc.size = sizeof(VanityWGPUConfig);
    buf_desc.usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst;
    g_wgpu.config_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Pattern buffer */
    buf_desc.label = make_string_view("pattern_buffer");
    buf_desc.size = 76;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst;
    g_wgpu.pattern_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Scalar buffer */
    buf_desc.label = make_string_view("scalar_buffer");
    buf_desc.size = 32;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst;
    g_wgpu.scalar_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Results buffer */
    buf_desc.label = make_string_view("results_buffer");
    buf_desc.size = sizeof(VanityWGPUResult) * 64;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopySrc;
    g_wgpu.results_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Result count buffer */
    buf_desc.label = make_string_view("result_count_buffer");
    buf_desc.size = 4;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst |
                     WGPUBufferUsage_CopySrc;
    g_wgpu.result_count_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Staging buffer for results readback */
    buf_desc.label = make_string_view("results_staging");
    buf_desc.size = sizeof(VanityWGPUResult) * 64;
    buf_desc.usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst;
    g_wgpu.results_staging = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Staging buffer for count readback */
    buf_desc.label = make_string_view("count_staging");
    buf_desc.size = 4;
    buf_desc.usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst;
    g_wgpu.count_staging = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    if (!g_wgpu.config_buffer || !g_wgpu.pattern_buffer ||
        !g_wgpu.scalar_buffer || !g_wgpu.results_buffer ||
        !g_wgpu.result_count_buffer || !g_wgpu.results_staging ||
        !g_wgpu.count_staging) {
        g_wgpu.last_error = WGPU_ERROR_BUFFER_CREATE;
        return WGPU_ERROR_BUFFER_CREATE;
    }

    g_wgpu.pipeline_created = 1;
    g_wgpu.last_error = WGPU_ERROR_NONE;
    return WGPU_ERROR_NONE;
#endif /* WGPU_AVAILABLE */
}

void vanity_wgpu_destroy_pipeline(void) {
#if WGPU_AVAILABLE
    if (!g_wgpu.pipeline_created) return;

    if (g_wgpu.count_staging) {
        wgpuBufferRelease(g_wgpu.count_staging);
        g_wgpu.count_staging = NULL;
    }
    if (g_wgpu.results_staging) {
        wgpuBufferRelease(g_wgpu.results_staging);
        g_wgpu.results_staging = NULL;
    }
    if (g_wgpu.result_count_buffer) {
        wgpuBufferRelease(g_wgpu.result_count_buffer);
        g_wgpu.result_count_buffer = NULL;
    }
    if (g_wgpu.results_buffer) {
        wgpuBufferRelease(g_wgpu.results_buffer);
        g_wgpu.results_buffer = NULL;
    }
    if (g_wgpu.scalar_buffer) {
        wgpuBufferRelease(g_wgpu.scalar_buffer);
        g_wgpu.scalar_buffer = NULL;
    }
    if (g_wgpu.pattern_buffer) {
        wgpuBufferRelease(g_wgpu.pattern_buffer);
        g_wgpu.pattern_buffer = NULL;
    }
    if (g_wgpu.config_buffer) {
        wgpuBufferRelease(g_wgpu.config_buffer);
        g_wgpu.config_buffer = NULL;
    }
    if (g_wgpu.pipeline) {
        wgpuComputePipelineRelease(g_wgpu.pipeline);
        g_wgpu.pipeline = NULL;
    }
    if (g_wgpu.bind_group_layout) {
        wgpuBindGroupLayoutRelease(g_wgpu.bind_group_layout);
        g_wgpu.bind_group_layout = NULL;
    }
    if (g_wgpu.shader) {
        wgpuShaderModuleRelease(g_wgpu.shader);
        g_wgpu.shader = NULL;
    }

    g_wgpu.pipeline_created = 0;
#endif
}

int vanity_wgpu_launch(const VanityWGPUConfig* config,
                       const uint8_t* pattern,
                       const uint32_t* base_scalar) {
#if !WGPU_AVAILABLE
    (void)config;
    (void)pattern;
    (void)base_scalar;
    return WGPU_ERROR_NO_ADAPTER;
#else
    if (!g_wgpu.initialized || !g_wgpu.pipeline_created) {
        return WGPU_ERROR_NO_DEVICE;
    }

    /* Write data to buffers */
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.config_buffer, 0,
                         config, sizeof(VanityWGPUConfig));

    /* Pack pattern bytes as big-endian u32s to match shader's address packing */
    uint32_t pattern_packed[19] = {0};
    for (size_t i = 0; i < 76 && pattern[i]; i++) {
        size_t word_idx = i / 4;
        size_t byte_pos = 3 - (i % 4);  /* Big-endian: first byte in MSB */
        pattern_packed[word_idx] |= ((uint32_t)pattern[i]) << (byte_pos * 8);
    }
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.pattern_buffer, 0,
                         pattern_packed, 76);
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.scalar_buffer, 0,
                         base_scalar, 32);

    /* Reset result count */
    uint32_t zero = 0;
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.result_count_buffer, 0,
                         &zero, sizeof(zero));

    /* Create bind group */
    WGPUBindGroupEntry bind_entries[5] = {0};
    bind_entries[0].binding = 0;
    bind_entries[0].buffer = g_wgpu.config_buffer;
    bind_entries[0].size = sizeof(VanityWGPUConfig);

    bind_entries[1].binding = 1;
    bind_entries[1].buffer = g_wgpu.pattern_buffer;
    bind_entries[1].size = 76;

    bind_entries[2].binding = 2;
    bind_entries[2].buffer = g_wgpu.scalar_buffer;
    bind_entries[2].size = 32;

    bind_entries[3].binding = 3;
    bind_entries[3].buffer = g_wgpu.results_buffer;
    bind_entries[3].size = sizeof(VanityWGPUResult) * 64;

    bind_entries[4].binding = 4;
    bind_entries[4].buffer = g_wgpu.result_count_buffer;
    bind_entries[4].size = 4;

    WGPUBindGroupDescriptor bind_group_desc = {0};
    bind_group_desc.label = make_string_view("vanity_bind_group");
    bind_group_desc.layout = g_wgpu.bind_group_layout;
    bind_group_desc.entryCount = 5;
    bind_group_desc.entries = bind_entries;

    WGPUBindGroup bind_group = wgpuDeviceCreateBindGroup(
        g_wgpu.device, &bind_group_desc);

    /* Create command encoder */
    WGPUCommandEncoderDescriptor enc_desc = {0};
    enc_desc.label = make_string_view("vanity_encoder");
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(
        g_wgpu.device, &enc_desc);

    /* Begin compute pass */
    WGPUComputePassDescriptor pass_desc = {0};
    pass_desc.label = make_string_view("vanity_pass");
    WGPUComputePassEncoder pass = wgpuCommandEncoderBeginComputePass(
        encoder, &pass_desc);

    wgpuComputePassEncoderSetPipeline(pass, g_wgpu.pipeline);
    wgpuComputePassEncoderSetBindGroup(pass, 0, bind_group, 0, NULL);

    /* Dispatch workgroups (64 threads per workgroup) */
    uint32_t workgroup_count = (config->batch_size + 63) / 64;
    wgpuComputePassEncoderDispatchWorkgroups(pass, workgroup_count, 1, 1);

    wgpuComputePassEncoderEnd(pass);

    /* Copy results to staging buffers */
    wgpuCommandEncoderCopyBufferToBuffer(encoder,
        g_wgpu.result_count_buffer, 0,
        g_wgpu.count_staging, 0, 4);
    wgpuCommandEncoderCopyBufferToBuffer(encoder,
        g_wgpu.results_buffer, 0,
        g_wgpu.results_staging, 0, sizeof(VanityWGPUResult) * 64);

    /* Submit */
    WGPUCommandBufferDescriptor cmd_desc = {0};
    WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &cmd_desc);
    wgpuQueueSubmit(g_wgpu.queue, 1, &commands);

    /* Wait for completion using ProcessEvents loop */
    g_wgpu.work_done = 0;
    WGPUQueueWorkDoneCallbackInfo done_info = {0};
    done_info.callback = work_done_callback;
    done_info.mode = WGPUCallbackMode_AllowProcessEvents;
    wgpuQueueOnSubmittedWorkDone(g_wgpu.queue, done_info);

    while (!g_wgpu.work_done) {
        wgpuInstanceProcessEvents(g_wgpu.instance);
    }

    /* Map count staging buffer and read */
    g_wgpu.map_done = 0;
    WGPUBufferMapCallbackInfo map_info = {0};
    map_info.callback = map_callback;
    map_info.mode = WGPUCallbackMode_AllowProcessEvents;
    wgpuBufferMapAsync(g_wgpu.count_staging, WGPUMapMode_Read, 0, 4, map_info);

    while (!g_wgpu.map_done) {
        wgpuInstanceProcessEvents(g_wgpu.instance);
    }

    const uint32_t* count_ptr = (const uint32_t*)wgpuBufferGetConstMappedRange(
        g_wgpu.count_staging, 0, 4);
    if (count_ptr) {
        g_wgpu.result_count = *count_ptr;
    } else {
        g_wgpu.result_count = 0;
    }
    wgpuBufferUnmap(g_wgpu.count_staging);

    /* If there are results, map and read them */
    if (g_wgpu.result_count > 0) {
        uint32_t count = g_wgpu.result_count;
        if (count > 64) count = 64;

        g_wgpu.map_done = 0;
        wgpuBufferMapAsync(g_wgpu.results_staging, WGPUMapMode_Read, 0,
                          sizeof(VanityWGPUResult) * 64, map_info);

        while (!g_wgpu.map_done) {
            wgpuInstanceProcessEvents(g_wgpu.instance);
        }

        const void* data = wgpuBufferGetConstMappedRange(
            g_wgpu.results_staging, 0, sizeof(VanityWGPUResult) * count);
        if (data) {
            memcpy(g_wgpu.results, data, sizeof(VanityWGPUResult) * count);
        }
        wgpuBufferUnmap(g_wgpu.results_staging);
    }

    /* Update attempt count */
    g_wgpu.total_attempts += config->batch_size;

    /* Cleanup */
    wgpuCommandBufferRelease(commands);
    wgpuComputePassEncoderRelease(pass);
    wgpuCommandEncoderRelease(encoder);
    wgpuBindGroupRelease(bind_group);

    g_wgpu.last_error = WGPU_ERROR_NONE;
    return WGPU_ERROR_NONE;
#endif /* WGPU_AVAILABLE */
}

uint32_t vanity_wgpu_get_results(VanityWGPUResult* results, uint32_t max_results) {
#if !WGPU_AVAILABLE
    (void)results;
    (void)max_results;
    return 0;
#else
    if (!g_wgpu.initialized || !g_wgpu.pipeline_created || !results) {
        return 0;
    }

    /* For now, return cached results */
    uint32_t count = g_wgpu.result_count;
    if (count > max_results) count = max_results;
    if (count > 64) count = 64;

    memcpy(results, g_wgpu.results, count * sizeof(VanityWGPUResult));
    return count;
#endif
}

uint64_t vanity_wgpu_get_attempt_count(void) {
    return g_wgpu.total_attempts;
}

int vanity_wgpu_get_last_error(void) {
    return g_wgpu.last_error;
}

int vanity_wgpu_is_available(void) {
#if WGPU_AVAILABLE
    return 1;
#else
    return 0;
#endif
}
