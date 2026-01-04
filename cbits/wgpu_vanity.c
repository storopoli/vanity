/**
 * wgpu_vanity.c - WGPU/Metal backend implementation
 *
 * Uses wgpu-native for cross-platform GPU compute.
 *
 * Copyright (c) 2025 Jose Storopoli
 * MIT License
 */

#include "wgpu_vanity.h"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef __APPLE__
#define WGPU_BACKEND_TYPE WGPUBackendType_Metal
#elif defined(_WIN32)
#define WGPU_BACKEND_TYPE WGPUBackendType_D3D12
#else
#define WGPU_BACKEND_TYPE WGPUBackendType_Vulkan
#endif

/* Check if wgpu-native is available */
#if __has_include(<wgpu.h>)
#include <wgpu.h>
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
#endif
    int initialized;
    int pipeline_created;
    WGPUVanityError last_error;
    uint64_t total_attempts;
    char device_name[256];
    VanityWGPUResult results[64];  /* Max 64 results per batch */
    uint32_t result_count;
} g_wgpu = {0};

#if WGPU_AVAILABLE

/* Callback for adapter request */
static void adapter_callback(WGPURequestAdapterStatus status,
                            WGPUAdapter adapter,
                            const char* message,
                            void* userdata) {
    (void)message;
    int* done = (int*)userdata;
    if (status == WGPURequestAdapterStatus_Success) {
        g_wgpu.adapter = adapter;
    }
    *done = 1;
}

/* Callback for device request */
static void device_callback(WGPURequestDeviceStatus status,
                           WGPUDevice device,
                           const char* message,
                           void* userdata) {
    (void)message;
    int* done = (int*)userdata;
    if (status == WGPURequestDeviceStatus_Success) {
        g_wgpu.device = device;
    }
    *done = 1;
}

/* Error callback */
static void device_error_callback(WGPUErrorType type,
                                  const char* message,
                                  void* userdata) {
    (void)userdata;
    fprintf(stderr, "WGPU Device Error [%d]: %s\n", type, message);
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

    /* Request adapter */
    WGPURequestAdapterOptions adapter_opts = {
        .powerPreference = WGPUPowerPreference_HighPerformance,
        .backendType = WGPU_BACKEND_TYPE,
    };

    int done = 0;
    wgpuInstanceRequestAdapter(g_wgpu.instance, &adapter_opts,
                               adapter_callback, &done);

    /* Poll until adapter is ready */
    while (!done) {
        wgpuInstanceProcessEvents(g_wgpu.instance);
    }

    if (!g_wgpu.adapter) {
        g_wgpu.last_error = WGPU_ERROR_NO_ADAPTER;
        return WGPU_ERROR_NO_ADAPTER;
    }

    /* Get adapter info for device name */
    WGPUAdapterProperties props = {0};
    wgpuAdapterGetProperties(g_wgpu.adapter, &props);
    if (props.name) {
        strncpy(g_wgpu.device_name, props.name, sizeof(g_wgpu.device_name) - 1);
    } else {
        strcpy(g_wgpu.device_name, "Unknown GPU");
    }

    /* Request device */
    WGPUDeviceDescriptor device_desc = {
        .label = "vanity_device",
        .requiredFeatureCount = 0,
        .requiredLimits = NULL,
    };

    done = 0;
    wgpuAdapterRequestDevice(g_wgpu.adapter, &device_desc,
                             device_callback, &done);

    while (!done) {
        wgpuInstanceProcessEvents(g_wgpu.instance);
    }

    if (!g_wgpu.device) {
        g_wgpu.last_error = WGPU_ERROR_NO_DEVICE;
        return WGPU_ERROR_NO_DEVICE;
    }

    /* Set error callback */
    wgpuDeviceSetUncapturedErrorCallback(g_wgpu.device,
                                         device_error_callback, NULL);

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

    /* Get device limits */
    WGPUSupportedLimits limits = {0};
    wgpuDeviceGetLimits(g_wgpu.device, &limits);

    info->max_workgroup_size = limits.limits.maxComputeWorkgroupSizeX;
    info->max_compute_units = limits.limits.maxComputeInvocationsPerWorkgroup;
    info->memory_bytes = 0;  /* Not directly available from WGPU */

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

    /* Create shader module */
    WGPUShaderModuleWGSLDescriptor wgsl_desc = {
        .chain = {
            .sType = WGPUSType_ShaderModuleWGSLDescriptor,
        },
        .code = shader_code,
    };

    WGPUShaderModuleDescriptor shader_desc = {
        .nextInChain = (WGPUChainedStruct*)&wgsl_desc,
        .label = "vanity_shader",
    };

    g_wgpu.shader = wgpuDeviceCreateShaderModule(g_wgpu.device, &shader_desc);
    if (!g_wgpu.shader) {
        g_wgpu.last_error = WGPU_ERROR_SHADER_COMPILE;
        return WGPU_ERROR_SHADER_COMPILE;
    }

    /* Create bind group layout */
    WGPUBindGroupLayoutEntry layout_entries[] = {
        /* @binding(0): config uniform */
        {
            .binding = 0,
            .visibility = WGPUShaderStage_Compute,
            .buffer = {
                .type = WGPUBufferBindingType_Uniform,
                .minBindingSize = sizeof(VanityWGPUConfig),
            },
        },
        /* @binding(1): pattern storage */
        {
            .binding = 1,
            .visibility = WGPUShaderStage_Compute,
            .buffer = {
                .type = WGPUBufferBindingType_ReadOnlyStorage,
                .minBindingSize = 76,
            },
        },
        /* @binding(2): base_scalar storage */
        {
            .binding = 2,
            .visibility = WGPUShaderStage_Compute,
            .buffer = {
                .type = WGPUBufferBindingType_ReadOnlyStorage,
                .minBindingSize = 32,
            },
        },
        /* @binding(3): results storage */
        {
            .binding = 3,
            .visibility = WGPUShaderStage_Compute,
            .buffer = {
                .type = WGPUBufferBindingType_Storage,
                .minBindingSize = sizeof(VanityWGPUResult) * 64,
            },
        },
        /* @binding(4): result_count atomic */
        {
            .binding = 4,
            .visibility = WGPUShaderStage_Compute,
            .buffer = {
                .type = WGPUBufferBindingType_Storage,
                .minBindingSize = 4,
            },
        },
    };

    WGPUBindGroupLayoutDescriptor layout_desc = {
        .label = "vanity_bind_group_layout",
        .entryCount = 5,
        .entries = layout_entries,
    };

    g_wgpu.bind_group_layout = wgpuDeviceCreateBindGroupLayout(
        g_wgpu.device, &layout_desc);
    if (!g_wgpu.bind_group_layout) {
        g_wgpu.last_error = WGPU_ERROR_PIPELINE_CREATE;
        return WGPU_ERROR_PIPELINE_CREATE;
    }

    /* Create pipeline layout */
    WGPUPipelineLayoutDescriptor pipeline_layout_desc = {
        .label = "vanity_pipeline_layout",
        .bindGroupLayoutCount = 1,
        .bindGroupLayouts = &g_wgpu.bind_group_layout,
    };

    WGPUPipelineLayout pipeline_layout = wgpuDeviceCreatePipelineLayout(
        g_wgpu.device, &pipeline_layout_desc);

    /* Create compute pipeline */
    WGPUComputePipelineDescriptor pipeline_desc = {
        .label = "vanity_pipeline",
        .layout = pipeline_layout,
        .compute = {
            .module = g_wgpu.shader,
            .entryPoint = "main",
        },
    };

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
    buf_desc.label = "config_buffer";
    buf_desc.size = sizeof(VanityWGPUConfig);
    buf_desc.usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst;
    g_wgpu.config_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Pattern buffer */
    buf_desc.label = "pattern_buffer";
    buf_desc.size = 76;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst;
    g_wgpu.pattern_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Scalar buffer */
    buf_desc.label = "scalar_buffer";
    buf_desc.size = 32;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst;
    g_wgpu.scalar_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Results buffer */
    buf_desc.label = "results_buffer";
    buf_desc.size = sizeof(VanityWGPUResult) * 64;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopySrc;
    g_wgpu.results_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    /* Result count buffer */
    buf_desc.label = "result_count_buffer";
    buf_desc.size = 4;
    buf_desc.usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst |
                     WGPUBufferUsage_CopySrc;
    g_wgpu.result_count_buffer = wgpuDeviceCreateBuffer(g_wgpu.device, &buf_desc);

    if (!g_wgpu.config_buffer || !g_wgpu.pattern_buffer ||
        !g_wgpu.scalar_buffer || !g_wgpu.results_buffer ||
        !g_wgpu.result_count_buffer) {
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
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.pattern_buffer, 0,
                         pattern, 76);
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.scalar_buffer, 0,
                         base_scalar, 32);

    /* Reset result count */
    uint32_t zero = 0;
    wgpuQueueWriteBuffer(g_wgpu.queue, g_wgpu.result_count_buffer, 0,
                         &zero, sizeof(zero));

    /* Create bind group */
    WGPUBindGroupEntry bind_entries[] = {
        {.binding = 0, .buffer = g_wgpu.config_buffer,
         .size = sizeof(VanityWGPUConfig)},
        {.binding = 1, .buffer = g_wgpu.pattern_buffer, .size = 76},
        {.binding = 2, .buffer = g_wgpu.scalar_buffer, .size = 32},
        {.binding = 3, .buffer = g_wgpu.results_buffer,
         .size = sizeof(VanityWGPUResult) * 64},
        {.binding = 4, .buffer = g_wgpu.result_count_buffer, .size = 4},
    };

    WGPUBindGroupDescriptor bind_group_desc = {
        .label = "vanity_bind_group",
        .layout = g_wgpu.bind_group_layout,
        .entryCount = 5,
        .entries = bind_entries,
    };

    WGPUBindGroup bind_group = wgpuDeviceCreateBindGroup(
        g_wgpu.device, &bind_group_desc);

    /* Create command encoder */
    WGPUCommandEncoderDescriptor enc_desc = {
        .label = "vanity_encoder",
    };
    WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(
        g_wgpu.device, &enc_desc);

    /* Begin compute pass */
    WGPUComputePassDescriptor pass_desc = {
        .label = "vanity_pass",
    };
    WGPUComputePassEncoder pass = wgpuCommandEncoderBeginComputePass(
        encoder, &pass_desc);

    wgpuComputePassEncoderSetPipeline(pass, g_wgpu.pipeline);
    wgpuComputePassEncoderSetBindGroup(pass, 0, bind_group, 0, NULL);

    /* Dispatch workgroups (64 threads per workgroup) */
    uint32_t workgroup_count = (config->batch_size + 63) / 64;
    wgpuComputePassEncoderDispatchWorkgroups(pass, workgroup_count, 1, 1);

    wgpuComputePassEncoderEnd(pass);

    /* Submit */
    WGPUCommandBufferDescriptor cmd_desc = {0};
    WGPUCommandBuffer commands = wgpuCommandEncoderFinish(encoder, &cmd_desc);
    wgpuQueueSubmit(g_wgpu.queue, 1, &commands);

    /* Wait for completion */
    wgpuDevicePoll(g_wgpu.device, true, 0);

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

    /* Read result count */
    /* Note: In production, use proper buffer mapping/staging */
    /* This is a simplified synchronous read */

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
