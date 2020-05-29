use winapi::{
    shared::{
        dxgi::*, dxgi1_2::*, dxgi1_3::*, dxgi1_5::*, dxgi1_6::*, dxgiformat::*, dxgitype::*,
        winerror::SUCCEEDED,
    },
    um::{d3d12::*, d3d12sdklayers::*, d3dcommon::*, d3dcompiler::*, unknwnbase::*},
};

struct Vertex {
    position: directx_math::XMFLOAT3,
    color: directx_math::XMFLOAT4,
}

const FRAME_COUNT: u32 = 2;

fn main() {
    use raw_window_handle::HasRawWindowHandle;

    let event_loop = winit::event_loop::EventLoop::new();
    let window = winit::window::WindowBuilder::new()
        .with_visible(false)
        .build(&event_loop)
        .unwrap();
    let window_handle = match window.raw_window_handle() {
        raw_window_handle::RawWindowHandle::Windows(windows_handle) => windows_handle,
        _ => panic!("Failed to get the raw-window-handle. D3D12 only exists on Window ;)"),
    };
    let hwnd = window_handle.hwnd as winapi::shared::windef::HWND;

    let factory = create_factory();
    let adapter = get_adapter(factory);
    let device = create_device(adapter);
    let command_queue = create_command_queue(device);
    let (swap_chain, mut frame_index) = create_swap_chain(&window, &hwnd, factory, command_queue);
    let (descriptor_heap, descriptor_heap_size) = create_discriptor_heap(device);
    let render_targets =
        create_render_targets(device, swap_chain, descriptor_heap, descriptor_heap_size);
    let command_allocator = create_command_allocator(device);
    let root_signature = create_root_signature(device);
    let pipeline_state = create_pipeline_state(device, root_signature);
    let command_list = create_command_list(device, command_allocator, pipeline_state);
    unsafe { (&*command_list).Close() };

    let vertices = [
        Vertex {
            position: directx_math::XMFLOAT3 {
                x: 0.0,
                y: 0.5,
                z: 0.0,
            },
            color: directx_math::XMFLOAT4 {
                x: 1.0,
                y: 0.0,
                z: 0.0,
                w: 1.0,
            },
        },
        Vertex {
            position: directx_math::XMFLOAT3 {
                x: 0.5,
                y: -0.5,
                z: 0.0,
            },
            color: directx_math::XMFLOAT4 {
                x: 0.0,
                y: 1.0,
                z: 0.0,
                w: 1.0,
            },
        },
        Vertex {
            position: directx_math::XMFLOAT3 {
                x: -0.5,
                y: -0.5,
                z: 0.0,
            },
            color: directx_math::XMFLOAT4 {
                x: 0.0,
                y: 0.0,
                z: 1.0,
                w: 1.0,
            },
        },
    ];

    let vertices_size = std::mem::size_of_val(&vertices);

    let mut vertex_buffer = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        (&*device).CreateCommittedResource(
            &D3D12_HEAP_PROPERTIES {
                Type: D3D12_HEAP_TYPE_UPLOAD,
                CPUPageProperty: D3D12_CPU_PAGE_PROPERTY_UNKNOWN,
                MemoryPoolPreference: D3D12_MEMORY_POOL_UNKNOWN,
                CreationNodeMask: 1,
                VisibleNodeMask: 1,
            },
            D3D12_HEAP_FLAG_NONE,
            &D3D12_RESOURCE_DESC {
                Dimension: D3D12_RESOURCE_DIMENSION_BUFFER,
                Alignment: 0,
                Width: vertices_size as _,
                Height: 1,
                DepthOrArraySize: 1,
                MipLevels: 1,
                Format: DXGI_FORMAT_UNKNOWN,
                SampleDesc: DXGI_SAMPLE_DESC {
                    Count: 1,
                    Quality: 0,
                },
                Layout: D3D12_TEXTURE_LAYOUT_ROW_MAJOR,
                Flags: D3D12_RESOURCE_FLAG_NONE,
            },
            D3D12_RESOURCE_STATE_GENERIC_READ,
            std::ptr::null(),
            &IID_ID3D12Resource,
            &mut vertex_buffer,
        )
    }) {
        panic!("CreateCommittedResource")
    }

    let vertex_buffer = vertex_buffer as *mut ID3D12Resource;

    let mut vertex_data_begin = std::ptr::null_mut();
    let read_range = D3D12_RANGE { Begin: 0, End: 0 };

    if !SUCCEEDED(unsafe { (&*vertex_buffer).Map(0, &read_range, &mut vertex_data_begin) }) {
        panic!("Map");
    }

    unsafe { libc::memcpy(vertex_data_begin, vertices.as_ptr() as _, vertices_size) };

    unsafe { (&*vertex_buffer).Unmap(0, std::ptr::null()) };

    let vertex_buffer_view = D3D12_VERTEX_BUFFER_VIEW {
        BufferLocation: unsafe { (&*vertex_buffer).GetGPUVirtualAddress() },
        StrideInBytes: std::mem::size_of::<Vertex>() as _,
        SizeInBytes: vertices_size as _,
    };

    let mut fence = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        (&*device).CreateFence(0, D3D12_FENCE_FLAG_NONE, &IID_ID3D12Fence, &mut fence)
    }) {
        panic!("CreateFence");
    }

    let fence = fence as *mut ID3D12Fence;

    let mut fence_value = 0;

    let fence_event =
        unsafe { winapi::um::synchapi::CreateEventW(std::ptr::null_mut(), 0, 0, std::ptr::null()) };

    if fence_event.eq(&std::ptr::null_mut()) {
        panic!("CreateEventW");
    }

    wait_for_prev_frame(
        swap_chain,
        command_queue,
        &mut frame_index,
        fence,
        fence_event,
        &mut fence_value,
    );

    window.set_visible(true);

    event_loop.run(move |event, _, control_flow| {
        *control_flow = winit::event_loop::ControlFlow::Poll;
        match event {
            winit::event::Event::WindowEvent {
                window_id: _,
                event,
            } => match event {
                winit::event::WindowEvent::CloseRequested => {
                    *control_flow = winit::event_loop::ControlFlow::Exit
                }
                _ => {}
            },
            winit::event::Event::RedrawRequested(window_id) => {
                populate_command_list(
                    &window,
                    command_allocator,
                    command_list,
                    pipeline_state,
                    root_signature,
                    &render_targets,
                    &(frame_index as _),
                    descriptor_heap,
                    &(descriptor_heap_size as _),
                    &vertex_buffer_view,
                );
                let command_lists = [command_list as *mut ID3D12CommandList];
                unsafe {
                    (&*command_queue)
                        .ExecuteCommandLists(command_lists.len() as _, command_lists.as_ptr())
                };
                if !SUCCEEDED(unsafe { (&*swap_chain).Present(1, 0) }) {
                    panic!("Present");
                }
                wait_for_prev_frame(
                    swap_chain,
                    command_queue,
                    &mut frame_index,
                    fence,
                    fence_event,
                    &mut fence_value,
                );
            }
            winit::event::Event::LoopDestroyed => {
                wait_for_prev_frame(
                    swap_chain,
                    command_queue,
                    &mut frame_index,
                    fence,
                    fence_event,
                    &mut fence_value,
                );
                unsafe { winapi::um::handleapi::CloseHandle(fence_event) };
            }
            _ => {}
        }
    });
}

fn wait_for_prev_frame(
    swapchain: *mut IDXGISwapChain4,
    command_queue: *mut ID3D12CommandQueue,
    frame_index: &mut u32,
    fence: *mut ID3D12Fence,
    fence_event: winapi::um::winnt::HANDLE,
    fence_value: &mut u64,
) {
    let prev_fence_value = fence_value.clone();
    if !SUCCEEDED(unsafe { (&*command_queue).Signal(fence, prev_fence_value) }) {
        panic!("Signal");
    }
    *fence_value += 1;

    if unsafe { (&*fence).GetCompletedValue() } < prev_fence_value {
        if !SUCCEEDED(unsafe { (&*fence).SetEventOnCompletion(prev_fence_value, fence_event) }) {
            panic!("SetEventOnCompletion");
        }
        unsafe {
            winapi::um::synchapi::WaitForSingleObject(fence_event, winapi::um::winbase::INFINITE)
        };
    }

    *frame_index = unsafe { (&*swapchain).GetCurrentBackBufferIndex() };
}

fn populate_command_list(
    window: &winit::window::Window,
    command_allocator: *mut ID3D12CommandAllocator,
    command_list: *mut ID3D12GraphicsCommandList,
    pipeline_state: *mut ID3D12PipelineState,
    root_signature: *mut ID3D12RootSignature,
    render_targets: &[*mut ID3D12Resource; FRAME_COUNT as _],
    frame_index: &usize,
    descriptor_heap: *mut ID3D12DescriptorHeap,
    descriptor_heap_size: &usize,
    vertex_buffer_view: &D3D12_VERTEX_BUFFER_VIEW,
) {
    if !SUCCEEDED(unsafe { (&*command_allocator).Reset() }) {
        panic!("Reset");
    }
    if !SUCCEEDED(unsafe { (&*command_list).Reset(command_allocator, pipeline_state) }) {
        panic!("Reset");
    }
    unsafe { (&*command_list).SetGraphicsRootSignature(root_signature) };
    unsafe {
        (&*command_list).RSSetViewports(
            1,
            &D3D12_VIEWPORT {
                TopLeftX: 0.0,
                TopLeftY: 0.0,
                Height: window.inner_size().height as _,
                Width: window.inner_size().width as _,
                MaxDepth: D3D12_MIN_DEPTH,
                MinDepth: D3D12_MAX_DEPTH,
            },
        )
    };
    unsafe {
        (&*command_list).RSSetScissorRects(
            1,
            &D3D12_RECT {
                left: 0,
                top: 0,
                bottom: window.inner_size().height as _,
                right: window.inner_size().width as _,
            },
        )
    };
    unsafe {
        (&*command_list).ResourceBarrier(
            1,
            &D3D12_RESOURCE_BARRIER {
                Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
                Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
                u: {
                    let mut resource_barrier_transition =
                        std::mem::zeroed::<D3D12_RESOURCE_BARRIER_u>();
                    *resource_barrier_transition.Transition_mut() =
                        D3D12_RESOURCE_TRANSITION_BARRIER {
                            pResource: render_targets[*frame_index],
                            StateBefore: D3D12_RESOURCE_STATE_PRESENT,
                            StateAfter: D3D12_RESOURCE_STATE_RENDER_TARGET,
                            Subresource: D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES,
                        };
                    resource_barrier_transition
                },
            },
        )
    };
    let descriptor_handle = D3D12_CPU_DESCRIPTOR_HANDLE {
        ptr: unsafe { (&*descriptor_heap).GetCPUDescriptorHandleForHeapStart() }.ptr
            + frame_index * descriptor_heap_size,
    };
    unsafe { (&*command_list).OMSetRenderTargets(1, &descriptor_handle, 0, std::ptr::null()) };

    let clear_color = [0.0, 0.2, 0.4, 1.0];
    unsafe {
        (&*command_list).ClearRenderTargetView(descriptor_handle, &clear_color, 0, std::ptr::null())
    };
    unsafe { (&*command_list).IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST) };
    unsafe { (&*command_list).IASetVertexBuffers(0, 1, vertex_buffer_view) };
    unsafe { (&*command_list).DrawInstanced(3, 1, 0, 0) };
    unsafe {
        (&*command_list).ResourceBarrier(
            1,
            &D3D12_RESOURCE_BARRIER {
                Type: D3D12_RESOURCE_BARRIER_TYPE_TRANSITION,
                Flags: D3D12_RESOURCE_BARRIER_FLAG_NONE,
                u: {
                    let mut resource_barrier_transition =
                        std::mem::zeroed::<D3D12_RESOURCE_BARRIER_u>();
                    *resource_barrier_transition.Transition_mut() =
                        D3D12_RESOURCE_TRANSITION_BARRIER {
                            pResource: render_targets[*frame_index],
                            StateBefore: D3D12_RESOURCE_STATE_RENDER_TARGET,
                            StateAfter: D3D12_RESOURCE_STATE_PRESENT,
                            Subresource: D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES,
                        };
                    resource_barrier_transition
                },
            },
        )
    };
    if !SUCCEEDED(unsafe { (&*command_list).Close() }) {
        panic!("Close");
    }
}

fn create_factory() -> *mut IDXGIFactory6 {
    let mut factory_flags = 0u32;
    #[cfg(debug_assertions)]
    {
        let mut debug_interface = std::ptr::null_mut();
        if SUCCEEDED(unsafe { D3D12GetDebugInterface(&IID_ID3D12Debug, &mut debug_interface) }) {
            let debug_interface = debug_interface as *mut ID3D12Debug;
            unsafe { (&*debug_interface).EnableDebugLayer() };
            factory_flags |= DXGI_CREATE_FACTORY_DEBUG;
        }
    }

    let mut factory = std::ptr::null_mut();

    if !SUCCEEDED(unsafe { CreateDXGIFactory2(factory_flags, &IID_IDXGIFactory6, &mut factory) }) {
        panic!("winapi::shared::dxgi1_3::CreateDXGIFactory2");
    }

    factory as _
}

fn get_adapter<'t>(factory: *mut IDXGIFactory6) -> *mut IDXGIAdapter4 {
    let mut adapter = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        (&*factory).EnumAdapterByGpuPreference(
            0,
            DXGI_GPU_PREFERENCE_HIGH_PERFORMANCE,
            &IID_IDXGIAdapter4,
            &mut adapter,
        )
    }) {
        panic!("EnumAdapterByGpuPreference");
    }

    let adapter = adapter as *mut IDXGIAdapter4;

    #[cfg(debug_assertions)]
    {
        let mut adapter_desc = unsafe { std::mem::zeroed::<DXGI_ADAPTER_DESC3>() };
        if !SUCCEEDED(unsafe { (&*adapter).GetDesc3(&mut adapter_desc) }) {
            panic!("GetDesc3")
        };
        println!("{}", String::from_utf16(&adapter_desc.Description).unwrap());
    }
    adapter
}

fn create_device(adapter: *mut IDXGIAdapter4) -> *mut ID3D12Device2 {
    let mut device = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        D3D12CreateDevice(
            adapter as *mut IUnknown,
            D3D_FEATURE_LEVEL_12_1,
            &IID_ID3D12Device2,
            &mut device,
        )
    }) {
        panic!("D3D12CreateDevice");
    }

    device as _
}

fn create_command_queue(device: *mut ID3D12Device2) -> *mut ID3D12CommandQueue {
    let mut command_queue = std::ptr::null_mut();

    let command_queue_desc = D3D12_COMMAND_QUEUE_DESC {
        Flags: D3D12_COMMAND_QUEUE_FLAG_NONE,
        Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
        ..Default::default()
    };

    if !SUCCEEDED(unsafe {
        (&*device).CreateCommandQueue(
            &command_queue_desc,
            &IID_ID3D12CommandQueue,
            &mut command_queue,
        )
    }) {
        panic!("CreateCommandQueue");
    }

    command_queue as _
}

fn create_swap_chain(
    window: &winit::window::Window,
    hwnd: &winapi::shared::windef::HWND,
    factory: *mut IDXGIFactory6,
    command_queue: *mut ID3D12CommandQueue,
) -> (*mut IDXGISwapChain4, u32) {
    let mut swap_chain = std::ptr::null_mut();

    let swap_chain_desc = DXGI_SWAP_CHAIN_DESC1 {
        Width: window.inner_size().width,
        Height: window.inner_size().height,
        Format: DXGI_FORMAT_R8G8B8A8_UNORM,
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            ..Default::default()
        },
        BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
        BufferCount: FRAME_COUNT,
        SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
        ..Default::default()
    };

    if !SUCCEEDED(unsafe {
        (&*factory).CreateSwapChainForHwnd(
            command_queue as *mut IUnknown,
            *hwnd,
            &swap_chain_desc,
            std::ptr::null(),
            std::ptr::null_mut(),
            &mut swap_chain,
        )
    }) {
        panic!("CreateSwapChainForHwnd");
    }

    if !SUCCEEDED(unsafe {
        (&*factory).MakeWindowAssociation(*hwnd, /* DXGI_MWA_NO_ALT_ENTER */ 1 << 1)
    }) {
        panic!("MakeWindowAssociation");
    }

    let swap_chain = unsafe { &*swap_chain };
    let mut swap_chain_4 = std::ptr::null_mut();
    if !SUCCEEDED(unsafe { swap_chain.QueryInterface(&IID_IDXGISwapChain4, &mut swap_chain_4) }) {
        panic!("QueryInterface");
    }
    let swap_chain_4 = swap_chain_4 as *mut IDXGISwapChain4;

    (swap_chain_4, unsafe {
        (&*swap_chain_4).GetCurrentBackBufferIndex()
    })
}

fn create_discriptor_heap(device: *mut ID3D12Device2) -> (*mut ID3D12DescriptorHeap, u32) {
    let descriptor_heap_desc = D3D12_DESCRIPTOR_HEAP_DESC {
        NumDescriptors: FRAME_COUNT,
        Type: D3D12_DESCRIPTOR_HEAP_TYPE_RTV,
        Flags: D3D12_DESCRIPTOR_HEAP_FLAG_NONE,
        ..Default::default()
    };

    let mut descriptor_heap = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        (&*device).CreateDescriptorHeap(
            &descriptor_heap_desc,
            &IID_ID3D12DescriptorHeap,
            &mut descriptor_heap,
        )
    }) {
        panic!("CreateDescriptorHeap");
    }

    let descriptor_heap = descriptor_heap as *mut ID3D12DescriptorHeap;

    let descriptor_heap_size =
        unsafe { (&*device).GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_RTV) };

    (descriptor_heap as _, descriptor_heap_size)
}

fn create_render_targets(
    device: *mut ID3D12Device2,
    swap_chain: *mut IDXGISwapChain4,
    descriptor_heap: *mut ID3D12DescriptorHeap,
    descriptor_size: u32,
) -> [*mut ID3D12Resource; FRAME_COUNT as usize] {
    let mut render_targets: [*mut ID3D12Resource; FRAME_COUNT as usize] =
        [std::ptr::null_mut(); FRAME_COUNT as usize];
    let mut descriptor_handle = unsafe { (&*descriptor_heap).GetCPUDescriptorHandleForHeapStart() };
    for i in 0..FRAME_COUNT {
        let mut render_target = std::ptr::null_mut();
        if !SUCCEEDED(unsafe {
            (&*swap_chain).GetBuffer(i, &IID_ID3D12Resource, &mut render_target)
        }) {
            panic!("GetBuffer");
        }
        unsafe {
            (&*device).CreateRenderTargetView(
                render_target as _,
                std::ptr::null(),
                descriptor_handle,
            );
        }
        descriptor_handle.ptr += descriptor_size as usize;
        render_targets[i as usize] = render_target as _;
    }
    render_targets
}

fn create_command_allocator(device: *mut ID3D12Device2) -> *mut ID3D12CommandAllocator {
    let mut command_allocator = std::ptr::null_mut();

    let result = unsafe {
        (&*device).CreateCommandAllocator(
            D3D12_COMMAND_LIST_TYPE_DIRECT,
            &IID_ID3D12CommandAllocator,
            &mut command_allocator,
        )
    };

    if !SUCCEEDED(result) {
        panic!("CreateCommandAllocator");
    }
    command_allocator as _
}

fn create_root_signature(device: *mut ID3D12Device2) -> *mut ID3D12RootSignature {
    let root_signature_desc = D3D12_ROOT_SIGNATURE_DESC {
        NumParameters: 0,
        pParameters: std::ptr::null(),
        NumStaticSamplers: 0,
        pStaticSamplers: std::ptr::null(),
        Flags: D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT,
    };

    let mut signature = std::ptr::null_mut();
    let mut error = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        D3D12SerializeRootSignature(
            &root_signature_desc,
            D3D_ROOT_SIGNATURE_VERSION_1,
            &mut signature,
            &mut error,
        )
    }) {
        panic!("D3D12SerializeRootSignature");
    }

    let mut root_signature = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        (&*device).CreateRootSignature(
            0,
            (&*signature).GetBufferPointer(),
            (&*signature).GetBufferSize(),
            &IID_ID3D12RootSignature,
            &mut root_signature,
        )
    }) {
        panic!("CreateRootSignature");
    }

    root_signature as _
}

fn create_pipeline_state(
    device: *mut ID3D12Device2,
    root_signature: *mut ID3D12RootSignature,
) -> *mut ID3D12PipelineState {
    #[cfg(debug_assertions)]
    let compiler_flags = D3DCOMPILE_DEBUG | D3DCOMPILE_SKIP_OPTIMIZATION;
    #[cfg(not(debug_assertions))]
    let compiler_flags = 0u32;

    //let shaders = include_bytes!("../shaders.hlsl");

    let shader_name = String::from("shaders\\shaders.hlsl")
        .encode_utf16()
        .collect::<Vec<_>>();

    let vertex_shader_entry_point_name = b"VSMain\0";
    let vertex_shader_target_name = b"vs_5_0\0";
    let mut vertex_shader = std::ptr::null_mut();
    let mut vertex_shader_error = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        D3DCompileFromFile(
            shader_name.as_ptr() as _,
            std::ptr::null(),
            std::ptr::null_mut(),
            vertex_shader_entry_point_name.as_ptr() as _,
            vertex_shader_target_name.as_ptr() as _,
            compiler_flags,
            0,
            &mut vertex_shader,
            &mut vertex_shader_error,
        )
    }) {
        if !vertex_shader_error.eq(&std::ptr::null_mut()) {
            let error = unsafe {
                String::from_raw_parts(
                    (&*vertex_shader_error).GetBufferPointer() as _,
                    (&*vertex_shader_error).GetBufferSize() as _,
                    (&*vertex_shader_error).GetBufferSize() as _,
                )
            };
            println!("{}", error);
        }
        panic!("D3DCompileFromFile");
    }

    let pixel_shader_entry_point_name = b"PSMain\0";
    let pixel_shader_target_name = b"ps_5_0\0";
    let mut pixel_shader = std::ptr::null_mut();
    let mut pixel_shader_error = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        D3DCompileFromFile(
            shader_name.as_ptr() as _,
            std::ptr::null(),
            std::ptr::null_mut(),
            pixel_shader_entry_point_name.as_ptr() as _,
            pixel_shader_target_name.as_ptr() as _,
            compiler_flags,
            0,
            &mut pixel_shader,
            &mut pixel_shader_error,
        )
    }) {
        if !pixel_shader_error.eq(&std::ptr::null_mut()) {
            let error = unsafe {
                String::from_raw_parts(
                    (&*pixel_shader_error).GetBufferPointer() as _,
                    (&*pixel_shader_error).GetBufferSize() as _,
                    (&*pixel_shader_error).GetBufferSize() as _,
                )
            };
            println!("{}", error);
        }
        panic!("D3DCompileFromFile");
    }

    let position = b"POSITION\0";
    let color = b"COLOR\0";

    let input_element_descs = vec![
        D3D12_INPUT_ELEMENT_DESC {
            SemanticName: position.as_ptr() as _,
            SemanticIndex: 0,
            Format: DXGI_FORMAT_R32G32B32_FLOAT,
            InputSlot: 0,
            AlignedByteOffset: 0,
            InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
            InstanceDataStepRate: 0,
        },
        D3D12_INPUT_ELEMENT_DESC {
            SemanticName: color.as_ptr() as _,
            SemanticIndex: 0,
            Format: DXGI_FORMAT_R32G32B32A32_FLOAT,
            InputSlot: 0,
            AlignedByteOffset: 12,
            InputSlotClass: D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
            InstanceDataStepRate: 0,
        },
    ];

    let graphics_pipeline_state_desc = D3D12_GRAPHICS_PIPELINE_STATE_DESC {
        pRootSignature: root_signature,
        VS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: unsafe { (&*vertex_shader).GetBufferPointer() },
            BytecodeLength: unsafe { (&*vertex_shader).GetBufferSize() },
        },
        PS: D3D12_SHADER_BYTECODE {
            pShaderBytecode: unsafe { (&*pixel_shader).GetBufferPointer() },
            BytecodeLength: unsafe { (&*pixel_shader).GetBufferSize() },
        },
        BlendState: D3D12_BLEND_DESC {
            AlphaToCoverageEnable: 0,
            IndependentBlendEnable: 0,
            RenderTarget: [D3D12_RENDER_TARGET_BLEND_DESC {
                BlendEnable: 0,
                LogicOpEnable: 0,
                SrcBlend: D3D12_BLEND_ONE,
                DestBlend: D3D12_BLEND_ZERO,
                BlendOp: D3D12_BLEND_OP_ADD,
                SrcBlendAlpha: D3D12_BLEND_ONE,
                DestBlendAlpha: D3D12_BLEND_ZERO,
                BlendOpAlpha: D3D12_BLEND_OP_ADD,
                LogicOp: D3D12_LOGIC_OP_NOOP,
                RenderTargetWriteMask: D3D12_COLOR_WRITE_ENABLE_ALL as _,
            }; 8],
        },
        SampleMask: std::u32::MAX,
        RasterizerState: D3D12_RASTERIZER_DESC {
            FillMode: D3D12_FILL_MODE_SOLID,
            CullMode: D3D12_CULL_MODE_BACK,
            FrontCounterClockwise: 0,
            DepthBias: D3D12_DEFAULT_DEPTH_BIAS as _,
            DepthBiasClamp: D3D12_DEFAULT_DEPTH_BIAS_CLAMP,
            SlopeScaledDepthBias: D3D12_DEFAULT_SLOPE_SCALED_DEPTH_BIAS,
            DepthClipEnable: 1,
            MultisampleEnable: 0,
            AntialiasedLineEnable: 0,
            ForcedSampleCount: 0,
            ConservativeRaster: D3D12_CONSERVATIVE_RASTERIZATION_MODE_OFF,
        },
        DepthStencilState: D3D12_DEPTH_STENCIL_DESC {
            DepthEnable: 0,
            StencilEnable: 0,
            ..Default::default()
        },
        InputLayout: D3D12_INPUT_LAYOUT_DESC {
            pInputElementDescs: input_element_descs.as_ptr(),
            NumElements: input_element_descs.len() as _,
        },
        PrimitiveTopologyType: D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE,
        NumRenderTargets: 1,
        RTVFormats: {
            let mut rtv_formats = [0; 8];
            rtv_formats[0] = DXGI_FORMAT_R8G8B8A8_UNORM;
            rtv_formats
        },
        SampleDesc: DXGI_SAMPLE_DESC {
            Count: 1,
            ..Default::default()
        },
        ..Default::default()
    };

    let mut graphics_pipeline_state = std::ptr::null_mut();

    if !SUCCEEDED(unsafe {
        (&*device).CreateGraphicsPipelineState(
            &graphics_pipeline_state_desc,
            &IID_ID3D12PipelineState,
            &mut graphics_pipeline_state,
        )
    }) {
        panic!("CreateGraphicsPipelineState");
    }

    graphics_pipeline_state as _
}

fn create_command_list(
    device: *mut ID3D12Device2,
    command_allocator: *mut ID3D12CommandAllocator,
    pipeline_state: *mut ID3D12PipelineState,
) -> *mut ID3D12GraphicsCommandList {
    let mut command_list = std::ptr::null_mut();
    if !SUCCEEDED(unsafe {
        (&*device).CreateCommandList(
            0,
            D3D12_COMMAND_LIST_TYPE_DIRECT,
            command_allocator,
            pipeline_state,
            &IID_ID3D12GraphicsCommandList,
            &mut command_list,
        )
    }) {
        panic!("CreateCommandList");
    }
    command_list as _
}
