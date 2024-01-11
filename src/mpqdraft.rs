use std::slice;

use libc::c_void;
use winapi::shared::windef::HWND;

struct Module;

#[allow(non_snake_case)]
#[repr(C)]
struct Vtable {
    Identify: unsafe extern "stdcall" fn(*mut MpqdraftPlugin, *mut u32) -> u32,
    GetPluginName: unsafe extern "stdcall" fn(*mut MpqdraftPlugin, *mut u8, u32) -> u32,
    CanPatchExecutable: unsafe extern "stdcall" fn(*mut MpqdraftPlugin, *const u8) -> u32,
    Configure: unsafe extern "stdcall" fn(*mut MpqdraftPlugin, HWND) -> u32,
    ReadyForPatch: unsafe extern "stdcall" fn(*mut MpqdraftPlugin) -> u32,
    GetModules: unsafe extern "stdcall" fn(*mut MpqdraftPlugin, *mut Module, *mut u32) -> u32,
    InitializePlugin: unsafe extern "stdcall" fn(*mut MpqdraftPlugin, *mut c_void) -> u32,
    TerminatePlugin: unsafe extern "stdcall" fn(*mut MpqdraftPlugin) -> u32,
}

#[repr(C)]
pub struct MpqdraftPlugin {
    vtable: *const Vtable,
}

static VTABLE: Vtable = Vtable {
    Identify: identify,
    GetPluginName: get_plugin_name,
    CanPatchExecutable: can_patch_executable,
    Configure: configure,
    ReadyForPatch: ready_for_patch,
    GetModules: get_modules,
    InitializePlugin: initialize_plugin,
    TerminatePlugin: terminate_plugin,
};

static mut PLUGIN: MpqdraftPlugin = MpqdraftPlugin {
    vtable: &VTABLE,
};

unsafe extern "stdcall" fn identify(_plugin: *mut MpqdraftPlugin, plugin_id: *mut u32) -> u32 {
    *plugin_id = 0xffdd1135;
    1
}

unsafe extern "stdcall" fn get_plugin_name(
    _plugin: *mut MpqdraftPlugin,
    out: *mut u8,
    out_size: u32,
) -> u32 {
    use std::io::Write;

    let mut out = slice::from_raw_parts_mut(out, out_size as usize);
    let result = write!(
        out,
        "Aiscript extension plugin {}\0",
        env!("CARGO_PKG_VERSION")
    );
    if result.is_err() {
        0
    } else {
        1
    }
}

unsafe extern "stdcall" fn can_patch_executable(
    _plugin: *mut MpqdraftPlugin,
    _exe_name: *const u8,
) -> u32 {
    // TODO: Could check for 1161, but using a checksum may be too strict?
    1
}

unsafe extern "stdcall" fn configure(_plugin: *mut MpqdraftPlugin, _hwnd: HWND) -> u32 {
    1
}

unsafe extern "stdcall" fn ready_for_patch(_plugin: *mut MpqdraftPlugin) -> u32 {
    1
}

unsafe extern "stdcall" fn get_modules(
    _plugin: *mut MpqdraftPlugin,
    _modules: *mut Module,
    count: *mut u32,
) -> u32 {
    *count = 0;
    1
}

unsafe extern "stdcall" fn initialize_plugin(
    _plugin: *mut MpqdraftPlugin,
    _mpqdraft: *mut c_void,
) -> u32 {
    crate::Initialize();
    1
}

unsafe extern "stdcall" fn terminate_plugin(_plugin: *mut MpqdraftPlugin) -> u32 {
    1
}

#[no_mangle]
#[allow(non_snake_case)]
pub extern "stdcall" fn GetMPQDraftPlugin(out: *mut *mut MpqdraftPlugin) -> u32 {
    unsafe {
        *out = std::ptr::addr_of_mut!(PLUGIN);
    }
    1
}
