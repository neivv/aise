#![allow(non_snake_case)]
#![allow(dead_code)]

//! Provides safe winapi wrappers with nicer string handling

use std::ffi::{CString, OsStr, OsString};
use std::os::windows::ffi::{OsStrExt, OsStringExt};
use std::ptr::null_mut;

use libc::c_void;
use scopeguard::defer;

use winapi::shared::minwindef::{FARPROC, HMODULE};
use winapi::um::libloaderapi::{
    self, FreeLibrary, GetModuleFileNameW, GetModuleHandleExW, LoadLibraryW,
};
use winapi::um::minwinbase::{SYSTEMTIME};
use winapi::um::sysinfoapi::GetLocalTime;
use winapi::um::winuser::MessageBoxW;

pub fn GetProcAddress(handle: HMODULE, func: &str) -> FARPROC {
    unsafe {
        let name = CString::new(func.as_bytes()).unwrap();
        libloaderapi::GetProcAddress(handle, name.as_ptr())
    }
}

pub fn LoadLibrary(name: &str) -> HMODULE {
    unsafe { LoadLibraryW(winapi_str(name).as_ptr()) }
}

pub fn winapi_str<T: AsRef<OsStr>>(input: T) -> Vec<u16> {
    let mut buf = Vec::with_capacity(input.as_ref().len());
    buf.extend(input.as_ref().encode_wide());
    buf.push(0);
    buf
}

pub fn os_string_from_winapi(input: &[u16]) -> OsString {
    OsString::from_wide(input)
}

pub fn module_from_address(address: *mut c_void) -> Option<(OsString, HMODULE)> {
    unsafe {
        let mut out = null_mut();
        let ok = GetModuleHandleExW(4, address as *const _, &mut out);
        if ok == 0 {
            return None;
        }
        defer!({
            FreeLibrary(out);
        });
        module_name(out).map(|name| (name, out))
    }
}

pub fn module_name(handle: HMODULE) -> Option<OsString> {
    unsafe {
        let mut buf_size = 128;
        let mut buf = Vec::with_capacity(buf_size);
        loop {
            let result = GetModuleFileNameW(handle, buf.as_mut_ptr(), buf_size as u32);
            match result {
                n if n == buf_size as u32 => {
                    // reserve does not guarantee to reserve exactly specified size,
                    // unline with_capacity
                    let reserve_amt = buf.capacity();
                    buf.reserve(reserve_amt);
                    buf_size = buf.capacity();
                }
                0 => {
                    // Error
                    return None;
                }
                n => {
                    let winapi_str = ::std::slice::from_raw_parts(buf.as_ptr(), n as usize);
                    return Some(os_string_from_winapi(winapi_str));
                }
            }
        }
    }
}

pub fn message_box(caption: &str, msg: &str) {
    unsafe {
        MessageBoxW(
            null_mut(),
            winapi_str(msg).as_ptr(),
            winapi_str(caption).as_ptr(),
            0,
        );
    }
}

pub fn get_local_time() -> SYSTEMTIME {
    unsafe {
        let mut out: SYSTEMTIME = std::mem::zeroed();
        GetLocalTime(&mut out);
        out
    }
}
