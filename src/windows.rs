#![allow(non_snake_case)]
#![allow(dead_code)]

//! Provides safe winapi wrappers with nicer string handling

use kernel32;
use user32;
use std::ffi::{CString, OsString, OsStr};
use std::os::windows::ffi::{OsStringExt, OsStrExt};
use std::ptr::null_mut;

use libc::c_void;

use winapi::{HMODULE, FARPROC};

pub fn GetProcAddress(handle: HMODULE, func: &str) -> FARPROC {
    unsafe {
        let name = CString::new(func.as_bytes()).unwrap();
        kernel32::GetProcAddress(handle, name.as_ptr())
    }
}

pub fn LoadLibrary(name: &str) -> HMODULE {
    unsafe { kernel32::LoadLibraryW(winapi_str(name).as_ptr()) }
}

pub fn winapi_str<T: AsRef<OsStr>>(input: T) -> Vec<u16> {
    input.as_ref().encode_wide().chain(Some(0)).collect::<Vec<u16>>()
}

pub fn os_string_from_winapi(input: &[u16]) -> OsString {
    OsString::from_wide(input)
}

pub fn module_from_address(address: *mut c_void) -> Option<(OsString, HMODULE)> {
    unsafe {
        let mut out = null_mut();
        let ok = kernel32::GetModuleHandleExW(4, address as *const _, &mut out);
        if ok == 0 {
            return None;
        }
        defer!({ kernel32::FreeLibrary(out); });
        module_name(out).map(|name| (name, out))
    }
}

pub fn module_name(handle: HMODULE) -> Option<OsString> {
    unsafe {
        let mut buf_size = 128;
        let mut buf = Vec::with_capacity(buf_size);
        loop {
            let result = kernel32::GetModuleFileNameW(handle, buf.as_mut_ptr(), buf_size as u32);
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
        user32::MessageBoxW(
            null_mut(),
            winapi_str(msg).as_ptr(),
            winapi_str(caption).as_ptr(),
            0
        );
    }
}
