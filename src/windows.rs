#![allow(non_snake_case)]

//! Provides safe winapi wrappers with nicer string handling

use std::ffi::{OsStr, OsString};
use std::os::windows::ffi::{OsStrExt, OsStringExt};
use std::ptr::null_mut;

use windows_sys::Win32::Foundation::SYSTEMTIME;
use windows_sys::Win32::System::SystemInformation::GetLocalTime;
use windows_sys::Win32::UI::WindowsAndMessaging::MessageBoxW;

pub fn winapi_str<T: AsRef<OsStr>>(input: T) -> Vec<u16> {
    let mut buf = Vec::with_capacity(input.as_ref().len());
    buf.extend(input.as_ref().encode_wide());
    buf.push(0);
    buf
}

#[allow(dead_code)]
pub fn os_string_from_winapi(input: &[u16]) -> OsString {
    OsString::from_wide(input)
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
