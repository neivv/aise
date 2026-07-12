use std::ffi::{CStr, OsStr, OsString};
use std::fs::{self, File, DirEntry};
use std::io::{self, Read};
use std::os::windows::io::IntoRawHandle;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use bw_dat::Game;
use directories::UserDirs;

use crate::samase;

pub unsafe fn do_autosave(name_base: &[u8], cycle_size: u32, game: Game) {
    let cycle_size = cycle_size.max(1u32);
    let save_dir = match get_user_sp_save_dir() {
        Some(s) => s,
        None => {
            error!("Couldn't get single player save dir");
            return;
        }
    };
    if !save_dir.exists() {
        // Parent dir should exist at least, so no need for create_dir_recurse
        if let Err(e) = fs::create_dir(&save_dir) {
            error!("Failed to create save dir {}: {}", save_dir.display(), e);
            return;
        }
    }
    let files = match get_save_files_for_prefix(&save_dir, name_base, game) {
        Ok(o) => o,
        Err(e) => {
            error!("Failed to read dir {}: {}", save_dir.display(), e);
            return;
        }
    };
    let mut files: Vec<(Box<fs::DirEntry>, OsString, u32)> = files.into_iter().map(|x| {
        let num = get_autosave_number(&x.1);
        (x.0, x.1, num)
    }).collect();
    if files.is_empty() {
        // No files, create new first save
        // Just `name_base` if just overwrite
        // or `name_base-001` otherwise.
        // Since there are no files, this should always work unless some weird io error happens
        let filename = if cycle_size == 1 {
            OsString::from(String::from(String::from_utf8_lossy(&name_base)))
        } else {
            make_autosave_base_name(name_base, 1)
        };
        let path = save_dir.join(&filename);
        write_save_should_ok(&path, game);
    } else {
        files.sort_unstable_by_key(|x| x.2);

        // Create new numbered save, unless cycle size is 1, in which case just
        // save over old save. Also if cycle size is 1 and save was not autosave,
        // create new numbered save.
        if let Some((entry, filename, num)) = files.last() {
            let mut path = entry.path();
            if cycle_size > 1 || is_user_save(&path) {
                if let Some(next) = num.checked_add(1) {
                    let name = make_autosave_base_name(name_base, next);
                    let path = save_dir.join(&name);
                    write_save_should_ok(&path, game);
                } else {
                    // If user creates autosave-4294967295.snx then guess we just
                    // give up on saving. Not going to try to find a solution for every
                    // weird case.
                    warn!("Skipping save since {} is max number", filename.display());
                }
            } else {
                // Remove extension as write_save_should_ok does not want that
                path.set_extension("");
                write_save_should_ok(&path, game);
            }
        }
        if cycle_size != 1 {
            // Delete nth oldest save
            if let Some(idx) = files.len().checked_sub(cycle_size as usize) &&
                let Some((entry, _, _)) = files.get(idx)
            {
                let path = entry.path();
                if !is_user_save(&path) {
                    if let Err(e) = std::fs::remove_file(&path) {
                        warn!("Failed to remove old autosave {}: {e}", path.display());
                    }
                }
            }
        }
    }
}

/// Determines if the save is not aise autosave, as they have
/// "\r\n  Filename:  <AUTOSAVE>\r\n" at start of the save data
fn is_user_save(path: &Path) -> bool {
    let mut file = match File::open(path) {
        Ok(o) => o,
        Err(e) => {
            error!("is_user_save failed to open file {}: {e}", path.display());
            // Treating as user save to be conservative
            return true;
        }
    };
    // Even 0x100 should be enough since Filename: is first thing after "Starcraft save version xx"
    // header, but doing 0x800 to be sure
    let mut buffer = [0u8; 0x800];
    let header = match read_to_fill_or_end(&mut file, &mut buffer) {
        Ok(o) => o,
        Err(e) => {
            error!("is_user_save failed to read file {}: {e}", path.display());
            return true;
        }
    };
    let search_string = b"\r\n  Filename:  ";
    if let Some(text) = simple_memmem(header, search_string) &&
        let Some(rest) = text.get(search_string.len()..) &&
        rest.starts_with(b"<AUTOSAVE>\r\n")
    {
        return false;
    } else {
        return true;
    }
}

fn simple_memmem<'a>(input: &'a [u8], search: &[u8]) -> Option<&'a [u8]> {
    assert!(search.len() != 0);
    let mut pos = input;
    let first_char = search[0];
    while pos.len() >= search.len() {
        if pos[0] == first_char && pos.starts_with(search) {
            return Some(pos);
        }
        pos = &pos[1..];
    }
    None
}

fn read_to_fill_or_end<'a>(file: &mut File, buffer: &'a mut [u8]) -> io::Result<&'a mut [u8]> {
    let buf_len = buffer.len();
    let mut buf_pos = &mut buffer[..];
    while buf_pos.len() != 0 {
        match file.read(buf_pos) {
            Ok(0) => break,
            Ok(n) => {
                buf_pos = buf_pos.get_mut(n..).unwrap_or(&mut []);
            }
            Err(e) => return Err(e),
        }
    }
    let size = buf_len - buf_pos.len();
    Ok(&mut buffer[..size])
}

/// Caller expects this to work unless an I/O error happens
/// Handles any error if it happens.
///
/// Overwrites old save atomically.
///
/// Path should be without extension, this function adds the correct one.
fn write_save_should_ok(path: &Path, game: Game) {
    if let Err(e) = write_save(path, game) {
        let msg = format!(
            "Failed to autosave {}: {}",
            path.file_name().unwrap_or_else(|| OsStr::new("")).display(),
            e,
        );
        bw_print!("{}", msg);
        error!("{}", msg);
    }
}

/// Path should be without extension, this function adds the correct one.
fn write_save(path: &Path, game: Game) -> io::Result<()> {
    // Note: If aiscript intentionally adds . in the autosave name, this fails
    debug_assert!(path.extension().is_none());
    let tmp_path = path.with_added_extension("tmp");
    let file = File::create(&tmp_path)?;
    // Note: do_save closes the handle
    let handle = file.into_raw_handle();
    let time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH)
        .map(|x| x.as_secs() as u32)
        .unwrap_or(1);
    let game_elapsed_seconds = crate::globals::game_elapsed_seconds_for_save();
    unsafe {
        let ret = samase::do_save(
            handle,
            c"<AUTOSAVE>",
            time,
            c"<AUTOSAVE>",
            0,
            game_elapsed_seconds,
        );
        if ret == 0 {
            let _ = std::fs::remove_file(&tmp_path);
            if samase::do_save_supported() {
                return Err(io::Error::other("BW saving failed"));
            } else {
                // Don't spam user.
                return Ok(());
            }
        }
    }
    let dest_path = path.with_added_extension(bw_current_save_extension(game));
    debug!("Finishing save {} -> {}", tmp_path.display(), dest_path.display());
    if let Err(e) = std::fs::rename(&tmp_path, &dest_path) {
        let _ = std::fs::remove_file(&tmp_path);
        return Err(e);
    }
    Ok(())
}

fn bw_current_save_extension(game: Game) -> &'static str {
    unsafe {
        if samase::is_multiplayer() {
            if (**game).is_bw == 0 {
                "mlt"
            } else {
                "mlx"
            }
        } else {
            if (**game).is_bw == 0 {
                "sng"
            } else {
                "snx"
            }
        }
    }
}

fn make_autosave_base_name(base: &[u8], num: u32) -> OsString {
    use std::io::Write;

    let mut filename_vec = Vec::with_capacity(base.len() + 8);
    filename_vec.extend_from_slice(&base);
    filename_vec.push(b'-');
    // TODO u32::format_into would be nicer next month once it is stable
    let _ = write!(&mut filename_vec, "{:03}", num);
    OsString::from(String::from(String::from_utf8_lossy(&filename_vec)))
}

/// Extracts `6` from "some-save-006.snx"
fn get_autosave_number(filename: &OsStr) -> u32 {
    let filename = filename.as_encoded_bytes();
    let last_dash = match filename.iter().rposition(|&x| x == b'-') {
        Some(s) => s,
        None => return 0,
    };
    if let Some(number_part) = filename.get(last_dash + 1..) &&
        let Some(number_count) = number_part.iter().position(|&x| !x.is_ascii_digit()) &&
        let Some(number_part) = number_part.get(..number_count) &&
        let Ok(number_part) = std::str::from_utf8(number_part) &&
        let Ok(number) = number_part.parse::<u32>()
    {
        number
    } else {
        0
    }
}

/// returns files matching "{prefix}-*.snx"
/// returns boxed DirEntries since they are 500+ bytes on Windows
fn get_save_files_for_prefix(path: &Path, prefix: &[u8], game: Game) ->
    io::Result<Vec<(Box<DirEntry>, OsString)>>
{
    let mut ret = Vec::with_capacity(0x10);
    let save_extension = bw_current_save_extension(game);
    for entry in path.read_dir()? {
        if entry.is_err() {
            return Err(entry.unwrap_err());
        }
        let entry_ref = entry.as_ref().unwrap();
        let is_file = if let Ok(ft) = entry_ref.file_type() && ft.is_file() {
            true
        } else {
            false
        };
        if !is_file {
            continue;
        }
        let name_os_string = entry_ref.file_name();
        let name = name_os_string.as_encoded_bytes();
        let has_prefix = if let Some(p) = name.get(0..prefix.len()) &&
            p.eq_ignore_ascii_case(prefix) &&
            // Accept both "prefix-", and "prefix.snx" for 1-cycle sized saves,
            // but not "prefixunrelated.snx"
            name.get(prefix.len()).is_some_and(|&x| {
                x == b'-' || (x == b'.' && name.len() == prefix.len() + 4)
            })
        {
            true
        } else {
            false
        };
        if !has_prefix {
            continue;
        }
        let extension_ok = if let Some(idx) = name.len().checked_sub(4) &&
            let Some(suffix) = name.get(idx..) &&
            suffix[0] == b'.' &&
            suffix[1..].eq_ignore_ascii_case(save_extension.as_bytes())
        {
            true
        } else {
            false
        };
        if !extension_ok {
            continue;
        }
        ret.push((Box::new(entry?), name_os_string));
    }
    Ok(ret)
}

pub fn get_user_sp_save_dir() -> Option<PathBuf> {
    let user_name = local_player_name()?;
    Some(get_save_dir().join(user_name.to_str().ok()?))
}

fn local_player_name() -> Option<&'static CStr> {
    unsafe {
        let players = samase::players();
        let local_id = samase::local_unique_player_id() as usize;
        if local_id >= 8 {
            // Technically should also support obs slots here but those need some extra handling
            // and obs saves aren't a thing anyway.
            return None;
        }

        let local_player = players.add(local_id);
        CStr::from_bytes_until_nul(&*(&raw const (*local_player).name))
            .ok()
            .filter(|x| !x.is_empty())
    }
}

pub fn get_save_dir() -> PathBuf {
    let root = if crate::is_scr() {
        UserDirs::new()
            .and_then(|user_dirs| user_dirs.document_dir().map(|s| s.join("Starcraft")))
            .unwrap_or_else(|| ".".into())
    } else {
        ".".into()
    };
    root.join("save")
}

#[test]
fn test_get_autosave_number() {
    assert_eq!(get_autosave_number(OsStr::new("some-save-006.snx")), 6);
    assert_eq!(get_autosave_number(OsStr::new("somesave-1234.snx")), 1234);
    assert_eq!(get_autosave_number(OsStr::new("some-save.snx")), 0);
}
