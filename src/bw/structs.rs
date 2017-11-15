use libc::c_void;

#[repr(C, packed)]
pub struct AiScript {
    pub next: *mut AiScript,
    pub prev: *mut AiScript,
    pub pos: u32,
    pub wait: u32,
    pub player: u32,
    pub area: [u32; 4],
    pub center: [u32; 2],
    pub town: *mut c_void,
    pub flags: u32,
}

#[repr(C, packed)]
pub struct AiRegion {
    pub id: u16,
    pub target_region_id: u16,
    pub dc4: [u8; 0x30],
}

#[repr(C, packed)]
pub struct PlayerAiData {
    pub dc0: [u8; 0x218],
    pub flags: u16,
    pub dc21a: [u8; 0x4],
    pub attack_grouping_region: u16,
    pub dc220: [u8; 0x8],
    pub last_attack_second: u32,
    pub dc22c: [u8; 0x2bc],
}

#[repr(C, packed)]
pub struct Game {
    pub dc0: [u8; 0xe4],
    pub map_width_tiles: u16,
    pub map_height_tiles: u16,
    pub dce8: [u8; 0xe520],
    pub elapsed_seconds: u32,
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_sizes() {
        use std::mem;
        assert_eq!(mem::size_of::<AiScript>(), 0x34);
        assert_eq!(mem::size_of::<AiRegion>(), 0x34);
        assert_eq!(mem::size_of::<PlayerAiData>(), 0x4e8);
        assert_eq!(mem::size_of::<Game>(), 0xe60c);
    }
}
