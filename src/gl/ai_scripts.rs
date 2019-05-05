use std::mem;

use bw_dat::{TechId, UnitId, UpgradeId};

use crate::bw;
use crate::list::ListEntry;

use super::support::UiList;
use super::ui::Page;
use super::{tech_name, unit_name, upgrade_name, UiInput};

pub struct AiScripts {
    scripts: UiList<*mut bw::AiScript>,
}

// Move to main code if it needs
impl ListEntry for bw::AiScript {
    unsafe fn next(x: *mut Self) -> *mut *mut Self {
        &mut (*x).next
    }

    unsafe fn prev(x: *mut Self) -> *mut *mut Self {
        &mut (*x).prev
    }
}

impl AiScripts {
    pub fn new() -> AiScripts {
        AiScripts {
            scripts: UiList::new(),
        }
    }

    pub fn draw_page(&mut self, page: &mut Page) {
        page.clear();
        self.scripts.update();
        let script = match self.scripts.current() {
            Some(&s) => s,
            None => return,
        };
        let town_array = bw::town_array();
        unsafe {
            let town = (*script).town;
            let town_id = match town.is_null() {
                true => 0,
                false => (town as usize - town_array as usize) / mem::size_of::<bw::AiTown>(),
            };
            page.push(format!(
                "Script #{}/{}: {:p} player {:x} town id {:x} ({:p}), pos ({}, {})",
                self.scripts.current_pos(),
                self.scripts.len(),
                script,
                (*script).player,
                town_id,
                town,
                (*script).center.x,
                (*script).center.y,
            ));
            page.push(format!("Pos {:x} wait {:x}", (*script).pos, (*script).wait));
            page.push("Next commands:");
            {
                let mut indent = page.indent(4);
                let page = indent.page();
                for cmd in parse_script_commands(script).take(10) {
                    page.push(cmd.format());
                }
            }
        }
    }

    pub fn input(&mut self, value: char) -> UiInput {
        match value {
            'q' => {
                self.scripts.page_back(1);
                UiInput::Handled
            }
            'w' => {
                self.scripts.page_forward(1);
                UiInput::Handled
            }
            _ => UiInput::NotHandled,
        }
    }
}

unsafe fn parse_script_commands(script: *mut bw::AiScript) -> impl Iterator<Item = Command> {
    // Use a local copy since ScriptData parsing advances the position
    ParseScript {
        script: *script,
        ok: true,
    }
}

struct ParseScript {
    script: bw::AiScript,
    ok: bool,
}

struct Pos(u32);
struct Priority(u8);

#[allow(bad_style)]
enum Command {
    Unknown(u8),
    Goto(Pos),
    NoTowns_Jump(UnitId, Pos),
    Wait(u16),
    Start_Town,
    Start_AreaTown,
    Expand(u8, Pos),
    Build(u8, UnitId, Priority),
    Upgrade(u8, UpgradeId, Priority),
    Tech(TechId, Priority),
    Wait_Build(u8, UnitId),
    Wait_BuildStart(u8, UnitId),
    Attack_Clear,
    Attack_Add(u8, UnitId),
    Attack_Prepare,
    Attack_Do,
    Wait_Secure,
    Capt_Expand,
    Build_Bunkers,
    Wait_Bunkers,
    Random_Jump(u8, Pos),
    Call(Pos),
    Return,
    Stop,
    Wait_Force(u8, UnitId),
    Do_Morph(u8, UnitId),
    Train(u8, UnitId),
    Wait_Train(u8, UnitId),
    MultiRun(Pos),
    Start_Campaign,
}

impl Iterator for ParseScript {
    type Item = Command;
    fn next(&mut self) -> Option<Self::Item> {
        use self::Command::*;

        if !self.ok {
            return None;
        }
        let mut parse = unsafe { crate::aiscript::ScriptData::new(&mut self.script) };
        let cmd = match parse.read_u8() {
            0x00 => {
                self.ok = false;
                Goto(Pos(parse.read_jump_pos()))
            }
            0x01 => NoTowns_Jump(UnitId(parse.read_u16()), Pos(parse.read_u16().into())),
            0x02 => Wait(parse.read_u16()),
            0x03 => Start_Town,
            0x04 => Start_AreaTown,
            0x05 => Expand(parse.read_u8(), Pos(parse.read_u16().into())),
            0x06 => Build(
                parse.read_u8(),
                UnitId(parse.read_u16()),
                Priority(parse.read_u8()),
            ),
            0x07 => Upgrade(
                parse.read_u8(),
                UpgradeId(parse.read_u16()),
                Priority(parse.read_u8()),
            ),
            0x08 => Tech(TechId(parse.read_u16()), Priority(parse.read_u8())),
            0x09 => Wait_Build(parse.read_u8(), UnitId(parse.read_u16())),
            0x0a => Wait_BuildStart(parse.read_u8(), UnitId(parse.read_u16())),
            0x0b => Attack_Clear,
            0x0c => Attack_Add(parse.read_u8(), UnitId(parse.read_u16())),
            0x0d => Attack_Prepare,
            0x0e => Attack_Do,
            0x0f => Wait_Secure,
            0x10 => Capt_Expand,
            0x11 => Build_Bunkers,
            0x12 => Wait_Bunkers,
            0x24 => {
                self.ok = false;
                Stop
            }
            0x30 => Random_Jump(parse.read_u8(), Pos(parse.read_u16().into())),
            0x38 => Start_Campaign,
            0x3e => Wait_Force(parse.read_u8(), UnitId(parse.read_u16())),
            0x40 => Call(Pos(parse.read_u16().into())),
            0x41 => {
                self.ok = false;
                Return
            }
            0x46 => Do_Morph(parse.read_u8(), UnitId(parse.read_u16())),
            0x48 => MultiRun(Pos(parse.read_u16().into())),
            0x4c => Train(parse.read_u8(), UnitId(parse.read_u16())),
            0x4e => Wait_Train(parse.read_u8(), UnitId(parse.read_u16())),
            unk => {
                self.ok = false;
                Unknown(unk)
            }
        };
        Some(cmd)
    }
}

trait AiDebugFormat {
    fn fmt(&self) -> String;
}

impl AiDebugFormat for Pos {
    fn fmt(&self) -> String {
        format!("{:04x}", self.0)
    }
}

impl AiDebugFormat for Priority {
    fn fmt(&self) -> String {
        format!("{}", self.0)
    }
}

impl AiDebugFormat for UnitId {
    fn fmt(&self) -> String {
        format!("{}", unit_name(*self))
    }
}

impl AiDebugFormat for TechId {
    fn fmt(&self) -> String {
        format!("{}", tech_name(*self))
    }
}

impl AiDebugFormat for UpgradeId {
    fn fmt(&self) -> String {
        format!("{}", upgrade_name(*self))
    }
}

impl Command {
    fn format(&self) -> String {
        use std::fmt::Write;

        use self::Command::*;

        let mut out = String::new();
        match self {
            Unknown(x) => write!(out, "Unknown / unimplemented {:02x}", x),
            Goto(pos) => write!(out, "goto {}", pos.fmt()),
            NoTowns_Jump(unit, pos) => write!(out, "notowns_jump {} {}", unit.fmt(), pos.fmt()),
            Wait(time) => write!(out, "wait {}", time),
            Start_Town => write!(out, "start_town"),
            Start_AreaTown => write!(out, "start_areatown"),
            Expand(num, pos) => write!(out, "expand {} {}", num, pos.fmt()),
            Build(amt, unit, prio) => write!(out, "build {} {} {}", amt, unit.fmt(), prio.fmt()),
            Upgrade(level, upgrade, prio) => {
                write!(out, "upgrade {} {} {}", level, upgrade.fmt(), prio.fmt())
            }
            Tech(tech, prio) => write!(out, "tech {} {}", tech.fmt(), prio.fmt()),
            Wait_Build(amt, unit) => write!(out, "wait_build {} {}", amt, unit.fmt()),
            Wait_BuildStart(amt, unit) => write!(out, "wait_buildstart {} {}", amt, unit.fmt()),
            Attack_Clear => write!(out, "attack_clear"),
            Attack_Add(amt, unit) => write!(out, "attack_add {} {}", amt, unit.fmt()),
            Attack_Prepare => write!(out, "attack_prepare"),
            Attack_Do => write!(out, "attack_do"),
            Wait_Secure => write!(out, "wait_secure"),
            Capt_Expand => write!(out, "cap_expand"),
            Build_Bunkers => write!(out, "build_bunkers"),
            Wait_Bunkers => write!(out, "wait_bunkers"),
            Call(pos) => write!(out, "call {}", pos.fmt()),
            Random_Jump(chance, pos) => write!(out, "random_jump {} {}", chance, pos.fmt()),
            Return => write!(out, "return"),
            Stop => write!(out, "stop"),
            Start_Campaign => write!(out, "start_campaign"),
            Wait_Force(amt, unit) => write!(out, "wait_force {} {}", amt, unit.fmt()),
            Do_Morph(amt, unit) => write!(out, "do_morph {} {}", amt, unit.fmt()),
            Train(amt, unit) => write!(out, "train {} {}", amt, unit.fmt()),
            Wait_Train(amt, unit) => write!(out, "wait_train {} {}", amt, unit.fmt()),
            MultiRun(pos) => write!(out, "multirun {}", pos.fmt()),
        }
        .unwrap();
        out
    }
}
