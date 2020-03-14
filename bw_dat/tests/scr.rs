use std::mem;

mod bw {
    pub use bw_dat::structs::*;
}

use bw_dat::dialog::{Control, Event, EventHandler};

fn set_scr() {
    bw_dat::set_is_scr(true);
}

#[test]
fn dialog_hooking() {
    unsafe {
        static HANDLER_HOOK: EventHandler = EventHandler::new();

        unsafe extern "C" fn event_handler(
            _: *mut bw::scr::Control,
            evt: *mut bw::scr::ControlEvent,
        ) -> u32 {
            let event = Event::new(evt as *mut bw::ControlEvent);
            event.mouse_pos().0 as u32
        }

        unsafe extern "C" fn hook(
            ctrl: *mut bw::Control,
            evt: *mut bw::ControlEvent,
            orig: unsafe extern "C" fn(*mut bw::Control, *mut bw::ControlEvent) -> u32,
        ) -> u32 {
            let event = Event::new(evt);
            let add = event.mouse_pos().1 as u32;
            orig(ctrl, evt) * 2 + add
        }

        set_scr();
        // Test that setting the hook twice won't change anything, won't
        // add a second layer, etc.
        for _ in 0..2 {
            let mut control = bw::scr::Control {
                event_handler: Some(event_handler),
                ..mem::zeroed()
            };
            let control = &mut control as *mut bw::scr::Control;

            let mut event = bw::scr::ControlEvent {
                x: 5,
                y: 12,
                ..mem::zeroed()
            };
            let result = ((*control).event_handler.unwrap())(control, &mut event);
            assert_eq!(result, 5);

            let ctrl = Control::new(control as *mut bw::Control);
            ctrl.set_event_handler(HANDLER_HOOK.init(hook));
            let result = ((*control).event_handler.unwrap())(control, &mut event);
            assert_eq!(result, 22);
        }
    }
}
