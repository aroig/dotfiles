TODO
====

* Once sway has moved to wlroots and stabilized, add a patch to read the session
  from XDG_SESSION_ID, or alternatively the seat from XDG_SEAT. This way we will
  be able to run within a systemd user service.


## sway

* force sway to use the given WAYLAND_DISPLAY

* setup xwayland socket activation

* sway segfaults on exit if `WLC_XWAYLAND=0` is set. I tracked it down to the
  release of `selection.listener.link`. I think that resource is initialized in
  an event handler which is not called if there is no XWAYLAND around. I will not fix that mess!
    
        #0  0x00007f0f466268a7 in wl_list_remove () at /usr/lib/libwayland-server.so.0
        #1  0x00007f0f46f165ae in wlc_xwm_selection_release (xwm=<optimized out>) at /home/abdo/build/packages/x86_64-arch-linux/devel/wlc-dev/build/x86_64-arch-linux/src/wlc/src/xwayland/selection.c:460
        #2  0x00007f0f46f155f7 in wlc_xwm_release (xwm=xwm@entry=0x7f0f471333b0 <wlc+2544>) at /home/abdo/build/packages/x86_64-arch-linux/devel/wlc-dev/build/x86_64-arch-linux/src/wlc/src/xwayland/xwm.c:853
        #3  0x00007f0f46ef96e3 in xwayland_event (listener=0x7f0f47133c00 <wlc+4672>, data=<optimized out>) at /home/abdo/build/packages/x86_64-arch-linux/devel/wlc-dev/build/x86_64-arch-linux/src/wlc/src/compositor/compositor.c:318
        #4  0x00007f0f46f12e8c in wl_signal_emit (data=0x7fff864057bf, signal=0x7f0f47133e00 <wlc+5184>) at /usr/include/wayland-server-core.h:388
        #5  0x00007f0f46f12e8c in wlc_xwayland_terminate () at /home/abdo/build/packages/x86_64-arch-linux/devel/wlc-dev/build/x86_64-arch-linux/src/wlc/src/xwayland/xwayland.c:225
        #6  0x00007f0f46f10837 in wlc_cleanup () at /home/abdo/build/packages/x86_64-arch-linux/devel/wlc-dev/build/x86_64-arch-linux/src/wlc/src/wlc.c:160
        #7  0x000000895cd2326e in main ()

* I need a way to set the display of the xwayland server. right now it is hard-coded by wlc.

* sway fails a lot of dbus calls to logind. I'm not sure what is going on.

