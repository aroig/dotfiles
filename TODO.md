TODO
====

* Once sway has moved to wlroots and stabilized, add a patch to read the session
  from XDG_SESSION_ID, or alternatively the seat from XDG_SEAT. This way we will
  be able to run within a systemd user service.


## sway

* force sway to use the given WAYLAND_DISPLAY

* setup xwayland socket activation

* sway segfaults on exit if `WLC_XWAYLAND=0` is set

* sway fails a lot of dbus calls to logind

