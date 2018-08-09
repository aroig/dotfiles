TODO
====

## Priority

* Implement dropdown windows.
  - resize them on start
  - stash them on the scratchpad

* Implement statusbar with all I need.

## Potential blockers
  - dropdown thing

## Status
  - synced state
  - workers
  - priv
  - media
  - cpu
  - temp
  - memory
  - network
  - email?
  - battery
  - power
  - music
  - volume
  - active keyboard layout
  - time

### j4status
  - extend systemd input plugin to the user instance
  - fix pkgbuild `--sysconfdir=/etc` thing
  - fix assertion thing
  - keyboard layout?
  - report memory usage from /proc/meminfo

## Notifications
### eventd
  - extend journald plugin to have richer matching syntax.

## Applications
  - emacs sometimes hangs. Need to investigate

## Minor

* icon fonts for statusbar

* Configure mouse cursor


## After wlroots

* Make use of XDG_SESSION_ID if set, instead of using the pid to get the session ID.

* Make sure I can configure the wayland display.

* XWayland as socket activated service.
