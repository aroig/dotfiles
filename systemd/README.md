User Units
==========

## Notes

* My session is currently broken, since all systemd --user runs outside any
  session.

* xorg since 1.16 is able to run unprivileged, thanks to logind who manages the
  resources. However, since I run xorg outside a session this doesn't seem to
  work.

  It seems xorg uses GetSessionByPID for no reason, and that is the thing that
  enforces to run xorg within a session.
  
  http://lists.x.org/archives/xorg-devel/2014-February/040476.html

* I attempted to push environment variables to systemd from systemd services but seems to
  be racy. For xorg in particular, DISPLAY and XDG_VTNR need to be set externally before
  starting the service.

## Targets and services
* System state
  - `poweroff.target`: suspend system, maybe asking the user to close stuff (emacs...) 
  - `reboot.target`: reboot system, maybe asking the user to close stuff (emacs...)
  - `suspend.target`: suspend system, taking care to settle stuff (ssh, stop music...)
  - `wmquit.target`: quit window manager, taking care to settle stuff

* Hardware state
  - `power-ac.target`: using AC power
  - `power-battery.target`: using battery power

* Hooks
  - `shutdown.target`: pulls any automatic action that needs to be taken before shutdown
  - `sleep.target`: pulls any automatic action that needs to be taken before sleep
  - `wakeup.target`: pulls any actions to be taken on timer-triggered wakeups
  - `synced.target`: pull any service that must be started after a successful sync
  - `umount.target`: conflicts with all filesystems I may have mounted
  - `graphical-session.target`: pulled from the graphical environment.
  - `wmclean.target`: pulled before quiting wm. Asks user to close emacs buffers, etc.

* Window manager states
  - `console.target`: no wm, only terminal
  - `desktop.target`: wm in desktop mode
  - `laptop.target`: wm in laptop mode
  - `tablet.target`: wm in tablet mode
  - `lock.target`: wm is locked

* Sync services
  - `sync-hi.service`: synchronize at the begining of the day
  - `sync-bye.service`: synchronize at the end of the day
  - `sync-byeh.service`: synchronize and halt the system
  - `sync-byez.service`: synchronize and suspend the system
  - `sync-fake.service`: enter synced state without any syncing

* Fetching
  - `fetch.target`
  - `fetch-mail.target`
  - `fetch-news.target`
  - `fetch-contacts.target`


## Session issues
Due to the fact that I run the entire user processes outside of a session, the following
happens:

* external disks can't be mounted unprivileged
* I run `systemd-inhibit` and `systemctl poweroff` as root. Once they work outside of a
  session I should change to run them unprivileged!

All of this will eventually be fixed once upstream has kdbus in place, and the session
stuff can be advanced! Then, processes will not be required to descend from a session
leader. It will only be required that the associated user has an active session.

## Inhibition locks
