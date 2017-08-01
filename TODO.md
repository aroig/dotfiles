TODO
====

* Make `systemd --user` notify ready state when reaching `basic.target` instead
  of waiting to become idle. Without this long-running jobs started as deps of
  `default.target` can make the login timeout.

* Write a generator for generating the sshmux@%i.socket files

* Attempt to fix emacs hang on shutdown. 

* Debug `emacs-ask-save-quit.service`. Need a way to query the user from a
  frame-less emacs. I could produce a dialog with zenity, for example.

* Add a proxy service for sway, that gets started and stopped from sway exec's.

* use systemd-inhibit for emacs ask-save and other crytical stuff.

* Run `wakeup.target` when waking up from sleep. Don't see an obvious way to do it.
* Setup sway, xwayland, etc.

* Make a preset for user services in roberta's user.

* Different hosts need different user units. For example, disable tmp cleanups on
  skynet. Either use `ConditionHost` more often, or use presets.

* I removed explicit dependencies on `mount-priv.service` because failed on skynet. Think
  how to replace them.

* Isolate web browser services.

* Only mount `~/priv` on certain services.

* Add an `After` for all `Conflicts`, so they start and stop one after the other.

* can vncserver be made socket activated, like xorg?

* make sure the sync + shutdown still works fine

* Cleanup the xorg services

* remove as much as sudo's from services as I can.

* remove as much as systemctl's from services as I can.

* make synced.target into a RefuseManualStart thing. Note, the fake-sync would fail I guess

* add notifications via jwatch:
  - emacs shutdown
  - imminent shutdown
  - DNSSEC misses

* sync units should wait until the last commit is done!

* socket activated sshmux.serivce
  - currently, I can't use systemd-socket-proxy, because it produces an error. Look at
    ssh-proxy.service for more details.

* test mpv screensaver inhibition for xautolock. Setup xautolock, etc.

## Systemd issues

* journal has a race in which short-lived processes do not get all metadata attached, like
  cgroup. This needs fixing in kernel, but seems it will not
  happen. http://comments.gmane.org/gmane.linux.kernel/1551621
