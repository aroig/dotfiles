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
