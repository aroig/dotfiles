apps = require("apps")

return {
   -- systemd units
   termite               = "termite",
   docs                  = "dwb -p docs",
   ranger                = apps.termcmd("ranger", "ranger"),
   syslog                = apps.termcmd("sudo journalctl -n10 -f", "syslog"),
   octave                = apps.termcmd("octave", "octave"),
   sage                  = apps.termcmd("sage", "sage"),
   ipython               = apps.termcmd("ipython", "ipython"),
   lua                   = apps.termcmd("lua", "lua"),
   ghci                  = apps.termcmd("ghci", "ghci"),
}
