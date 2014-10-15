apps = require("apps")

return {
   -- top dropdowns
   termite               = "termite",
   thunar                = "thunar",
   ranger                = apps.termcmd("ranger", "ranger"),
   octave                = apps.termcmd("octave", "octave"),
   sage                  = apps.termcmd("sage", "sage"),
   ipython               = apps.termcmd("ipython", "ipython"),
   ipython2              = apps.termcmd("ipython2", "ipython2"),
   lua                   = apps.termcmd("lua", "lua"),
   ghci                  = apps.termcmd("ghci", "ghci"),
   ["awesome-client"]    = apps.termcmd("awesome-client", "awesome-client"),
   notes                 = "notes.service",

   -- monitoring
   syslog                = apps.termcmd("jctl -f -n10", "syslog-monitor"),
   htop                  = apps.termcmd("htop", "htop-monitor"),
   iftop                 = apps.termcmd("sudo iftop", "iftop-monitor"),
   iotop                 = apps.termcmd("sudo iotop", "iotop-monitor"),
   radeontop             = apps.termcmd("sudo radeontop", "radeontop-monitor"),
   atop                  = apps.termcmd("atop -af 1", "atop"),
   midi                  = apps.termcmd("jack_midi_dump", "midi-monitor"),

   -- sync and update actions
   hi                    = apps.termcmd("ictl sync-hi.service",    "hi"),
   bye                   = apps.termcmd("ictl sync-bye.service",   "bye"),
   byez                  = apps.termcmd("ictl sync-byez.service",  "byez"),
   fetch                 = apps.termcmd("ictl fetch.target",       "fetch"),
   sysupgrade            = apps.termcmd("ictl sysupgrade.service", "sysupgrade"),

   -- side dropdowns
   docs                  = "dwb -p docs",
   gmpc                  = "gmpc.service",
   chat                  = "chat.service",
   xournal               = "xournal.service",
   goldendict            = "goldendict.service",

   -- full dropdowns
   mu4e                  = "mu4e.service",
   calibre               = "calibre.service",
   org                   = "orgmode.service",
}
