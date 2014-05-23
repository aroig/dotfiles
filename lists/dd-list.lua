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
   syslog                = apps.termcmd("sudo journalctl -n10 -f", "syslog"),
   htop                  = apps.termcmd("htop", "htop"),
   iftop                 = apps.termcmd("sudo iftop", "iftop"),
   iotop                 = apps.termcmd("sudo iotop", "iotop"),

   -- sync actions
   hi                    = apps.termcmd("ictl hi.target",    "hi"),
   bye                   = apps.termcmd("ictl bye.target",   "bye"),
   byez                  = apps.termcmd("ictl byez.target",  "byez"),
   fetch                 = apps.termcmd("ictl fetch.target", "fetch"),

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
