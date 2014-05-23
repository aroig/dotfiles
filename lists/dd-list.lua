apps = require("apps")

return {
   -- top dropdowns
   termite               = "termite",
   ranger                = apps.termcmd("ranger", "ranger"),
   syslog                = apps.termcmd("sudo journalctl -n10 -f", "syslog"),
   octave                = apps.termcmd("octave", "octave"),
   sage                  = apps.termcmd("sage", "sage"),
   ipython               = apps.termcmd("ipython", "ipython"),
   lua                   = apps.termcmd("lua", "lua"),
   ghci                  = apps.termcmd("ghci", "ghci"),
   ["awesome-client"]    = apps.termcmd("awesome-client", "awesome-client"),
   notes                 = "notes.service",

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
