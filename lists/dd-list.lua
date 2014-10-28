apps = require("apps")

-- This configures a list of commands. Every command can have a prefix as follows:
--     cm: command to be run in a transient systemd unit
--     tm: command to be run. It runs it in a terminal
--     sd: systemd unit to start. if unit is blah@.service, starts
--         different instances every time it is executed.
--


return {
   -- top dropdowns
   termite               = "termite-dropdown@.service",
   thunar                = "thunar-dropdown@.service",
   notes                 = "notes-dropdown.service",

   ranger                = "tm:ranger",
   octave                = "tm:octave",
   sage                  = "tm:sage",
   ipython               = "tm:ipython",
   ipython2              = "tm:ipython2",
   lua                   = "tm:lua",
   ghci                  = "tm:ghci",
   ["awesome-client"]    = "tm:awesome-client",

   -- monitoring
   journal               = "journal-dropdown.service",
   htop                  = "tm:htop",
   iftop                 = "tm:sudo iftop",
   iotop                 = "tm:sudo iotop",
   radeontop             = "tm:sudo radeontop",
   atop                  = "tm:atop -af 1",
   midi                  = "tm:jack_midi_dump",

   -- sync and update actions
   hi                    = "sync-hi.service",
   bye                   = "sync-bye.service",
   byez                  = "sync-byez.service",
   fetch                 = "fetch.target",
   sysupgrade            = "sysupgrade.service",

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
