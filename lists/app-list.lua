apps = require("apps")

return {
   -- systemd units
   dwb                       = "dwb@.service",
   chromium                  = "chromium@.service",
   thunar                    = "thunar@.service",
   termite                   = "termite@.service",
   ranger                    = "ranger@.service",
   emacsclient               = "emacsclient@.service",

   org                       = "orgmode.service",
   calibre                   = "calibre.service",
   mu4e                      = "mu4e.service",
   chat                      = "chat.service",
   notes                     = "notes.service",
   xournal                   = "xournal.service",
   gmpc                      = "gmpc.service",
   goldendict                = "goldendict.service",

   firefox                   = "firefox",
   deluge                    = "deluge-gtk",
   amule                     = "amulegui",
   pidgin                    = "pidgin",
   skype                     = "skype",
   guvcview                  = "guvcview",

   pavucontrol               = "pavucontrol",
   qjackctl                  = "qjackctl",
   patchage                  = "patchage",
   qsynth                    = "qsynth",

   ["gnome-control-center"]  = "gnome-control-center",
   ["gnome-disks"]           = "gnome-disks",
   unison                    = "unison-gtk2",
   virtualbox                = "virtualbox",

   matlab                    = "matlab",
   sage                      = apps.termcmd("sage", "sage"),
   octave                    = apps.termcmd("octave", "octave"),
   maple                     = "xmaple",
   stellarium                = "stellarium",
   boinc                     = "boincm",

   ipython                   = apps.termcmd("ipython", "ipython"),
   ipython2                  = apps.termcmd("ipython2", "ipython2"),
   lua                       = apps.termcmd("lua", "lua"),
   ghci                      = apps.termcmd("ghci", "ghci"),

   libreoffice               = "libreoffice",
   gcstar                    = "gcstar",

   gimp                      = "gimp",
   inkscape                  = "inkscape",
   blender                   = "blender",
   luxrender                 = "luxrender",
   scribus                   = "scribus",
   qcad                      = "qcad",
   sigil                     = "sigil",
   pdfeditor                 = "pdfeditor",
   k3dsurf                   = "k3dsurf",
   kpovmodeler               = "kpovmodeler",
   openoffice                = "soffice",
   gucharmap                 = "gucharmap",

   ardour                    = "ardour3",
   rosegarden                = "rosegarden",
   denemo                    = "denemo",
   musescore                 = "mscore",

   avidemux                  = "avidemux2_gtk",
   subtitleeditor            = "subtitleeditor",
   puddletag                 = "puddletag",
   brasero                   = "brasero",

   qtcreator                 = "qtcreator",

   -- system state
   quit                      = "wm-quit.service",
   lock                      = "lock.target",
   suspend                   = "suspend.target",
   poweroff                  = "poweroff.target",
   reboot                    = "reboot.target",

   -- desktop state
   priv                      = "mount-priv.service",
   ["fake-sync"]             = "initial-synced-fake.service",
}
