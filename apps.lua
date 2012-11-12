---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


local apps = {
   terminal            = "urxvt",
   light_filemanager   = "thunar",
   filemanager         = "nautilus",
   filemanager_alt     = "nemo",
   browser             = "luakit",
   docbrowser          = "midori --private",
   editor              = "emacsclient -c -a emacs",
   pdfviewer           = "zathura",
   orgmode             = "emacs -org",
   mail                = "emacs -mail",
   news                = "emacs -news",
   notes               = "emacs -notes"
}

return apps
