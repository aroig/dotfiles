---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local os = os

local apps = {
    terminal            = os.getenv("TERMCMD"),
    editor              = os.getenv("EMACS"),
    browser             = os.getenv("BROWSER"),
    docbrowser          = "midori --private",
    filemanager         = "nautilus",
    pdfviewer           = "zathura",
    orgmode             = "emacs -org",
    mail                = "emacs -mail",
    news                = "emacs -news",
    notes               = "emacs -notes"
}

return apps
