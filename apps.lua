---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local os = os

local apps = {}

-- Apps from the environment
apps.terminal            = os.getenv("TERMCMD")
apps.editor              = os.getenv("EMACS")
apps.browser             = os.getenv("BROWSER")

-- Other apps
apps.docbrowser          = "chromium --user-data-dir=.config/chromium-docs"
apps.filemanager         = "nautilus -w"
apps.pdfviewer           = "zathura"
apps.music               = "quodlibet"
apps.twitter             = "urxvt -e turses"
apps.passwordsafe        = "keepassx"
apps.dictionary          = "goldendict"
apps.library             = "calibre"

-- Emacs stuff
apps.orgmode             = "emacs -org"
apps.mail                = "emacs -mail"
apps.news                = "emacs -news"
apps.notes               = "emacs -notes"
apps.chat                = "emacs -chat"

return apps
