---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local os = os

local apps = {}

-- Apps from the environment
apps.terminal            = os.getenv("TERMCMD")     or "urxvt"
apps.editor              = os.getenv("EMACS")       or "emacsclient -c -a emacs"
apps.browser             = os.getenv("BROWSER")     or "chromium"

-- Other apps
apps.docbrowser          = "chromium --user-data-dir=.config/chromium-docs"
apps.filemanager         = os.getenv("FILEMANAGER") or "thunar"
apps.pdfviewer           = "zathura"
apps.music               = "ario"
apps.twitter             = "urxvt -e turses"
apps.passwordsafe        = "keepassx"
apps.dictionary          = "goldendict"
apps.library             = "calibre"

-- Emacs stuff
apps.orgmode             = "emacs -org"
apps.mail                = "emacs -mail"
apps.notes               = "emacs -notes"
apps.chat                = "emacs -chat"

return apps
