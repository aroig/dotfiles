---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local os = os

local apps = {}

-- Apps from the environment
apps.terminal            = "urxvt"
apps.editor              = "emacsclient -c -a emacs"
apps.browser             = "chromium"

-- Other apps
apps.docbrowser          = "chromium --user-data-dir=.config/chromium-docs"
apps.filemanager         = "thunar"
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
