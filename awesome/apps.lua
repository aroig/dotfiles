---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local os = os

local apps = {}

-- Apps from the environment
apps.terminal            = os.getenv("TERMCMD")     or "xterm"
apps.editor              = os.getenv("EMACS")       or "emacs"
apps.browser             = os.getenv("BROWSER")     or "dwb"
apps.filemanager         = os.getenv("FILEMANAGER") or "thunar"

-- Other apps
apps.docbrowser          = apps.browser .. " -n"
apps.secondbrowser       = "chromium"
apps.pdfviewer           = "zathura"
apps.music               = "gmpc"
apps.twitter             = apps.terminal .. " -e turses"
apps.pwsafe              = "keepassx"
apps.dictionary          = "goldendict"
apps.library             = "calibre"
apps.print               = "scrot -e 'mv $f ~/down/'"
apps.xournal             = "xournal ~/work/notes/scratchpad.xoj"

-- Emacs stuff
apps.orgmode             = "emacs -org"
apps.mail                = "emacs -mail"
apps.notes               = "emacs -notes"
apps.chat                = "emacs -chat"

-- System stuff
apps.exit_cmd            = "bash ~/.config/awesome/exit.sh"
apps.lock_cmd            = "systemctl --user start lock.target"
apps.suspend_cmd         = "sudo systemctl suspend"
apps.poweroff_cmd        = "sudo systemctl poweroff"
apps.reboot_cmd          = "sudo systemctl reboot"


return apps
