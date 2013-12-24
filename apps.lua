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
apps.shell               = os.getenv("SHELL")       or "bash"

-- Other apps
apps.docbrowser          = apps.browser .. " -n"
apps.secondbrowser       = "chromium"
apps.pdfviewer           = "zathura"
apps.music               = "gmpc"
apps.dictionary          = "goldendict"
apps.library             = "systemctl --user start calibre.service"
apps.xournal             = "xournal ~/work/notes/scratchpad.xoj"

-- Actions
apps.print               = "scrot -e 'mv $f ~/down/'"

-- Emacs stuff
apps.orgmode             = "emacs -org"
apps.mail                = "emacs -mail"
apps.notes               = "emacs -notes"
apps.chat                = "emacs -chat"

-- System stuff
local cfgdir = awful.util.getdir("config")

apps.exit_cmd            = string.format("%s/bin/exit.sh", cfgdir)
apps.lock_cmd            = "systemctl --user start lock.target"
apps.suspend_cmd         = "sudo systemctl suspend"
apps.poweroff_cmd        = "sudo systemctl poweroff"
apps.reboot_cmd          = "sudo systemctl reboot"


return apps
