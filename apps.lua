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

apps.dictionary          = "systemctl --user start goldendict.service"
apps.music               = "systemctl --user start gmpc.service"
apps.library             = "systemctl --user start calibre.service"
apps.xournal             = "systemctl --user start xournal.service"

-- Logging
apps.syslog              = "sudo journalctl -n10 -f"

-- Actions
apps.print               = "scrot -e 'mv $f ~/down/'"

-- Emacs stuff
apps.orgmode             = "systemctl --user start orgmode.service"
apps.mail                = "systemctl --user start mu4e.service"
apps.chat                = "systemctl --user start chat.service"
apps.notes               = "systemctl --user start notes.service"

-- System stuff
local cfgdir = awful.util.getdir("config")

apps.quit_cmd            = "systemctl --user start quit-wm.target"
apps.lock_cmd            = "systemctl --user start lock.target"
apps.suspend_cmd         = "systemctl --user start sleep.target"
apps.poweroff_cmd        = "sudo systemctl poweroff"
apps.reboot_cmd          = "sudo systemctl reboot"


return apps
