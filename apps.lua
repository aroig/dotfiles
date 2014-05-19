---------------------------------------------------------------
-- File:    apps.lua             Apps to use                 --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local os = os

local util = require("abdo.util")

local apps = {}

-- run cmd inside a terminal
function apps.termcmd (cmd, title)
    local shcmd = apps.terminal

    if title then shcmd = shcmd .. string.format(" -t %s", util.shell_escape(title)) end
    if cmd then   shcmd = shcmd .. string.format(" -e %s", util.shell_escape(cmd))   end

    return shcmd
end

-- start a systemd unit
function apps.sdcmd (unit)
    return string.format("systemctl --user start %s", unit)
end


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

apps.dictionary          = apps.sdcmd("goldendict.service")
apps.music               = apps.sdcmd("gmpc.service")
apps.library             = apps.sdcmd("calibre.service")
apps.xournal             = apps.sdcmd("xournal.service")

-- Logging
apps.syslog              = "sudo journalctl -n10 -f"

-- Actions
apps.print               = "scrot -e 'mv $f ~/down/'"

-- Emacs stuff
apps.orgmode             = apps.sdcmd("orgmode.service")
apps.mail                = apps.sdcmd("mu4e.service")
apps.chat                = apps.sdcmd("chat.service")
apps.notes               = apps.sdcmd("notes.service")

return apps
