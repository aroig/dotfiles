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
    return string.format("systemctl --user start %s", util.shell_escape(unit))
end

-- Apps from the environment
apps.terminal            = os.getenv("TERMCMD")     or "xterm"
apps.editor              = os.getenv("EMACS")       or "emacs"
apps.browser             = os.getenv("BROWSER")     or "chromium"
apps.filemanager         = os.getenv("FILEMANAGER") or "thunar"
apps.shell               = os.getenv("SHELL")       or "bash"

-- Other apps
apps.docbrowser          = apps.browser .. " -n"
apps.secondbrowser       = "chromium"
apps.pdfviewer           = "zathura"

-- Actions
apps.print               = "scrot -e 'mv $f ~/down/'"

return apps
