---------------------------------------------------------------
-- File: naughty.lua       Naughty related stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

naughty   = require("naughty")    -- Slightly modified notification library
beautiful = require("beautiful")       -- theme

-- Naugty Setup
naughty.config.defaults = {
   timeout          = 8,
   screen           = 1,
   position         = "top_right",
   margin           = "10",

-- height           = 16,
-- width            = 300,
-- gap              = 1,
   ontop            = true,
   font             = beautiful.font_naughty or "sans 8",
-- icon             = nil,
   icon_size        = 64,
   fg               = beautiful.fg_focus,
   bg               = beautiful.bg_focus,
   border_color     = beautiful.border_naughty,
   opacity          = 0.9,
   border_width     = 1,
   hover_timeout    = nil
}

-- Naughty log
naughty.config.notify_callback  = function (args) naughtylog.append(args) return args end

-- It appears there's no svg support :(
-- naughty.config.default_preset.icon_formats = { "png", "gif", "svg"}


naughty.config.presets = {
    normal = {},
    low = {
        timeout = 5
    },
    critical = {
        bg = "#DD5650",
        fg = "#FFFFFF",
        timeout = 0,
    }
}



-- Naugty boxes


orgtasks = require("abdo.box.orgtasks")      -- org todo list
calendar = require("abdo.box.calendar")      -- calendar
syslog = require("abdo.box.syslog")          -- system log
naughtylog = require("abdo.box.naughtylog")  -- log naughty notifications
