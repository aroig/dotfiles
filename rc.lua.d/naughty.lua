---------------------------------------------------------------
-- File: naughty.lua       Naughty related stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local naughty   = naughty
local beautiful = beautiful

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


local sound = {
    gnoti           = beautiful.naughty_mail_sound,
    weechat         = beautiful.naughty_chat_sound,
    gtalk           = beautiful.naughty_chat_sound,
    rcirc           = beautiful.naughty_chat_sound,
    ["notify-send"] = beautiful.naughty_alert_sound,
}


local icon = {
    gnoti           = beautiful.naughty_mail_icon,
    weechat         = beautiful.naughty_chat_icon,
    gtalk           = beautiful.naughty_chat_icon,
    rcirc           = beautiful.naughty_chat_icon,
    ["notify-send"] = beautiful.naughty_alert_icon,
}


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


-- Naughty callback
function naughty_callback (args)
    -- log notification
    naughtylog.append(args)

    -- play sound
    if sound[args.appname] then
        exec(string.format("play \"%s\"", sound[args.appname]))
    end

    -- change icon
    if icon[args.appname] and not args.icon then
        args.icon = icon[args.appname]
    end

    return args
end

naughty.config.notify_callback  = naughty_callback
