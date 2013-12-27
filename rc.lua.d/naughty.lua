---------------------------------------------------------------
-- File: naughty.lua       Naughty related stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local naughty   = naughty
local beautiful = beautiful
local box = box

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
   hover_timeout    = nil,
}

-- icon autodetection is pretty broken. I set absolute paths on theme.lua
naughty.config.icon_formats = { "png", "gif", "svg"}
naughty.config.icon_dirs = { os.getenv("HOME") .. "/.icons/" }


local sound = {
    mail            = beautiful.naughty_mail_sound,
    weechat         = beautiful.naughty_chat_sound,
    gtalk           = beautiful.naughty_chat_sound,
    rcirc           = beautiful.naughty_chat_sound,
    twitter         = beautiful.naughty_chat_sound,
    ["notify-send"] = beautiful.naughty_alert_sound,
}


local icon = {
    mail            = beautiful.naughty_mail_icon,
    weechat         = beautiful.naughty_chat_icon,
    gtalk           = beautiful.naughty_chat_icon,
    rcirc           = beautiful.naughty_chat_icon,
    twitter         = beautiful.naughty_chat_icon,
    ["notify-send"] = beautiful.naughty_alert_icon,
    run             = beautiful.naughty_app_icon,
}

-- Set presets for different urgency levels
naughty.config.presets.low.timeout = 5

naughty.config.presets.critical.timeout = 0
naughty.config.presets.critical.fg = "#FFFFFF"
naughty.config.presets.critical.bg = "#B74747"


-- Naughty callback
function naughty_callback (args)
    -- log notification
    box.naughtylog.append(args)

    -- play sound
    if sound[args.appname] then
        exec(string.format("play -q \"%s\"", sound[args.appname]))
    end

    -- change icon
    if icon[args.appname] and not args.icon then
        args.icon = icon[args.appname]
    end

    return args
end

naughty.config.notify_callback  = naughty_callback
