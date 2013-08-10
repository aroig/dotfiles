---------------------------------------------------------------
-- File:    rc.lua             Main config file              --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

-----------------------------------
-- Module Loading                --
-----------------------------------

-- Standard awesome library
awful        = require("awful")
awful.rules  = require("awful.rules")
awful.remote = require("awful.remote")
awful.tag    = require("awful.tag")
awful.util   = require("awful.util")

beautiful    = require("beautiful")
menubar      = require("menubar")
gears        = require("gears")
naughty      = require("naughty")


-- Personal stuff
util        = require("abdo.util")           -- Utility functions
layout      = require("abdo.layout")         -- My layouts
apps        = require("apps")                -- My preferred apps
revelation  = require("abdo.revelation")     -- Tweaked expose


-----------------------------------
-- Error handling                --
-----------------------------------
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)

if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end


-----------------------------------
-- Variables                     --
-----------------------------------

-- Local
local cfgdir = awful.util.getdir("config")

-- Global
hostname  = awful.util.pread("hostname"):gsub("\n", "")
homedir   = os.getenv("HOME")

-- Modkeys
modkey   = "Mod4"
metakey  = "Mod1"
ctrlkey  = "Control"
shiftkey = "Shift"


-----------------------------------
-- Initial stuff                 --
-----------------------------------

-- Let always someone have focus.
require("awful.autofocus")

-- Load theme
beautiful.init(awful.util.getdir("config") .. "/themes/zenburn/theme.lua")


-----------------------------------
-- Layouts and tags              --
-----------------------------------

-- Layouts
-- Table of layouts to cover with awful.layout.inc, order matters.
layouts = {
    layout.dwindle,
    layout.tile,
    layout.fair,
--    layout.tile,
--    awful.layout.suit.max,
--    awful.layout.suit.floating,
--    awful.layout.suit.fair
--    awful.layout.suit.tile.left,
--    awful.layout.suit.tile.bottom,
--    awful.layout.suit.tile.top,
--    awful.layout.suit.max.fullscreen,
--    awful.layout.suit.fair.horizontal,
--    awful.layout.suit.spiral.dwindle,
--    awful.layout.suit.magnifier
}


-- Tags
-- Define a tag table which hold all screen tags.
nscreen = screen.count()

tags = {}
for s = 1, screen.count() do
   -- Each screen has its own tag table.
   tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 }, s, layouts[1])
end


-----------------------------------
-- Sourcing stuff                --
-----------------------------------

dofile(cfgdir .. "/rc.lua.d/wallpaper.lua")   -- Setup wallpaper
dofile(cfgdir .. "/rc.lua.d/launchers.lua")   -- Launcher functions
dofile(cfgdir .. "/rc.lua.d/naughty.lua")     -- Notifications
dofile(cfgdir .. "/rc.lua.d/mywidgets.lua")   -- Widgets config
dofile(cfgdir .. "/rc.lua.d/mywibox.lua")     -- Wibox config
dofile(cfgdir .. "/rc.lua.d/mymenu.lua")      -- Menus
dofile(cfgdir .. "/rc.lua.d/globalkeys.lua")  -- Globak keys
dofile(cfgdir .. "/rc.lua.d/client.lua")      -- Client rules
dofile(cfgdir .. "/rc.lua.d/signals.lua")     -- Signals

dofile(cfgdir .. "/rc.lua.d/autoload.lua")    -- Autoload
