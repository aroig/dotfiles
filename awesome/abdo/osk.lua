----------------------------------------------------
-- On Screen Keyboard for the awesome window manager
----------------------------------------------------
-- Coded  by: Farhaven  <gbe@ring0.de>
-- Hacked by: Adrian C. <anrxc@sysphere.org>
-- Licensed under the WTFPL version 2
--   * http://sam.zoy.org/wtfpl/COPYING
----------------------------------------------------
-- To use this module add:
--     require("osk")
-- to your rc.lua, and call it from a keybinding:
--     osk(position, screen)
--
-- Parameters:
--   position - optional, "bottom" by default
--   screen   - optional, screen.count() by default
----------------------------------------------------

-- WARNING: this is still broken. Need to update it to use new wibox


-- Grab environment
local util     = require("awful.util")
local button   = require("awful.button")
local wibox    = require("wibox")
local table    = table
local ipairs   = ipairs
local tostring = tostring
local setmetatable = setmetatable
local capi     = {
    screen     = screen,
    fake_input = root.fake_input
}

-- OSK: On Screen Keyboard for the awesome window manager
module("osk")

-- Variable definitions
local kbd = {}
kbd.codes = {
    q=24,    w=25, e=26, r=27, t=28, z=52, u=30, i=31, o=32,   p=33,   ["."]=60,
    a=38,    s=39, d=40, f=41, g=42, h=43, j=44, k=45, l=46,
    Caps=66, y=29, x=53, c=54, v=55, b=56, n=57, m=58, Spc=65, Ret=36, Del=22,
}

-- Create a chain of key widgets for an OSK row
local function create_button_row(...)
    local widgets = { horizontal = wibox.layout.flex.horizontal }

    for _, i in ipairs({...}) do
        local w = wibox.widget.textbox()
--        w:margin({ top = 10, left = 10, right = 10, bottom = 10 })
        w.border_width = 1
        w.text_align   = "center"
        w.border_color = "#1E2320"
        w.text = util.escape(tostring(i))
        w:buttons(util.table.join(
            button({ }, 1, nil, function ()
                capi.fake_input("key_press",   kbd.codes[i])
                capi.fake_input("key_release", kbd.codes[i])
            end)
        ))

        table.insert(widgets, w)
    end

    return widgets
end

-- Create a wibox holding OSK rows and toggle its visibility
setmetatable(_M, { __call = function (_, pos, scr)
    if not kbd.init then
        kbd.box = wibox({
                height   = 100,
                width = 100,
            position = pos or "bottom",
            screen   = scr or capi.screen.count(),
            fg       = "#F0DFAF",
            bg       = "#4F4F4F",
            widgets  = {
                { create_button_row("q", "w", "e", "r", "t", "z", "u", "i", "o", "p", ".") },
                { create_button_row("a", "s", "d", "f", "g", "h", "j", "k", "l") },
                { create_button_row("Caps", "y", "x", "c", "v", "b", "n", "m", "Spc", "Ret", "Del") },
                layout = wibox.layout.flex.vertical
            }
        })
        kbd.init = true
        kbd.box.visible = false
    end

    kbd.box.visible = not kbd.box.visible
end })
