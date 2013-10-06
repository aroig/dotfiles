---------------------------------------------------------------
-- File: client.lua      Client management stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

rules = require("awful.rules")

local ddclient = ddclient

-- Client keys
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, shiftkey  }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, ctrlkey   }, "space",  function (c) awful.client.floating.toggle()   end),
    awful.key({ modkey,           }, "t",      function (c) awful.client.floating.toggle()   end),
    awful.key({ modkey,           }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      function (c) awful.client.movetoscreen(c)     end),
    awful.key({ modkey, shiftkey  }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "r",      function (c) c:raise()                        end),
    -- awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = true               end),
    awful.key({ modkey,           }, "m",
              function (c)
                  c.maximized_horizontal = not c.maximized_horizontal
                  c.maximized_vertical   = not c.maximized_vertical
              end
    )
)

-- Client buttons
clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Rules
rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = 1,
                     -- border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     maximized_vertical   = false,
                     maximized_horizontal = false,
                     size_hints_honor = false } },

    -- Floats
    { rule_any = { class = {"Qpaeq", "Qjackctl", "Unison-gtk2", "pinentry", "Skype", "Pavucontrol", "Pidgin"} },
      properties = { floating = true } },

    -- Fixed position floats
    { rule_any = { class = {"mpv", "MPlayer", "feh"} },
      properties = { floating = true },
      callback   = function(c) awful.placement.centered(c) end },

    -- Fixed screen
    { rule = { class = "Xournal" },         properties = { screen = 1 } },
    { rule = { class = "Skype" },           properties = { screen = 1 % nscreen + 1} },

    -- Capture dropdowns
    { rule = { class = "Gmpc" },            callback = function(c) ddclient.music:capture(c) end },
    { rule = { class = "Keepassx" },        callback = function(c) ddclient.pwsafe:capture(c) end },
    { rule = { class = "Xournal" },         callback = function(c) ddclient.xournal:capture(c) end },
    { rule = { class = "Calibre-gui" },     callback = function(c) ddclient.calibre:capture(c) end },

    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
