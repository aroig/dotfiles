---------------------------------------------------------------
-- File: client.lua      Client management stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

require("awful.rules")

-- Client keys
clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, shiftkey  }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, ctrlkey   }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey,           }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, shiftkey  }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
)



-- Client buttons
clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))



-- Rules
awful.rules.rules = {
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
    { rule = { class = "Qpaeq" },           properties = { floating = true } },
    { rule = { class = "Qjackctl" },        properties = { floating = true } },
    { rule = { class = "Unison-gtk2" },     properties = { floating = true } },
    { rule = { class = "MPlayer" },         properties = { floating = true } },
    { rule = { class = "feh" },             properties = { floating = true } },
    { rule = { class = "pinentry" },        properties = { floating = true } },
--    { rule = { class = "Gimp" },          properties = { floating = true } },
    { rule = { class = "Skype" },           properties = { floating = true } },
    { rule = { class = "Keepassx" },        properties = { floating = true } },
    { rule = { class = "Nitrogen" },        properties = { floating = true } },
    { rule = { class = "Pavucontrol" },     properties = { floating = true } },
    { rule = { class = "Pidgin" },          properties = { floating = true } },

    -- Fixed screen
    { rule = { class = "Xournal" },         properties = { screen = 1 } },
    { rule = { class = "Skype" },           properties = { screen = 1 % nscreen + 1} },

    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
