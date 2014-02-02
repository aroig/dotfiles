---------------------------------------------------------------
-- File: client.lua      Client management stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

rules = require("awful.rules")
posix = require("posix")

local ddclient = ddclient


-- Capture emacs clients depending on WM_NAME.

-- This hackily solves a race condition in which emacs sets the window title too
-- late for awesome rules to detect a custom WM_NAME.
local function capture_emacsen(c)
    -- give it some time
    posix.sleep(1)

    -- capture dropdown according to WM_NAME
    local name = c.name

    if name == 'emacs-org' then
        ddclient.orgmode:capture(c)

    elseif name == 'emacs-chat' then
        ddclient.chat:capture(c)

    elseif name == 'emacs-mail' then
        ddclient.mail:capture(c)

    elseif name == 'emacs-notes' then
        ddclient.notes:capture(c)
    end
end


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

    -- Float dialogs
    { rule_any = { name = {"Print"} },
      properties = { floating = true } },

    -- Centered floats
    { rule_any = { class = {"mpv", "MPlayer", "feh"} },
      properties = { floating = true },
      callback   = function(c) awful.placement.centered(c) end },

    -- Fixed screen
    { rule = { class = "Xournal" },         properties = { screen = 1 } },
    { rule = { class = "Skype" },           properties = { screen = 1 % nscreen + 1} },

    -- Capture dropdowns
    { rule = { class = "Gmpc" },            callback = function(c) ddclient.music:capture(c) end },
    { rule = { class = "Xournal" },         callback = function(c) ddclient.xournal:capture(c) end },
    { rule = { class = "Calibre-gui" },     callback = function(c) ddclient.calibre:capture(c) end },
    { rule = { class = "Goldendict" },      callback = function(c) ddclient.dict:capture(c) end },

    -- Capture emacs clients for dropdown, depending on the WM_NAME property.
    -- Emacs sets the title property too late and there is a race condition here.
    { rule = { class = "Emacs" },           callback = capture_emacsen },

    -- Capture dropdowns in a terminal
    { rule = { class = "Termite", name = "dropdown-terminal" },
      callback = function(c) ddclient.terminal:capture(c) end },

    { rule = { class = "Termite", name = "dropdown-syslog" },
      callback = function(c) ddclient.syslog:capture(c) end },

    { rule = { class = "Termite", name = "dropdown-ranger" },
      callback = function(c) ddclient.ranger:capture(c) end },

    { rule = { class = "Termite", name = "dropdown-sage" },
      callback = function(c) ddclient.sage:capture(c) end },

    { rule = { class = "Termite", name = "dropdown-octave" },
      callback = function(c) ddclient.octave:capture(c) end },

    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}
