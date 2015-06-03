---------------------------------------------------------------
-- File: client.lua      Client management stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local systemd = systemd

local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}


-----------------------------------
-- Useful functions              --
-----------------------------------

-- Run a function delayed by a timeout. Puts the args in a closure. Seems not good w hen
-- the argument is a client, maybe the object gets copied?
local function run_delayed(func, timeout, args)
    local t = timer({timeout = timeout or 0})
    func(unpack(args))
    t:connect_signal("timeout",
                 function()
                     local args = args or {}
                     t:stop()
                     func(unpack(args))
                 end)
    t:start()
end


local function set_geometry(c, geom)
    local screengeom = capi.screen[c.screen].workarea
    local width, height, x, y

    -- if width or height values in the interval [0,1] represent a fraction of screen width or height.
    if geom.width  <= 1 then width  = screengeom.width  * geom.width
    else                     width  = geom.width
    end

    if geom.height <= 1 then height = screengeom.height * geom.height
    else                     height = geom.height
    end

    -- horizontal alignment: top, bottom, center
    if     geom.horiz == "left"  then x = screengeom.x
    elseif geom.horiz == "right" then x = screengeom.x + screengeom.width - width
    else                              x =  screengeom.x + (screengeom.width - width)/2
    end

    -- vertical alignment: top, bottom, center
    if     geom.vert == "bottom" then y = screengeom.y + screengeom.height - height
    elseif geom.vert == "top" then    y = screengeom.y
    else                              y = screengeom.y + (screengeom.height - height)/2
    end

    -- set the geometry of the client
    local bw = c.border_width
    c:geometry({ x = x, y = y, width = width - 2*bw, height = height - 2*bw })
end


local function geometry_cb(geom)
    local geom = geom
    return function (c) set_geometry(c, geom) end
end


local function swap_to_master(c)
    local m = awful.client.getmaster()
    if c ~= nil and m ~= nil then
        c:swap(m)
    end
end


-----------------------------------
-- Client bindings and buttons   --
-----------------------------------

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, shiftkey  }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, ctrlkey   }, "space",  function (c) awful.client.floating.toggle()   end),
    awful.key({ modkey,           }, "t",      function (c) awful.client.floating.toggle()   end),
    awful.key({ modkey,           }, "Return", function (c) swap_to_master(c)                end),
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



-----------------------------------
-- Signal handlers               --
-----------------------------------

-- adds manage signal for setting rules
rules = require("awful.rules")

-- systemd signals to capture cgroups and apply rules
capi.client.connect_signal("manage",
                           function(c, startup)
                               -- capi.client.focus = c
                               systemd.manage_client(c)
                               dropdown.manage_client(c)
                           end)

capi.client.connect_signal("unmanage",
                           function(c)
                               systemd.unmanage_client(c)
                               dropdown.unmanage_client(c)
                           end)


capi.client.connect_signal("focus",
                           function(c)
                               systemd.focus_client(c)
                               dropdown.focus_client(c)
                           end)



-----------------------------------
-- Rules                         --
-----------------------------------

-- awful rules
rules.rules = {
    -- All clients will match this rule.
    { rule = { }, except_any = { class = {"Tint2", "Plank"} },
      properties = { border_width = 1,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise=true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     maximized_vertical   = false,
                     maximized_horizontal = false,
                     size_hints_honor = false } },

    -- Panels or docks
    { rule_any = { class = { "Tint2", "Plank"} },
      properties = { floating = true,
                     focus = false } },


    -- Floats
    { rule_any = { class = {"Qpaeq", "Qjackctl", "Unison-gtk2", "pinentry", "Skype", "Pavucontrol", "Pidgin",
                            "Vmpk", "Wpa_gui"} },
      properties = { floating = true } },

    -- Float dialogs
    { rule_any = { name = {"Print"} },
      properties = { floating = true } },

    -- Float youtube, etc.
    { rule_any = { instance = { "plugin-container", "exe" } },
      properties = { floating = true, focus = true } },

    -- Centered floats
    { rule_any = { class = {"mpv", "MPlayer", "feh"} },
      properties = { floating = true },
      callback   = function(c) awful.placement.centered(c) end },

    -- Fixed screen
    { rule = { class = "Xournal" },         properties = { screen = 1 } },
    { rule = { class = "Skype" },           properties = { screen = 1 % nscreen + 1} },

    -- Set Firefox to always map on tags number 2 of screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { tag = tags[1][2] } },
}



-- systemd cgroup rules
systemd.rules = {
    -- float the dropdowns
    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/.*$', main = true },
      properties = { floating = true,
                     size_hints_honor = false,
                     ontop = true,
                     above = true,
                     skip_taskbar = true } },

    -- float virtual machines
    { rule       = { },
      process    = { cgroup = 'machines%.slice/.*$', main = false },
      properties = { floating = true },
      callback   = function(c) awful.placement.centered(c) end },

    -- default dropdown geometry
    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/.*$', main = true },
      callback   = geometry_cb({vert="top",    horiz="center", width=1.0, height=0.4}) },

    -- set the geometry for side dropdowns
    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/xournal%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="left",   width=0.5, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/orgmode%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="left",   width=1.0, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/calibre%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="left",   width=1.0, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/mu4e%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="left",   width=1.0, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/chat%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="left",   width=0.6, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/goldendict%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="right",  width=0.6, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/gmpc%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="right",  width=0.7, height=1.0}) },

    { rule       = { },
      except     = { modal = true },
      process    = { cgroup = 'dropdown%.slice/.*docs.*%.service$', main = true },
      callback   = geometry_cb({vert="center", horiz="right",  width=0.6, height=1.0}) },
}


-----------------------------------
-- Signals                       --
-----------------------------------
