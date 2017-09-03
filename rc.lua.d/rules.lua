
local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}


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



-----------------------------------
-- Signal handlers               --
-----------------------------------

-- adds manage signal for setting rules
rules = require("awful.rules")

capi.client.connect_signal("unmanage",
                           function(c)
                               dropdown.unmanage_client(c)
                           end)


capi.client.connect_signal("focus",
                           function(c)
                               dropdown.focus_client(c)
                           end)



-----------------------------------
-- Rules                         --
-----------------------------------

-- awful rules
clientrules = gears.table.join(
    clientrules,

    {
        -- All clients will match this rule.
        { rule = { }, except_any = { class = {"Tint2", "Plank"} },
          properties = { border_width = 1,
                         border_color = beautiful.border_normal,
                         focus = awful.client.focus.filter,
                         raise=true,
                         screen = awful.screen.preferred,
                         placement = awful.placement.no_overlap+awful.placement.no_offscreen,
                         maximized_vertical   = false,
                         maximized_horizontal = false,
                         size_hints_honor = false } },

        -- Panels or docks
        { rule_any = { class = { "Tint2", "Plank"} },
          properties = { floating = true,
                         focus = false } },

        -- Priority
        { rule_any = { instance = {"pinentry"} },
          properties = { floating = true,
                         ontop = true,
                         focus = true,
                         placement = awful.placement.centered } },

        -- Floats
        -- NOTE: For some reason wpa_gui does not let us center it.
        { rule_any = { instance = {"pavucontrol", "wpa_gui"} },
          properties = { floating = true,
                         placement = awful.placement.centered } },

        -- Float dialogs
        { rule_any = { name = {"Print"} },
          properties = { floating = true } },

        -- Float youtube, etc.
        { rule_any = { instance = { "plugin-container", "exe", 'vmpk' } },
          properties = { floating = true,
                         focus = true,
                         placement = awful.placement.centered } },

        -- Centered floats
        { rule_any = { class = {"mpv", "MPlayer", "feh", } },
          properties = { floating = true,
                         placement = awful.placement.centered } },

        -- Fixed screen
        { rule_any = { class = {"Xournal", "Skype" } },
          properties = { screen = 1 } },

        -- qemu
        { rule_any = { class = { "qemu-system-x86_64" } },
          properties = { floating = true,
                         placement = awful.placement.top_left,
                         honor_workarea = true } },

        -- dropdowns
        --
        -- NOTE: we match by instance (which I can set on emacs), then use this instance as
        -- name for the dropdown. This name is used when calling ddtoggle("app:name"), and
        -- must coincide with the name configured in the apps list.

        -- full-screen dropdowns
        { rule_any = { instance = {"mu4e", "org", "chat", "calibre-gui"} },
          properties = { floating = true,
                         size_hints_honor = false,
                         ontop = true,
                         above = true,
                         skip_taskbar = true },
          callback = function(c)
              if not c.modal then
                  local name = string.gsub(c['instance'], "-gui", "")
                  dropdown.manage_client(name, c)
                  set_geometry(c, {vert="center", horiz="center", width=1.0, height=1.0} )
              end
          end
        },

        -- top dropdowns
        { rule_any = { instance = { "termite-dropdown", "journal-dropdown", "vifm-dropdown"} },
          properties = { floating = true,
                         size_hints_honor = false,
                         ontop = true,
                         above = true,
                         skip_taskbar = true },
          callback = function(c)
              if not c.modal then
                  local name = c['instance']
                  dropdown.manage_client(name, c)
                  set_geometry(c, {vert="top", horiz="center", width=1.0, height=0.4} )
              end
          end
        },

        -- tall top dropdowns
        { rule_any = { instance = { "glances"} },
          properties = { floating = true,
                         size_hints_honor = false,
                         ontop = true,
                         above = true,
                         skip_taskbar = true },
          callback = function(c)
              if not c.modal then
                  local name = c['instance']
                  dropdown.manage_client(name, c)
                  set_geometry(c, {vert="top", horiz="center", width=1.0, height=0.8} )
              end
          end
        },

        -- half-screen dropdowns
        { rule_any = { instance = { "musicplayer" }  },
          properties = { floating = true,
                         size_hints_honor = false,
                         ontop = true,
                         above = true,
                         skip_taskbar = true },
          callback = function(c)
              if not c.modal then
                  local name = c['instance']
                  dropdown.manage_client(name, c)
                  set_geometry(c, {vert="center", horiz="right", width=0.6, height=1.0} )
              end
          end
        },

        -- Set Firefox to always map on tags number 2 of screen 1.
        -- { rule = { class = "Firefox" },
        --   properties = { screen = 1, tag = "2" } },
    }
)

