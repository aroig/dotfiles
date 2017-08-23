---------------------------------------------------------------
-- File: client.lua      Client management stuff             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}


-----------------------------------
-- Useful functions              --
-----------------------------------

local function swap_to_master(c)
    local m = awful.client.getmaster()
    if c ~= nil and m ~= nil then
        c:swap(m)
    end
end

local function drag_bydirection(dir, c)
    local c = c or client.focus
    if dir == 'up' then
        if c then
            awful.tag.viewnext()
            c:move_to_tag(c.screen.selected_tag)
            awful.client.focus.byidx(0, c)
        end

    elseif dir == 'down' then
        if c then
            awful.tag.viewprev()
            c:move_to_tag(c.screen.selected_tag)
            awful.client.focus.byidx(0, c)
        end

    elseif dir == 'left' then
        if c then
            awful.screen.focus_bydirection("left")
            c:move_to_screen(awful.screen.focused())
            awful.client.focus.byidx(0, c)
        end

    elseif dir == 'right' then
        if c then
            awful.screen.focus_bydirection("right")
            c:move_to_screen(awful.screen.focused())
            awful.client.focus.byidx(0, c)
        end
    end
end


-----------------------------------
-- Global bindings               --
-----------------------------------

globalkeys = gears.table.join(
    globalkeys,

    -- Client cycling by direction
    awful.key({ modkey,           }, "Up",     function () awful.client.focus.global_bydirection("up") end),
    awful.key({ modkey,           }, "Down",   function () awful.client.focus.global_bydirection("down") end),
    awful.key({ modkey,           }, "Left",   function () awful.client.focus.global_bydirection("left") end),
    awful.key({ modkey,           }, "Right",  function () awful.client.focus.global_bydirection("right") end),

    awful.key({ modkey,           }, "k",      function () awful.client.focus.global_bydirection("up") end),
    awful.key({ modkey,           }, "j",      function () awful.client.focus.global_bydirection("down") end),
    awful.key({ modkey,           }, "h",      function () awful.client.focus.global_bydirection("left") end),
    awful.key({ modkey,           }, "l",      function () awful.client.focus.global_bydirection("right") end),

    -- Client swapping by direction
    awful.key({ modkey, shiftkey  }, "Up",     function () awful.client.swap.global_bydirection("up") end),
    awful.key({ modkey, shiftkey  }, "Down",   function () awful.client.swap.global_bydirection("down") end),
    awful.key({ modkey, shiftkey  }, "Left",   function () awful.client.swap.global_bydirection("left") end),
    awful.key({ modkey, shiftkey  }, "Right",  function () awful.client.swap.global_bydirection("right") end),

    awful.key({ modkey, shiftkey  }, "k",      function () awful.client.swap.global_bydirection("up") end),
    awful.key({ modkey, shiftkey  }, "j",      function () awful.client.swap.global_bydirection("down") end),
    awful.key({ modkey, shiftkey  }, "h",      function () awful.client.swap.global_bydirection("left") end),
    awful.key({ modkey, shiftkey  }, "l",      function () awful.client.swap.global_bydirection("right") end),

    -- Client dragging
    awful.key({ modkey, ctrlkey, shiftkey }, "Up",    function () drag_bydirection("up") end),
    awful.key({ modkey, ctrlkey, shiftkey }, "Down",  function () drag_bydirection("down") end),
    awful.key({ modkey, ctrlkey, shiftkey }, "Left",  function () drag_bydirection("left") end),
    awful.key({ modkey, ctrlkey, shiftkey }, "Right", function () drag_bydirection("right") end),

    awful.key({ modkey, ctrlkey, shiftkey }, "k",     function () drag_bydirection("up") end),
    awful.key({ modkey, ctrlkey, shiftkey }, "j",     function () drag_bydirection("down") end),
    awful.key({ modkey, ctrlkey, shiftkey }, "h",     function () drag_bydirection("left") end),
    awful.key({ modkey, ctrlkey, shiftkey }, "l",     function () drag_bydirection("right") end),

    -- Other client stuff
    awful.key({ modkey, ctrlkey   }, "n",
              function ()
                  c = awful.client.restore()
                  if c then
                      awful.client.focus.byidx(0, c)
                  end
              end),

    awful.key({ modkey,           }, "u",   function () awful.client.urgent.jumpto() end),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),


    -- Client cycling by index
    awful.key({ modkey,           }, "Tab", function () awful.client.focus.byidx( 1) end),
    awful.key({ modkey, shiftkey  }, "Tab", function () awful.client.focus.byidx(-1) end),

    -- Client cycling by history
    awful.key({ modkey, ctrlkey   }, "Tab",
              function ()
                  awful.client.focus.history.previous()
                  if client.focus then client.focus:raise() end
              end)
)



-----------------------------------
-- Client bindings and buttons   --
-----------------------------------

clientkeys = gears.table.join(
    awful.key({ modkey }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end),

    awful.key({ modkey, shiftkey  }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, ctrlkey   }, "space",  function (c) awful.client.floating.toggle()   end),
    awful.key({ modkey,           }, "t",      function (c) awful.client.floating.toggle()   end),
    awful.key({ modkey,           }, "Return", function (c) swap_to_master(c)                end),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end),
    awful.key({ modkey, shiftkey  }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "r",      function (c) c:raise()                        end),
    -- awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",      function (c) c.minimized = true               end),
    awful.key({ modkey,           }, "m",
              function (c)
                  c.maximized = not c.maximized
                  c:raise()
              end
    )
)

-- Client buttons
clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))


-- Attach bindings to clients
clientrules = gears.table.join(
    clientrules,

    {
        { rule = { },
          properties = { keys = clientkeys,
                         buttons = clientbuttons } },
    }
)
