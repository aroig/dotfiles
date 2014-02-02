---------------------------------------------------------------
-- File: globalkeys.lua     Global awesome keybindings       --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local capi =
{
    client = client,
    mouse = mouse,
    screen = screen,
}

local ipairs = ipairs
local apps = apps
local ddclient = ddclient
local box = box


local drag = {}

function drag_bydirection(dir, c)
    local c = c or client.focus
    if dir == 'up' then
        if c then
            awful.tag.viewnext()
            local t = awful.tag.selected(c.screen)
            awful.client.movetotag(t, c)
            awful.client.focus.byidx(0, c)
        end

    elseif dir == 'down' then
        if c then
            awful.tag.viewprev()
            local t = awful.tag.selected(c.screen)
            awful.client.movetotag(t, c)
            awful.client.focus.byidx(0, c)
        end

    elseif dir == 'left' then
        if c then
            awful.screen.focus_bydirection("left")
            local s = mouse.screen
            awful.client.movetoscreen(c, s)
            awful.client.focus.byidx(0, c)
        end

    elseif dir == 'right' then
        if c then
            awful.screen.focus_bydirection("right")
            local s = mouse.screen
            awful.client.movetoscreen(c, s)
            awful.client.focus.byidx(0, c)
        end
    end
end


globalkeys = awful.util.table.join(
    -- Programs
    awful.key({ modkey, ctrlkey   }, "Print",  function () exec(apps.print) end),

    -- those are forked as usual, and belong to the awesome systemd unit
    awful.key({ modkey, ctrlkey   }, "Return", function () sdrun(apps.terminal,       nil, 'termite',     true, 'apps') end),
    awful.key({ modkey, ctrlkey   }, "e",      function () sdrun(apps.editor,         nil, 'emacsclient', true, 'apps') end),
    awful.key({ modkey, ctrlkey   }, "f",      function () sdrun(apps.filemanager,    nil, 'thunar',      true, 'apps') end),

    -- those are spawned with systemd-run, and get their own unit.
    awful.key({ modkey, ctrlkey   }, "b",      function () sdrun(apps.browser,        nil, 'dwb',         true, 'apps') end),
    awful.key({ modkey, metakey   }, "b",      function () sdrun(apps.secondbrowser,  nil, 'chromium',    true, 'apps') end),

    -- Dropdown clients
    awful.key({ modkey, ctrlkey   }, "d",      function () ddclient.dict:toggle() end),
    awful.key({ modkey, ctrlkey   }, "i",      function () ddclient.calibre:toggle() end),
    awful.key({ modkey, ctrlkey   }, "m",      function () ddclient.music:toggle() end),
    awful.key({ modkey, ctrlkey   }, "w",      function () ddclient.xournal:toggle() end),

    awful.key({ modkey, ctrlkey   }, "o",      function () ddclient.orgmode:toggle() end),
    awful.key({ modkey, ctrlkey   }, "u",      function () ddclient.mail:toggle() end),
    awful.key({ modkey, ctrlkey   }, "t",      function () ddclient.chat:toggle() end),

    -- Music
    awful.key({ modkey, ctrlkey   }, "Home",      function () exec("mpc toggle") end),
    awful.key({ modkey, ctrlkey   }, "Page_Up",   function () exec("mpc prev") end),
    awful.key({ modkey, ctrlkey   }, "Page_Down", function () exec("mpc next") end),

    awful.key({ modkey, ctrlkey   }, "Insert",    function () exec("pvol +2db") end),
    awful.key({ modkey, ctrlkey   }, "Delete",    function () exec("pvol -2db") end),

    -- Desktop boxes
    awful.key({ modkey,           }, "F1",     box.calendar.toggle_calendar),
    awful.key({ modkey,           }, "F2",     box.orgtasks.toggle_todo),
    awful.key({ modkey,           }, "F3",     box.syslog.toggle_syslog),
    awful.key({ ctrlkey,          }, "F3",     box.userlog.toggle_userlog),
    awful.key({ modkey,           }, "F4",     box.naughtylog.toggle_naughtylog),

    -- Top dropdown clients
    awful.key({ modkey,           }, "F9",     function() ddclient.syslog:show() end),
    awful.key({                   }, "F9",     function() ddclient.syslog:hide() end),

    awful.key({ modkey,           }, "F10",    function() ddclient.notes:show() end),
    awful.key({                   }, "F10",    function() ddclient.notes:hide() end),

    awful.key({ modkey,           }, "F11",    function() ddclient.octave:show() end),
    awful.key({ ctrlkey           }, "F11",    function() ddclient.sage:show() end),
    awful.key({                   }, "F11",    function() ddclient.octave:hide(); ddclient.sage:hide() end),

    awful.key({ modkey,           }, "F12",    function() ddclient.terminal:show() end),
    awful.key({ ctrlkey           }, "F12",    function() ddclient.ranger:show() end),
    awful.key({                   }, "F12",    function() ddclient.terminal:hide(); ddclient.ranger:hide() end),

    -- Prompts
    awful.key({ modkey,           }, "F5",     prompt.wikipedia),
    awful.key({ modkey,           }, "F6",     prompt.mathscinet),
    awful.key({ modkey,           }, "F7",     prompt.docs),
    awful.key({ modkey,           }, "F8",     function() ddclient.document:show() end),
    awful.key({                   }, "F8",     function() ddclient.document:hide() end),

    awful.key({ modkey, ctrlkey   }, "x",      prompt.lua),
    awful.key({ modkey,           }, "x",      prompt.command),

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

    -- Screen cycling by direction
    awful.key({ modkey, ctrlkey   }, "Left",   function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "Right",  function () awful.screen.focus_bydirection("right") end),

    awful.key({ modkey, ctrlkey   }, "h",      function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "l",      function () awful.screen.focus_bydirection("right") end),

    -- Tag Cycling by direction
    awful.key({ modkey, ctrlkey   }, "Up",     function () awful.tag.viewnext() end),
    awful.key({ modkey, ctrlkey   }, "Down",   function () awful.tag.viewprev() end),

    awful.key({ modkey, ctrlkey   }, "k",      function () awful.tag.viewnext() end),
    awful.key({ modkey, ctrlkey   }, "j",      function () awful.tag.viewprev() end),

    awful.key({ modkey, metakey   }, "Up",     function () awful.tag.viewnext(awful.util.cycle(screen.count(), mouse.screen + 1)) end),
    awful.key({ modkey, metakey   }, "Down",   function () awful.tag.viewprev(awful.util.cycle(screen.count(), mouse.screen + 1)) end),

    awful.key({ modkey, metakey   }, "k",      function () awful.tag.viewnext(awful.util.cycle(screen.count(), mouse.screen + 1)) end),
    awful.key({ modkey, metakey   }, "j",      function () awful.tag.viewprev(awful.util.cycle(screen.count(), mouse.screen + 1)) end),

    awful.key({ modkey, metakey, ctrlkey  }, "Up",    function () for s = 1, screen.count() do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "Down",  function () for s = 1, screen.count() do awful.tag.viewprev(s) end end),

    awful.key({ modkey, metakey, ctrlkey  }, "k",     function () for s = 1, screen.count() do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "j",     function () for s = 1, screen.count() do awful.tag.viewprev(s) end end),

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
              end),

    -- Layout cycling
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, shiftkey  }, "space", function () awful.layout.inc(layouts, -1) end),

    -- Layout manipulation
    awful.key({ modkey,           }, "+",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "-",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, shiftkey  }, "-",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, shiftkey  }, "+",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, ctrlkey   }, "-",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, ctrlkey   }, "+",     function () awful.tag.incncol(-1)         end),

    -- System stuff
    awful.key({ metakey, ctrlkey          }, "l",     function () exec(apps.lock_cmd) end),
    awful.key({metakey, ctrlkey, shiftkey }, "a",     awesome.restart),

    awful.key({metakey, ctrlkey, shiftkey }, "q",
              function ()
                  prompt.ask_run("Quit", apps.exit_cmd)
              end),

    awful.key({metakey, ctrlkey, shiftkey }, "z",
              function ()
                  prompt.ask_run("Suspend", apps.suspend_cmd)
              end),

    awful.key({metakey, ctrlkey, shiftkey }, "h",
              function ()
                  prompt.ask_run("Poweroff", apps.poweroff_cmd)
              end),

    awful.key({metakey, ctrlkey, shiftkey }, "r",
              function ()
                  prompt.ask_run("Reboot", apps.reboot_cmd)
              end)
)


keynumber = 10
local key

-- Client move by tag number
for i = 1, keynumber do
    key = tostring(i % 10)
    globalkeys = awful.util.table.join(
        globalkeys,

        awful.key({ modkey }, key,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewonly(tags[screen][i])
                      end
		          end
        ),

        awful.key({ modkey, ctrlkey   }, key,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end
        ),

        awful.key({ modkey, shiftkey }, key,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end
        ),

        awful.key({ modkey, ctrlkey  , shiftkey }, key,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
                      end
                  end
    ))
end


-- Set keys
root.keys(globalkeys)
