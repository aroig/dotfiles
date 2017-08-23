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

local awful = awful
local ipairs = ipairs
local apps = apps
local box = box

local drag = {}

function drag_bydirection(dir, c)
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


globalkeys = gears.table.join(
    globalkeys,

    -- Machine state
    awful.key({ modkey,           }, "v",      function() switch.output_mode(1)      end),
    awful.key({ modkey, shiftkey  }, "v",      function() switch.output_mode(-1)     end),

    awful.key({ modkey,           }, "b",      function() switch.orientation_mode(1)      end),
    awful.key({ modkey, shiftkey  }, "b",      function() switch.orientation_mode(-1)     end),

    -- Applications started from instantiated units
    awful.key({ modkey, ctrlkey   }, "Return", function () run('app:termite')         end),
    awful.key({ modkey, ctrlkey   }, "f",      function () run('app:vifm')            end),
    awful.key({ modkey            }, "z",      myw.keyb.keybwdg.next_layout              ),

    awful.key({ modkey, ctrlkey   }, "b",      function () run('app:chromium')        end),
    awful.key({ modkey, metakey   }, "b",      function () run('app:firefox')         end),
    awful.key({ modkey, ctrlkey   }, "e",      function () run('app:emacsclient')     end),

    awful.key({ modkey, ctrlkey   }, "w",      function () run('app:xournal')         end),

    -- Dropdown clients
    awful.key({ modkey, ctrlkey   }, "i",      function () ddtoggle("app:calibre",     true) end),
    awful.key({ modkey, ctrlkey   }, "m",      function () ddtoggle("app:musicplayer", true) end),
    awful.key({ modkey, ctrlkey   }, "o",      function () ddtoggle('app:org',         true) end),
    awful.key({ modkey, ctrlkey   }, "u",      function () ddtoggle('app:mu4e',        true) end),
    awful.key({ modkey, ctrlkey   }, "t",      function () ddtoggle('app:chat',        true) end),
    awful.key({ modkey, ctrlkey   }, "g",      function () ddtoggle('app:glances',     true) end),

    -- Desktop boxes
    awful.key({ modkey,           }, "F1",     hotkeys.widget.show_help),
    awful.key({ modkey,           }, "F2",     box.calendar.toggle_calendar),
    awful.key({ modkey,           }, "F3",     box.orgtasks.toggle_todo),
    awful.key({ modkey,           }, "F4",     box.naughtylog.toggle_naughtylog),

    -- Top dropdown clients
    awful.key({ modkey   }, "F12",             function() ddshow_last()              end),
    awful.key({          }, "F12",             function() ddhide_last()              end),
    awful.key({ shiftkey }, "F12",             function() ddhide_all()               end),
    awful.key({ modkey   }, "XF86MyComputer",  function() ddshow_last()              end),
    awful.key({          }, "XF86MyComputer",  function() ddhide_last()              end),
    awful.key({ shiftkey }, "XF86MyComputer",  function() ddshow_all()               end),


    awful.key({ modkey, metakey   }, "Return", function () ddtoggle('app:termite-dropdown', true) end),
    awful.key({ modkey, metakey   }, "f",      function () ddtoggle('app:vifm-dropdown',    true) end),

    awful.key({ modkey   }, "F9",              function() ddshow("app:journal-dropdown",    true) end),
    awful.key({          }, "F9",              function() ddhide("app:journal-dropdown")          end),
    awful.key({ modkey   }, "XF86Tools",       function() ddshow("app:journal-dropdown",    true) end),
    awful.key({          }, "XF86Tools",       function() ddhide("app:journal-dropdown")          end),

    -- Prompts
    awful.key({ modkey   }, "w",               prompt.wikipedia),
    awful.key({ modkey   }, "e",               prompt.mathscinet),

    -- TODO: change binding
    -- awful.key({ modkey   }, "z",               prompt.docs),
    awful.key({ modkey   }, "a",               prompt.lua),
    awful.key({ modkey   }, "s",               prompt.systemd),
    awful.key({ modkey   }, "x",               prompt.command),
    awful.key({ modkey   }, "d",               prompt.dropdown),

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

    -- System stuff
    awful.key({ metakey, ctrlkey          }, "l",   function () run('app:lock')                        end),
    awful.key({metakey, ctrlkey, shiftkey }, "a",   function () awesome.restart()                      end),
    awful.key({metakey, ctrlkey, shiftkey }, "q",   function () run('app:quit',     { ask=true })      end),
    awful.key({metakey, ctrlkey, shiftkey }, "z",   function () run('app:suspend',  { ask=true })      end),
    awful.key({metakey, ctrlkey, shiftkey }, "h",   function () run('app:poweroff', { ask=true })      end),
    awful.key({metakey, ctrlkey, shiftkey }, "r",   function () run('app:reboot',   { ask=true })      end),

    -- Desktop stuff
    awful.key({ modkey, ctrlkey },   "Print",     function () shexec(apps.print)                       end),

    -- Brightness
    awful.key({}, "XF86MonBrightnessUp",          function () shexec("xbacklight -inc 5%")             end),
    awful.key({}, "XF86MonBrightnessDown",        function () shexec("xbacklight -dec 5%")             end),

    -- Music
    awful.key({ modkey, ctrlkey   }, "Home",      function () shexec("mpc -q toggle")                  end),
    awful.key({ modkey, ctrlkey   }, "Page_Up",   function () shexec("mpc -q prev")                    end),
    awful.key({ modkey, ctrlkey   }, "Page_Down", function () shexec("mpc -q next")                    end),

    awful.key({}, "XF86AudioRaiseVolume",         function () shexec("avol up")                        end),
    awful.key({}, "XF86AudioLowerVolume",         function () shexec("avol down")                      end),

    -- TODO: mute stuff
    awful.key({}, "XF86AudioMute",                function () shexec("pvol mute-sink")                 end),
    awful.key({}, "XF86AudioMicMute",             function () shexec("pvol mute-source")               end),

    awful.key({ modkey, ctrlkey   }, "Insert",    function () shexec("avol up")                        end),
    awful.key({ modkey, ctrlkey   }, "End",       function () shexec("avol up")                        end), -- for galois
    awful.key({ modkey, ctrlkey   }, "Delete",    function () shexec("avol down")                      end)
)


