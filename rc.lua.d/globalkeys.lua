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

globalkeys = awful.util.table.join(
    -- Programs
    -- I do the sleep thing because otherwise urxvt starts too fast and does not get focus.
    awful.key({ modkey, ctrlkey   }, "Return", function () exec(apps.terminal) end),
    awful.key({ modkey, ctrlkey   }, "f",      function () exec(apps.filemanager) end),
    awful.key({ modkey, ctrlkey   }, "e",      function () exec(apps.editor) end),
    awful.key({ modkey, ctrlkey   }, "b",      function () exec(apps.browser) end),
    awful.key({ modkey, ctrlkey   }, "o",      function () exec(apps.orgmode) end),
    awful.key({ modkey, ctrlkey   }, "u",      function () exec(apps.mail) end),
    awful.key({ modkey, ctrlkey   }, "k",      function () exec(apps.passwordsafe) end),
--    awful.key({ modkey, ctrlkey   }, "r",      function () exec(apps.news) end),

    -- Right dropdown clients
    awful.key({ modkey, ctrlkey   }, "d",      function () ddclient.dict:toggle() end),
    awful.key({ modkey, ctrlkey   }, "h",      function () ddclient.calibre:toggle() end),
    awful.key({ modkey, ctrlkey   }, "i",      function () ddclient.chat:toggle() end),
    awful.key({ modkey, ctrlkey   }, "m",      function () ddclient.music:toggle() end),
    awful.key({ modkey, ctrlkey   }, "t",      function () ddclient.twitter:toggle() end),

    -- Music
    awful.key({ modkey, ctrlkey   }, "Home",      function () exec("mpc toggle") end),
    awful.key({ modkey, ctrlkey   }, "Page_Up",   function () exec("mpc prev") end),
    awful.key({ modkey, ctrlkey   }, "Page_Down", function () exec("mpc next") end),

    awful.key({ modkey, ctrlkey   }, "Insert",    function () exec("pvol +2db") end),
    awful.key({ modkey, ctrlkey   }, "Delete",    function () exec("pvol -2db") end),

    -- Desktop
    awful.key({ metakey, ctrlkey  }, "l",      function () exec("lock") end),
    awful.key({ modkey,           }, "Print",  function ()
                                                   exec("scrot -e 'mv $f ~/down/'")
                                                   naughty.notify({title = "Screenshot",
                                                                   text = "Saved in ~/down"})
                                               end),
    awful.key({ modkey,           }, "t",      awful.client.floating.toggle),

    awful.key({ modkey,           }, "F1",     calendar.toggle_calendar),
    awful.key({ modkey,           }, "F2",     orgtasks.toggle_todo),
    awful.key({ modkey,           }, "F3",     syslog.toggle_syslog),
    awful.key({ modkey,           }, "F4",     naughtylog.toggle_naughtylog),

    -- Top dropdown clients
    awful.key({ modkey,           }, "F10",    function() ddclient.notes:show() end),
    awful.key({                   }, "F10",    function() ddclient.notes:hide() end),

    awful.key({ modkey,           }, "F11",    function() ddclient.octave:show() end),
    awful.key({ ctrlkey           }, "F11",    function() ddclient.sage:show() end),
    awful.key({                   }, "F11",
              function()
                  ddclient.octave:hide()
                  ddclient.sage:hide()
              end),

    awful.key({ modkey,           }, "F12",    function() ddclient.terminal:show() end),
    awful.key({ ctrlkey           }, "F12",    function() ddclient.ranger:show() end),
    awful.key({                   }, "F12",
              function()
                  ddclient.terminal:hide()
                  ddclient.ranger:hide()
              end),

    awful.key({ modkey,           }, "F5",     prompt.wikipedia),
    awful.key({ modkey,           }, "F6",     prompt.mathscinet),
    awful.key({ modkey,           }, "F7",     prompt.docs),
    awful.key({ modkey,           }, "F8",     function() ddclient.document:show() end),
    awful.key({                   }, "F8",     function() ddclient.document:hide() end),


    -- Client cycling
    awful.key({ modkey,           }, "Up",     function () awful.client.focus.global_bydirection("up") end),
    awful.key({ modkey,           }, "Down",   function () awful.client.focus.global_bydirection("down") end),
    awful.key({ modkey,           }, "Left",   function () awful.client.focus.global_bydirection("left") end),
    awful.key({ modkey,           }, "Right",  function () awful.client.focus.global_bydirection("right") end),

    awful.key({ modkey, shiftkey  }, "Up",     function () awful.client.swap.global_bydirection("up") end),
    awful.key({ modkey, shiftkey  }, "Down",   function () awful.client.swap.global_bydirection("down") end),
    awful.key({ modkey, shiftkey  }, "Left",   function () awful.client.swap.global_bydirection("left") end),
    awful.key({ modkey, shiftkey  }, "Right",  function () awful.client.swap.global_bydirection("right") end),

    -- Screen cycling
    awful.key({ modkey, ctrlkey   }, "Left",   function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "Right",  function () awful.screen.focus_bydirection("right") end),

    -- Tag Cycling
    awful.key({ modkey, ctrlkey   }, "Up",     function () awful.tag.viewnext() end),
    awful.key({ modkey, ctrlkey   }, "Down",   function () awful.tag.viewprev() end),

    awful.key({ modkey, metakey   }, "Up",     function () awful.tag.viewnext(awful.util.cycle(screen.count(), mouse.screen + 1)) end),
    awful.key({ modkey, metakey   }, "Down",   function () awful.tag.viewprev(awful.util.cycle(screen.count(), mouse.screen + 1)) end),

    awful.key({ modkey, metakey, ctrlkey  }, "Up",   function () for s = 1, screen.count() do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "Down", function () for s = 1, screen.count() do awful.tag.viewprev(s) end end),

--    awful.key({ modkey            }, "e",    revelation.expose),


    -- Client dragging
    awful.key({ modkey, ctrlkey, shiftkey }, "Up",
              function ()
                  local c = client.focus
                  if c then
                      awful.tag.viewnext()
                      local t = awful.tag.selected(c.screen)
                      awful.client.movetotag(t, c)
                      awful.client.focus.byidx(0, c)
                  end
              end),

    awful.key({ modkey, ctrlkey, shiftkey }, "Down",
              function ()
                  local c = client.focus
                  if c then
                      awful.tag.viewprev()
                      local t = awful.tag.selected(c.screen)
                      awful.client.movetotag(t, c)
                      awful.client.focus.byidx(0, c)
                  end
              end),


    awful.key({ modkey, ctrlkey, shiftkey }, "Left",
              function ()
                  local c = client.focus
                  if c then
                      awful.screen.focus_bydirection("left")
                      local s = mouse.screen
                      awful.client.movetoscreen(c, s)
                      awful.client.focus.byidx(0, c)
                  end
              end),

    awful.key({ modkey, ctrlkey, shiftkey }, "Right",
              function ()
                  local c = client.focus
                  if c then
                      awful.screen.focus_bydirection("right")
                      local s = mouse.screen
                      awful.client.movetoscreen(c, s)
                      awful.client.focus.byidx(0, c)
                  end
              end),



    -- Awesome stuff
    awful.key({metakey, ctrlkey, shiftkey }, "a",     function () awesome.restart() end),
    awful.key({metakey, ctrlkey, shiftkey }, "q",     awesome.quit),

    -- System stuff
    awful.key({metakey, ctrlkey, shiftkey }, "z",     function () exec("sudo systemctl suspend") end),
    awful.key({metakey, ctrlkey, shiftkey }, "h",     function () exec("sudo systemctl poweroff") end),
    awful.key({metakey, ctrlkey, shiftkey }, "r",     function () exec("sudo systemctl reboot") end),

    -- Awesome defaults
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",   function () awful.client.focus.byidx( 1) end),
    awful.key({ modkey,           }, "k",   function () awful.client.focus.byidx(-1) end),

--    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, shiftkey  }, "j",   function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, shiftkey  }, "k",   function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, ctrlkey   }, "j",   function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, ctrlkey   }, "k",   function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u",   awful.client.urgent.jumpto),

    awful.key({ modkey,           }, "Tab", function () awful.client.focus.byidx( 1) end),
    awful.key({ modkey, shiftkey  }, "Tab", function () awful.client.focus.byidx(-1) end),
    awful.key({ modkey,           }, "r",   function () if client.focus then client.focus:raise() end end),

    awful.key({ modkey, ctrlkey   }, "Tab",
              function ()
                  awful.client.focus.history.previous()
                  if client.focus then client.focus:raise() end
              end),

    -- Prompt
    awful.key({ modkey, ctrlkey   }, "x",     prompt.lua),
    awful.key({ modkey,           }, "x",     prompt.command),

    -- Standard awesome stuff
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, shiftkey  }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, shiftkey  }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, ctrlkey   }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, ctrlkey   }, "l",     function () awful.tag.incncol(-1)         end),

    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, shiftkey  }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, ctrlkey   }, "n",
              function ()
                  c = awful.client.restore()
                  if c then awful.client.focus.byidx(0, c) end
              end)
)


keynumber = 10
local key

for i = 1, keynumber do
   key = tostring(i % 10)
   globalkeys = awful.util.table.join(
      globalkeys,
      awful.key({ modkey }, key,
		function ()
		   local screen = mouse.screen
		   if tags[screen][i] then awful.tag.viewonly(tags[screen][i]) end
		end),
      awful.key({ modkey, ctrlkey   }, key,
		function ()
		   local screen = mouse.screen
		   if tags[screen][i] then awful.tag.viewtoggle(tags[screen][i]) end
		end),
      awful.key({ modkey, shiftkey }, key,
		function ()
		   if client.focus and tags[client.focus.screen][i] then
		      awful.client.movetotag(tags[client.focus.screen][i])
		   end
		end),
      awful.key({ modkey, ctrlkey  , shiftkey }, key,
		function ()
		   if client.focus and tags[client.focus.screen][i] then
		      awful.client.toggletag(tags[client.focus.screen][i])
		   end
		end))
end


-- Set keys
root.keys(globalkeys)
