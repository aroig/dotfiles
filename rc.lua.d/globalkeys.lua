---------------------------------------------------------------
-- File: globalkeys.lua     Global awesome keybindings       --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

pickle = require("abdo.pickle")                -- Some crude pickling routines



local capi =
{
    client = client,
    mouse = mouse,
    screen = screen,
}
local ipairs = ipairs

--- Restore (=unminimize) a random client, and set focus to it.
-- @param s The screen to use.
-- @return True if some client was restored.

local function restore_focus(s)
   local s = s or (capi.client.focus and capi.client.focus.screen) or capi.mouse.screen
   local cls = capi.client.get(s)
   local tags = awful.tag.selectedlist(s)
   local mcls = {}
   for k, c in pairs(cls) do
      local ctags = c:tags()
      if c.minimized then
         for k, t in ipairs(tags) do
            if awful.util.table.hasitem(ctags, t) then
               c.minimized = false
               awful.client.focus.byidx(0, c)
               return true
            end
         end
      end
   end
   return false
end




globalkeys = awful.util.table.join(
    -- Programs
    -- I do the sleep thing because otherwise urxvt starts too fast and does not get focus.
    awful.key({ modkey, "Control" }, "Return", function () shexec("sleep 0.1; " .. apps.terminal) end),
    awful.key({ modkey, "Control" }, "f",      function () exec(apps.filemanager) end),
    awful.key({ modkey, "Control" }, "g",      function () exec(apps.filemanager_alt) end),
    awful.key({ modkey, "Control" }, "e",      function () exec(apps.editor) end),
    awful.key({ modkey, metakey   }, "b",      function () exec(apps.browser) end),
    awful.key({ modkey, "Control" }, "b",      function () exec("chromium") end),
    awful.key({ modkey, "Control" }, "k",      function () exec("keepassx") end),
    awful.key({ modkey, "Control" }, "o",      function () exec("emacs -org") end),
    awful.key({ modkey, "Control" }, "u",      function () exec("emacs -mail") end),
--    awful.key({ modkey, "Control" }, "r",      function () exec("emacs -news") end),
--    awful.key({ modkey, "Control" }, "h",      function () exec("calibre") end),
--    awful.key({ modkey, "Control" }, "m",      function () exec("quodlibet") end),


    awful.key({ modkey, "Control" }, "d",      function () drop.toggle("goldendict", "goldendict",
								"center", "right", 0.5, 1) end),

    awful.key({ modkey, "Control" }, "h",      function () drop.toggle("calibre", "calibre",
							     "center", "right", 1, 1) end),

    awful.key({ modkey, "Control" }, "p",      function () drop.toggle("pidgin", "pidgin",
								"center", "right", 300, 1) end),

    awful.key({ modkey, "Control" }, "m",      function () drop.toggle("quodlibet", "quodlibet",
								"center", "right", 0.6, 1) end),

    -- Music
    awful.key({ modkey, "Control" }, "Home",      function () exec("quodlibet --play-pause") end),
    awful.key({ modkey, "Control" }, "Page_Up",   function () exec("quodlibet --previous") end),
    awful.key({ modkey, "Control" }, "Page_Down", function () exec("quodlibet --next") end),

    awful.key({ modkey, "Control" }, "Insert",    function () exec("pvol +2db") end),
    awful.key({ modkey, "Control" }, "Delete",    function () exec("pvol -2db") end),

    -- Desktop
    awful.key({ metakey, "Control"}, "l",      function () exec("slimlock") end),
    awful.key({ modkey,           }, "Print",  function () exec("scrot -s -e 'mv $f ~/Downloads/'") end),
    awful.key({ modkey,           }, "t",      awful.client.floating.toggle),

    awful.key({ modkey,           }, "F1",     calendar.toggle_calendar),
    awful.key({ modkey,           }, "F2",     orgtasks.toggle_todo),
    awful.key({ modkey,           }, "F3",     syslog.toggle_syslog),
    awful.key({ modkey,           }, "F4",     naughtylog.toggle_naughtylog),

    awful.key({ modkey,           }, "F9",     function () drop.toggle("desktop", apps.light_filemanager .. " Desktop",
								"top", "center", 1, 0.4) end),
    awful.key({ modkey, "Control" }, "F9",     function () drop.toggle("ranger", apps.terminal .. " -e ranger",
								"top", "center", 1, 0.4) end),

    awful.key({ modkey,           }, "F10",    function () drop.toggle("notes", apps.notes,
                                                                "top", "center", 1, 0.4) end),

    awful.key({ modkey,           }, "F11",    function () drop.toggle("octave", apps.terminal .. " -e octave",
								"top", "center", 1, 0.4) end),
    awful.key({ modkey, "Control" }, "F11",    function () drop.toggle("sage", apps.terminal .. " -e sage",
								"top", "center", 1, 0.4) end),

    awful.key({ modkey,           }, "F12",    function () drop.toggle("terminal", apps.terminal,
								"top", "center", 1, 0.4) end),


    awful.key({ modkey,           }, "F8",     function () drop.toggle("doc", nil,
								"center", "right", 0.7, 1) end),
    awful.key({ modkey,           }, "F7",     prompt.docs),
    awful.key({ modkey,           }, "F6",     prompt.wikipedia),


    -- Client cycling
    awful.key({ modkey,           }, "Up",     function () awful.client.focus.global_bydirection("up") end),
    awful.key({ modkey,           }, "Down",   function () awful.client.focus.global_bydirection("down") end),
    awful.key({ modkey,           }, "Left",   function () awful.client.focus.global_bydirection("left") end),
    awful.key({ modkey,           }, "Right",  function () awful.client.focus.global_bydirection("right") end),

    awful.key({ modkey, "Shift"   }, "Up",     function () awful.client.swap.global_bydirection("up") end),
    awful.key({ modkey, "Shift"   }, "Down",   function () awful.client.swap.global_bydirection("down") end),
    awful.key({ modkey, "Shift"   }, "Left",   function () awful.client.swap.global_bydirection("left") end),
    awful.key({ modkey, "Shift"   }, "Right",  function () awful.client.swap.global_bydirection("right") end),

    -- Screen cycling
    awful.key({ modkey, "Control" }, "Left",   function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, "Control" }, "Right",  function () awful.screen.focus_bydirection("right") end),

    -- Tag Cycling
    awful.key({ modkey, "Control" }, "Up",     function () awful.tag.viewnext() end),
    awful.key({ modkey, "Control" }, "Down",   function () awful.tag.viewprev() end),

    awful.key({ modkey, metakey   }, "Up",     function () awful.tag.viewnext(awful.util.cycle(screen.count(), mouse.screen + 1)) end),
    awful.key({ modkey, metakey   }, "Down",   function () awful.tag.viewprev(awful.util.cycle(screen.count(), mouse.screen + 1)) end),

    awful.key({ modkey, metakey, "Control"}, "Up",   function () for s = 1, screen.count() do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, "Control"}, "Down", function () for s = 1, screen.count() do awful.tag.viewprev(s) end end),

--    awful.key({ modkey            }, "e",    revelation.expose),


    -- Client dragging
    awful.key({ modkey, "Shift", "Control"}, "Up",
              function ()
                  local c = client.focus
                  if c then
                      awful.tag.viewnext()
                      local t = awful.tag.selected(c.screen)
                      awful.client.movetotag(t, c)
                      awful.client.focus.byidx(0, c)
                  end
              end),

    awful.key({ modkey, "Shift", "Control"}, "Down",
              function ()
                  local c = client.focus
                  if c then
                      awful.tag.viewprev()
                      local t = awful.tag.selected(c.screen)
                      awful.client.movetotag(t, c)
                      awful.client.focus.byidx(0, c)
                  end
              end),


    awful.key({ modkey, "Shift", "Control"}, "Left",
              function ()
                  local c = client.focus
                  if c then
                      awful.screen.focus_bydirection("left")
                      local s = mouse.screen
                      awful.client.movetoscreen(c, s)
                      awful.client.focus.byidx(0, c)
                  end
              end),

    awful.key({ modkey, "Shift", "Control"}, "Right",
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
    awful.key({metakey, "Shift", "Control"}, "a",     function () awesome.restart() end),
    awful.key({metakey, "Shift", "Control"}, "q",     awesome.quit),

    -- System stuff
    awful.key({metakey, "Control", "Shift" }, "z",     function () exec("systemctl suspend") end),
    awful.key({metakey, "Control", "Shift" }, "h",     function () exec("systemctl poweroff") end),
    awful.key({metakey, "Control", "Shift" }, "r",     function () exec("systemctl reboot") end),

    -- Awesome defaults
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
--    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Prompt
    awful.key({ modkey, "Control" }, "x",     prompt.lua),
    awful.key({ modkey,           }, "x",     prompt.command),

    -- Standard awesome stuff
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),

    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  c = awful.client.restore()
                  if c then
                      awful.client.focus.byidx(0, c)
                  end
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
      awful.key({ modkey, "Control" }, key,
		function ()
		   local screen = mouse.screen
		   if tags[screen][i] then awful.tag.viewtoggle(tags[screen][i]) end
		end),
      awful.key({ modkey, "Shift" }, key,
		function ()
		   if client.focus and tags[client.focus.screen][i] then
		      awful.client.movetotag(tags[client.focus.screen][i])
		   end
		end),
      awful.key({ modkey, "Control", "Shift" }, key,
		function ()
		   if client.focus and tags[client.focus.screen][i] then
		      awful.client.toggletag(tags[client.focus.screen][i])
		   end
		end))
end


-- Set keys
root.keys(globalkeys)
