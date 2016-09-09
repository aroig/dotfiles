---------------------------------------------------------------
-- File:    mywibox.lua     Wibox stuff                      --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local wibox = wibox
local gears = gears

local host_config = host_config


-----------------------------------
-- Wibox layout                  --
-----------------------------------

mywibox = {}

-- Create a wibox for each screen and add it
awful.screen.connect_for_each_screen(function(s)
   -- Create the wibox
   mywibox[s] = awful.wibar({ position = "top", screen = s, height=host_config.wibox_height })
   -- Add widgets to the wibox - order matters


   -- Left widgets
   local left_layout = wibox.layout.fixed.horizontal()

   left_layout:add(myw.layoutbox[s])
   left_layout:add(myw.taglist[s])

   left_layout:add(myw.spacer)

   left_layout:add(myw.promptbox[s])


   -- Right widgets
   local right_layout = wibox.layout.fixed.horizontal()

   right_layout:add(myw.sys.syncwdg)
   right_layout:add(myw.sys.privwdg)

   right_layout:add(myw.separator)

   right_layout:add(myw.hdw.cpuicon)
   right_layout:add(myw.hdw.cpuwdg)

   right_layout:add(myw.spacer)

   right_layout:add(myw.hdw.tempicon)
   right_layout:add(myw.hdw.tempwdg)

   right_layout:add(myw.spacer)

   right_layout:add(myw.hdw.memicon)
   right_layout:add(myw.hdw.memwdg)

   right_layout:add(myw.separator)

   right_layout:add(myw.net.icon)
   right_layout:add(myw.net.dwdg)
   right_layout:add(myw.slash)
   right_layout:add(myw.net.uwdg)

   right_layout:add(myw.spacer)

   right_layout:add(myw.mail.icon)
   right_layout:add(myw.mail.inwdg)
   right_layout:add(myw.slash)
   right_layout:add(myw.mail.outwdg)

   right_layout:add(myw.separator)

   if util.file_exists("/sys/class/power_supply/BAT0/status") then
      right_layout:add(myw.bat.pcicon)
      right_layout:add(myw.bat.pcwidget)
      right_layout:add(myw.spacer)
      right_layout:add(myw.bat.rticon)
      right_layout:add(myw.bat.rtwidget)
      right_layout:add(myw.separator)
   end

   right_layout:add(myw.mpd.icon)
   right_layout:add(myw.mpd.stateicon)
   right_layout:add(myw.spacer)

   right_layout:add(myw.vol.icon)
   right_layout:add(myw.vol.widget)
   right_layout:add(myw.separator)

   right_layout:add(myw.chat.icon)

   if s == 1 then
      right_layout:add(myw.systray)
      right_layout:add(myw.separator)
   end

   right_layout:add(myw.keyb.icon)
   right_layout:add(myw.keyb.keybwdg)
   right_layout:add(myw.separator)

   right_layout:add(myw.clock.icon)
   right_layout:add(myw.clock.clockwdg)

   local middle_layout = wibox.layout.fixed.horizontal()
   middle_layout:add(myw.tasklist[s])

   -- Put it all together
   local layout = wibox.layout.align.horizontal(left_layout, middle_layout, right_layout)
   mywibox[s]:set_widget(layout)
end)
