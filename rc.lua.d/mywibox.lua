---------------------------------------------------------------
-- File:    mywibox.lua     Wibox stuff                      --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local wibox   = require("wibox")


-----------------------------------
-- Convenience stuff             --
-----------------------------------

local scount = screen.count()

local gradcols = {beautiful.fg_grad1_widget, beautiful.fg_grad2_widget,
		  beautiful.fg_grad3_widget, beautiful.fg_grad4_widget}

local gradcols_rev = {beautiful.fg_grad4_widget, beautiful.fg_grad3_widget,
		      beautiful.fg_grad2_widget, beautiful.fg_grad1_widget}




-----------------------------------
-- Wibox layout                  --
-----------------------------------

mywibox = {}

-- Create a wibox for each screen and add it
for s = 1, screen.count() do
   -- Create the wibox
   mywibox[s] = awful.wibox({ position = "top", screen = s, height=16 })
   -- Add widgets to the wibox - order matters


   local layout = wibox.layout.align.horizontal()

   -- Left widgets
   local left_layout = wibox.layout.fixed.horizontal()

   left_layout:add(myw.layoutbox[s])
   left_layout:add(myw.taglist[s])
   left_layout:add(myw.spacer)
   left_layout:add(myw.promptbox[s])


   -- Right widgets
   local right_layout = wibox.layout.fixed.horizontal()

   right_layout:add(myw.separator)

   right_layout:add(myw.cpu.icon)
   right_layout:add(myw.cpu.text)
   right_layout:add(myw.spacer)

   right_layout:add(myw.temp.text)
   right_layout:add(myw.spacer)

   right_layout:add(myw.mem.icon)
   right_layout:add(myw.mem.text)

   right_layout:add(myw.separator)

   right_layout:add(myw.net.dnicon)
   right_layout:add(myw.net.text)
   right_layout:add(myw.net.upicon)
   right_layout:add(myw.spacer)

   right_layout:add(myw.mail.icon)
   right_layout:add(myw.mail.text)

   right_layout:add(myw.separator)

   if util.file_exists("/sys/class/power_supply/BAT0/status") then
      right_layout:add(myw.bat.icon)
      right_layout:add(myw.bat.text)
      right_layout:add(myw.spacer)
      right_layout:add(myw.bat.rate)
      right_layout:add(myw.separator)
   end

   right_layout:add(myw.mpd.icon)
   right_layout:add(myw.mpd.stateicon)
   right_layout:add(myw.mpd.text)
   right_layout:add(myw.spacer)

   right_layout:add(myw.vol.icon)
   right_layout:add(myw.vol.text)
   right_layout:add(myw.separator)

   if s == 1 then
      right_layout:add(myw.spacer)
      right_layout:add(myw.systray)
      right_layout:add(myw.spacer)
   end

   right_layout:add(myw.textclock)

   local middle_layout = wibox.layout.fixed.horizontal()
   middle_layout:add(myw.tasklist[s])

   -- Put it all together
   layout:set_left(left_layout)
   layout:set_middle(middle_layout)
   layout:set_right(right_layout)

   mywibox[s]:set_widget(layout)
end
