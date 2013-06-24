-- Calendar

local pairs = pairs
local ipairs = ipairs
local io = io
local os = os
local tonumber = tonumber
local string = string
local table = table
local beautiful = beautiful
local util = awful.util

local char_width = 7.3
local offset = 0

local calendar = {}
calendar.cal = nil


local function generate_calendar(offset)
   local text_color = beautiful.fg_normal or "#FFFFFF"
   local today_color = beautiful.fg_urgent or "#00FF00"
   local font = beautiful.font_box

   local query = os.date("%Y-%m-%d")
   local _, _, cur_year, cur_month, cur_day = string.find(query,"(%d%d%d%d)%-(%d%d)%-(%d%d)")
   cur_month = tonumber(cur_month) + offset
   if cur_month > 12 then
      cur_month = (cur_month % 12) .. "f"
      cur_year = cur_year + 1
   elseif cur_month < 1 then
      cur_month = (cur_month + 12) .. "p"
      cur_year = cur_year - 1
   end
   local cal = util.pread("LANG=C; cal -m " .. cur_month .. " " .. cur_year)

--   cal = string.gsub(cal, "^%s*(.-)%s*$", "%1")
--   local _, _, head, cal = string.find(cal,"(.+%d%d%d%d)%s*\n(.+)")

   if string.sub(cur_day,1,1) == "0" then
      cur_day = string.sub(cur_day,2)
   end
   if offset == 0 then
      cal = string.gsub(cal, "(" .. cur_day .."[%s/%lt;])",
                        '<span weight="bold" foreground="'.. today_color ..'">%1</span>', 1)
   end

--   cal = head .. "\n" .. cal
--   cal = "\n" .. cal

   cal = string.format('<span font="%s">\n%s</span>', font, cal)
   return cal
end


function calendar.add_calendar(inc_offset)
   local save_offset = offset
   calendar.remove_calendar()
   offset = save_offset + inc_offset
   local data = generate_calendar(offset - 1) .. generate_calendar(offset) .. generate_calendar(offset + 1)
   calendar.cal = naughty.notify({ title = "Calendar",
				   text = data,
				   timeout = 0})
end

function calendar.remove_calendar()
   if calendar.cal ~= nil then
      naughty.destroy(calendar.cal)
      calendar.cal = nil
      offset = 0
   end
end

function calendar.toggle_calendar()
   if calendar.cal == nil then calendar.add_calendar(0)
   else calendar.remove_calendar(0) end
end


return calendar
