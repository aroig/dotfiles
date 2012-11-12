
local table    = require("table")
local os       = require("os")
local string   = require("string")

local abdoutil = require("abdo.util")

local theme = theme or {}

local row_length = 130

local font = theme.font_box
local time_color = theme.fg_grey
local origin_color = theme.fg_green
local text_color = theme.fg_normal


local num_entries = 20
local ignore_titles = abdoutil.set({"Notification Log", "System Log", "Calendar", "Tasks", "Debug", "Status"})


local naughtylog = {}
naughtylog.nlog = nil
naughtylog.log = {}



local function generate_naughtylog(data)
   local text = "\n"
   local num = #data

   if num > num_entries then numi = num - num_entries
   else                      numi = 1
   end

   for i=num,numi,-1 do
      local m = data[i]
      local len = 0
      text = text .. string.format('<span color="%s">%s</span>', time_color, os.date("%H:%M", m.time)) .. "  "
      len = len + 5 + 2

      if m.appname then
	 text = text .. string.format('<span color="%s">%s</span>', origin_color, awful.util.escape(m.appname)) .. ": "
	 len = len + #m.appname + 2
      end

      if m.title then
	 text = text .. string.format('<span color="%s">%s</span>', text_color, awful.util.escape(m.title)) .. ". "
	 len = len + #m.title + 2
      end

      local flattext
      if row_length > len and m.text then
	 flattext = awful.util.escape(string.sub(string.gsub(m.text, "\n", ". "), 1, row_length - len))
      else
	 flattext = ""
      end

      text = text .. string.format('<span color="%s">%s</span>', text_color, flattext)
      text = text .. "\n"
   end
   text = string.format('<span font="%s">%s</span>', font, text)
   return {log=text}
end


function naughtylog.add_naughtylog()
   naughtylog.remove_naughtylog()
   local data = generate_naughtylog(naughtylog.log)
   naughtylog.nlog = naughty.notify({ title = os.date("Notification Log"),
				 text = data.log,
				 timeout = 0})
end

function naughtylog.remove_naughtylog()
   if naughtylog.nlog ~= nil then
      naughty.destroy(naughtylog.nlog)
      naughtylog.nlog = nil
   end
end



function naughtylog.toggle_naughtylog()
   if naughtylog.nlog == nil then
      naughtylog.add_naughtylog()
   else
      naughtylog.remove_naughtylog()
   end
end



function naughtylog.append(args)
   if ignore_titles[args.title] then
      return nil
   else

      table.insert(naughtylog.log, {title=args.title,
				     text=args.text,
                                     appname=args.appname or "awesome",
				     time=os.time()})
   end
end



return naughtylog