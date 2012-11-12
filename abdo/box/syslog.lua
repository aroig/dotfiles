io       = require("io")
string   = require("string")
table    = require("table")

local theme = theme or {}

local font = theme.font_box
local row_length = 130
local num_entries = 30

local time_color = theme.fg_grey
local text_color = theme.fg_normal

local daemon_color = {kernel = theme.fg_red, default = theme.fg_green}

local syslog = {}
syslog.syslog = nil

local function generate_syslog()
   local text = "\n"
   local fd = io.popen('sudo journalctl -a -n' .. tostring(100*num_entries) ..
                 ' | grep -v "NetworkManager\\|slim\\|sudo" | tail -n ' .. tostring(num_entries))
   local lines = {}
   for l in fd:lines() do
      table.insert(lines, l)
   end

   for i=#lines,1,-1  do
      local line  = lines[i]
      local len = 0
--      local _, _, time, daemon, message = string.find(line, "^%w+%s*%d+%s*(.-):%d+%s+%w+%s*(.-)%s*:%s*(.*)")
      local _, _, time, rawmessage = string.find(line, "^%w+%s*%d+%s*(.-):%d+%s+%w+%s*(.*)")

      if time and rawmessage then
         len = len + #time + 2
         new = string.format('<span color="%s">%s</span>  ', time_color, time)

         local _, _, daemon, message = string.find(rawmessage, "^(.-):%s+(.*)")

         if daemon then
            daemon = string.gsub(daemon, "%s*%[.-%]%s*", "")
            len = len + #daemon + 2
            if daemon_color[daemon] then
               color = daemon_color[daemon]
            else
               color = daemon_color['default']
            end
            daemon = string.gsub(string.lower(daemon), "/usr/%wbin/", "")
            new = new .. string.format('<span color="%s">%s: </span> ', color, daemon)
         else
            message = rawmessage
         end

         if row_length > len then
            message = string.gsub(message, "%s*%[.-%]%s*", "")
            message = awful.util.escape(string.sub(message, 1, row_length - len))
         else
            message = ""
         end
         new = new .. string.format('<span color="%s">%s</span> ', text_color, message)

         text = text .. new  .. "\n"
      end
   end
   text = string.format('<span font="%s">%s</span>', font, text)
   return {log=text}
end



function syslog.add_syslog()
   syslog.remove_syslog()
   local data = generate_syslog()
   syslog.syslog = naughty.notify({ title = os.date("System Log"),
			     text = data.log,
			     timeout = 0})
end

function syslog.remove_syslog()
   if syslog ~= nil then
      naughty.destroy(syslog.syslog)
      syslog.syslog = nil
   end
end




function syslog.toggle_syslog()
   if syslog.syslog == nil then
      syslog.add_syslog()
   else
      syslog.remove_syslog()
   end
end


return syslog