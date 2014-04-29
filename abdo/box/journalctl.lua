local io = io
local string = string
local table = table
local beautiful = beautiful

local row_length = 130
local num_entries = 40


function journalctl(args)
   local font = beautiful.font_box
   local time_color = beautiful.color_grey
   local text_color = beautiful.color_fg
   local daemon_color = {kernel = beautiful.color_red, default = beautiful.color_green}

   local text = "\n"

   if not args then args = "" end
   local fd = io.popen('sudo journalctl -a -n' .. tostring(10*num_entries) .. string.format(' %s ', args) ..
                 ' | grep -v "slim\\|sudo" | tail -n ' .. tostring(num_entries))

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

         text = text .. new
         if i > 1 then
             text = text  .. "\n"
         end
      end
   end
   text = string.format('<span font="%s">%s</span>', font, text)
   return {log=text}
end


return journalctl
