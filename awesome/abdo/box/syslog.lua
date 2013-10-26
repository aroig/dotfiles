local io = io
local string = string
local table = table

journalctl = require("abdo.box.journalctl")

local syslog = {}
syslog.syslog = nil

-- TODO: For now I filter by priority. I'd like to be able to filter out user session
-- messages though!
function syslog.add_syslog()
   syslog.remove_syslog()
   local data = journalctl("PRIORITY=5")
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
