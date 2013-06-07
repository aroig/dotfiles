local io = io
local string = string
local table = table

journalctl = require("abdo.box.journalctl")

local userlog = {}
userlog.userlog = nil

function userlog.add_userlog()
   userlog.remove_userlog()
   local data = journalctl("_SYSTEMD_OWNER_UID=$(id -u $USER)")
   userlog.userlog = naughty.notify({ title = os.date("User Log"),
			     text = data.log,
			     timeout = 0})
end


function userlog.remove_userlog()
   if userlog ~= nil then
      naughty.destroy(userlog.userlog)
      userlog.userlog = nil
   end
end


function userlog.toggle_userlog()
   if userlog.userlog == nil then
      userlog.add_userlog()
   else
      userlog.remove_userlog()
   end
end

return userlog
