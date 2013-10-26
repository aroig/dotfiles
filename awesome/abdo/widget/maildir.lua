---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

posix = require('posix')

-- {{{ Grab environment
local tonumber = tonumber
local io = { open = io.open }
-- }}}

maildir = {}

local function worker(format, args)
   num = 0
   for _,v in ipairs(args) do
      newmail = v .. '/new'
      if posix.access(newmail, 'r') then
	 num = num + #posix.dir(newmail) - 2
      end
   end
   return {num}
end

return setmetatable(maildir, { __call = function(_, ...) return worker(...) end })
