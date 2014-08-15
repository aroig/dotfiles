---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

-- {{{ Grab environment
local io = { open = io.open }
-- }}}

filex = {}

local function worker(format, args)
   local f = io.open(args[1], "r")
   if f ~= nil then
       f:close()
       return true
   else
       return false
   end
end

return setmetatable(filex, { __call = function(_, ...) return worker(...) end })
