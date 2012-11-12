---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

-- {{{ Grab environment
local tonumber = tonumber
local io = { popen = io.popen }
-- }}}

sheval = {}

local function worker(format, arg)
   
   local f = io.popen(arg, "r")
   local val
   if f ~= nil then
      val = f:read("*all")
   else
      val = ""
   end
   f:close()
   return {val}
end

return setmetatable(sheval, { __call = function(_, ...) return worker(...) end })
