---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

-- {{{ Grab environment
local tonumber = tonumber
local io = { open = io.open }
-- }}}

fileval = {}

local function worker(format, args)
   
   local f = io.open(args[1], "r")
   if f ~= nil then
      local val = f:read("*all")
      f:close()
      return {val}
   else
      return {args[2]}
   end
end

return setmetatable(fileval, { __call = function(_, ...) return worker(...) end })
