---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

local tonumber = tonumber
local io = { open = io.open }

local pvol_file = "/tmp/abdo-pvol/vol"

local function get_volume()
   local f = io.open(pvol_file, "r")
   if f then
      local vol = f:read("*all")
      f:close()
      return tonumber(vol)
   else
      return nil
   end
end


local function worker(format)
   local vol = get_volume()
   if vol == nil then vol = 0 end
   return { vol, "on"}
end

return worker

-- setmetatable(_M, { __call = function(_, ...) return worker(...) end })
