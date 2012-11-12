---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

-- Abdo Roig
-- I have patched the core path. temp1_input changed to temp2_input

local type = type
local tonumber = tonumber
local string = { match = string.match }
local helpers = require("vicious.helpers")



local function worker(format, warg)
    if not warg then return end

    local zone = { -- Known temperature data sources
        ["sys"]  = {"/sys/class/thermal/",     file = "temp",       div = 1000},
        ["core"] = {"/sys/devices/platform/",  file = "temp2_input",div = 1000},
        ["proc"] = {"/proc/acpi/thermal_zone/",file = "temperature"}
    } --  Default to /sys/class/thermal
    warg = type(warg) == "table" and warg or { warg, "sys" }

    -- Get temperature from thermal zone
    local thermal = helpers.pathtotable(zone[warg[2]][1] .. warg[1])

    if thermal[zone[warg[2]].file] then
        if zone[warg[2]].div then
            return {thermal[zone[warg[2]].file] / zone[warg[2]].div}
        else -- /proc/acpi "temperature: N C"
            return {tonumber(string.match(thermal[zone[warg[2]].file], "[%d]+"))}
        end
    end

    return {0}
end

-- setmetatable(_M, { __call = function(_, ...) return worker(...) end })

return worker