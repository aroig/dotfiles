---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

local type = type
local tonumber = tonumber
local string = { match = string.match }
local helpers = require("helpers")

local thermal = {}

local function worker(format, warg)
    if not warg then return end

    -- Get temperature from thermal zone
    local data = helpers.pathtotable("/sys/class/hwmon/" .. warg[1])
    local name = warg[2] or "temp1"

    if data then
        local value = {temp = data[name .. "_input"] and tonumber(data[name .. "_input"]) / 1000,
                       crit = data[name .. "_crit"] and tonumber(data[name .. "_crit"]) / 1000,
                       label = data[name .. "_label"] or "<unknown>"}
        return value
    end

    return {temp = 0, crit = 0, label = "<unknown>"}
end

return setmetatable(thermal, { __call = function(_, ...) return worker(...) end })
