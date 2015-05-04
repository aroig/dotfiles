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
    if type(name) == "string" then
        name = { name }
    end

    if data then
        local all_values = {temp = 0, sensors = {} }
        for _,n in ipairs(name) do
            local value = {temp = data[n .. "_input"] and tonumber(data[n .. "_input"]) / 1000,
                           crit = data[n .. "_crit"] and tonumber(data[n .. "_crit"]) / 1000,
                           label = data[n .. "_label"] or "<unknown>"}
            table.insert(all_values.sensors, value)
            if all_values.temp < value.temp then
                all_values.temp = value.temp
            end
        end
        return all_values
    end

    return { temp = 0, sensors = {temp = 0, crit = 0, label = "<unknown>"} }
end

return setmetatable(thermal, { __call = function(_, ...) return worker(...) end })
