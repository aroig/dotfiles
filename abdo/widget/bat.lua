---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

-- {{{ Grab environment
local tonumber = tonumber
local setmetatable = setmetatable
local string = { format = string.format }
local helpers = require("helpers")
local math = {
    min = math.min,
    floor = math.floor
}
-- }}}


-- Bat: provides state, charge, and remaining time for a requested battery
-- vicious.widgets.bat
local bat = {}


-- {{{ Battery widget type
local function worker(format, warg)
    if not warg then return end

    local battery = helpers.pathtotable("/sys/class/power_supply/"..warg)

    local state = "unknown"
    local rate = 0
    local percent = 0
    local time = "?"

    local timeleft = nil
    local remaining = nil
    local capacity = nil

    -- Check if the battery is present
    if battery.present ~= "1\n" then
        return {state=state, percent=percent, rate=rate, time=time}
    end

    -- Get state information. Can be: full, unknown, charged, charging, discharging.
    state = battery.status or "unknown"
    state = state:lower():gsub("\n", "")

    -- Get capacity information
    if battery.charge_now then
        remaining, capacity = battery.charge_now, battery.charge_full
    elseif battery.energy_now then
        remaining, capacity = battery.energy_now, battery.energy_full
    end

    -- Get charge information
    if battery.current_now then
        rate = tonumber(battery.current_now)
    elseif battery.power_now then
        rate = tonumber(battery.power_now)
    end

    -- Calculate percentage (but work around broken BAT/ACPI implementations)
    if remaining ~= nil and capacity ~= nil then
        percent = math.min(math.floor(remaining / capacity * 100), 100)
    end

    -- Calculate remaining (charging or discharging) time
    if remaining ~= nil and capacity ~= nil and rate ~= nil and rate ~= 0 then
        if state == "+" then
            timeleft = (tonumber(capacity) - tonumber(remaining)) / tonumber(rate)
        elseif state == "-" then
            timeleft = tonumber(remaining) / tonumber(rate)
        end

        -- Calculate time
        if timeleft ~= nil then
            local hoursleft   = math.floor(timeleft)
            local minutesleft = math.floor((timeleft - hoursleft) * 60 )
            time = string.format("%02d:%02d", hoursleft, minutesleft)
        end
    end

    return {state=state, percent=percent, rate=rate/1000000, time=time}
end
-- }}}

return setmetatable(bat, { __call = function(_, ...) return worker(...) end })
