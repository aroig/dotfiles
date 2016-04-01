---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
--  * (c) 2009, Lucas de Vries <lucas@glacicle.com>
---------------------------------------------------

-- {{{ Grab environment
local tonumber = tonumber
local os = { time = os.time }
local io = { lines = io.lines }
local setmetatable = setmetatable
local string = { match = string.match }
local helpers = require("helpers")
-- }}}


-- Net: provides state and usage statistics of all network interfaces
-- vicious.widgets.net
local net = {}


-- Initialize function tables
local nets = {}
-- Variable definitions
local unit = { ["b"] = 1, ["kb"] = 1024,
    ["mb"] = 1024^2, ["gb"] = 1024^3
}

-- {{{ Net widget type
local function worker(format, warg)
    local name = warg
    local args = {}

    -- Get NET stats
    for line in io.lines("/proc/net/dev") do
        if string.match(line, name:gsub("%-", "%%-") .. ":") then
            -- Received bytes, first value after the name
            local recv = tonumber(string.match(line, ":[%s]*([%d]+)"))
            -- Transmited bytes, 7 fields from end of the line
            local send = tonumber(string.match(line,
             "([%d]+)%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d+%s+%d$"))

            helpers.uformat(args, "rx", recv, unit)
            helpers.uformat(args, "tx", send, unit)

            -- Operational state and carrier detection
            local sysnet = helpers.pathtotable("/sys/class/net/" .. name)
            args["{carrier}"] = tonumber(sysnet.carrier) or 0

            local now = os.time()
            if nets[name] == nil then
                -- Default values on the first run
                nets[name] = {}
                helpers.uformat(args, "down", 0, unit)
                helpers.uformat(args, "up",   0, unit)
            else -- Net stats are absolute, substract our last reading
                local interval = now - nets[name].time
                if interval <= 0 then interval = 1 end

                local down = (recv - nets[name][1]) / interval
                local up   = (send - nets[name][2]) / interval

                helpers.uformat(args, "down", down, unit)
                helpers.uformat(args, "up",   up,   unit)
            end

            nets[name].time = now

            -- Store totals
            nets[name][1] = recv
            nets[name][2] = send
        end
    end

    return args
end
-- }}}

return setmetatable(net, { __call = function(_, ...) return worker(...) end })
