---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Radu A. <admiral0@tuxfamily.org>
---------------------------------------------------

-- {{{ Grab environment
local io = { popen = io.popen }
local setmetatable = setmetatable
local table = { insert = table.insert }
-- }}}


-- Netctl: provides active netctl network profiles
-- vicious.contrib.netctl
local netctl = {}


-- {{{ Netctl widget type
local function worker(format)
    -- Initialize counters
    local profiles = {}

    local f = io.popen("netctl list")
    for line in f:lines() do
        if line ~= nil and line ~= "" and string.sub(line, 1, 1) == "*" then
            table.insert(profiles, string.sub(line, 3))
        end
    end
    f:close()

    return profiles
end
-- }}}

return setmetatable(netctl, { __call = function(_, ...) return worker(...) end })
