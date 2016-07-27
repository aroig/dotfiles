---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2014, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

-- {{{ Grab environment
local io = { open = io.open }
local awful = awful
local string = string

-- }}}

sdunit = {}

function get_uid()
    f = io.popen("/usr/bin/id -u")
    local hostname = f:read("*a") or ""
    f:close()
    return string.gsub(hostname, "\n$", "")
end

local uid = get_uid()
local cgpath = '/sys/fs/cgroup/systemd'

local function worker(format, args)
    local instance = args[1]
    local cgroup = args[2]
    local path = cgpath

    if instance == 'user' then
        path = path .. '/user.slice'
        path = path .. string.format('/user-%s.slice/user@%s.service', uid, uid)

    else
        path = path .. '/system.slice'
    end

    path = path .. '/' .. cgroup .. '/tasks'

    local f = io.open(path, "r")
    if f ~= nil then
        f:close()
        return true
    else
        return false
    end
end

return setmetatable(sdunit, { __call = function(_, ...) return worker(...) end })
