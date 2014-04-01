---------------------------------------------------
-- Licensed under the GNU General Public License v3
-- 2014, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

-- {{{ Grab environment
local table = table
local io = io
local os = os
-- }}}

fileval = {}

local function worker(format, args)

    local path = os.getenv('XDG_RUNTIME_DIR') .. '/mutag'
    local mail = {}
    local queue = {}

    f = io.open(path .. '/mail', 'r')
    if f ~= nil then
        for line in f:lines() do
            table.insert(mail, line)
        end
    end
    f = io.open(path .. '/queue', 'r')
    if f ~= nil then
        for line in f:lines() do
            table.insert(queue, line)
        end
    end

    return {mail, queue}
end

return setmetatable(fileval, { __call = function(_, ...) return worker(...) end })
