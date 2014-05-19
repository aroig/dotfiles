---------------------------------------------------------------
-- File:    systemd.lua         Interface with systemd       --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

-----------------------------------
-- Environment                   --
-----------------------------------

local os = os
local string = string

local util = require("abdo.util")

local systemd = {}

-- Execute an external program and connect the output to systemd journal
function systemd.exec (cmd, name)
    awful.util.spawn_with_shell(cmd .. string.format(' 2>&1 | systemd-cat -t %s', name))
end


-- Execute an external program as a systemd scope or service
function systemd.run (cmd, name, scope, slice)
    local sdcmd = "systemd-run --user "
    if scope then sdcmd = sdcmd .. "--scope " end
    if slice then sdcmd = sdcmd .. string.format("--slice=\"%s\" ", slice) end
    if name  then sdcmd = sdcmd .. string.format("--description=\"%s\" ", name) end

    local pid = nil
    if scope then
        -- capture output to journal
        if name then cmd = string.format('%s 2>&1 | systemd-cat -t \"%s\"', cmd, name)
        else         cmd = string.format('%s &> /dev/null', cmd)
        end

        -- do not catch stdout. The process does NOT end immediately
        awful.util.spawn_with_shell(string.format('%s sh -c %s &> /dev/null',
                                                  sdcmd,
                                                  util.shell_escape(cmd)))
    else

        -- launch systemd service and capture the service name
        -- TODO: capture stderr to get the pid
        local f = io.popen(sdcmd, "r")
        if f ~= nil then
            local raw = f:read("*all")
            local pid = raw:gsub(".*run%-([0-9]*)%.service.*", "%1")

            -- naughty.notify({title="sdexec", text=tostring(raw)})
            f:close()
        end
    end

    if pid then return tonumber(pid)
    else        return nil
    end
end


function systemd.isactive(unit)
    local cmd = string.format("systemctl --user -q is-active %s",
                              util.shell_escape(unit))
    return os.execute(cmd)
end


-- start a systemd unit. If unit is an unspecified instance of an @ unit
-- instantiate it as the first nonexistent instance it finds!
function systemd.start (unit)
    local shcmd
    local startunit=unit
    if string.match(unit, '.*@%.service') then
        for i=0,100 do
            local newunit = string.gsub(unit, '@%.',
                                        string.format('@%d.', i))

            if not systemd.isactive(newunit) then
                startunit = newunit
                break
            end
        end
    end

    shcmd = string.format('systemctl --user start %s',
                          util.shell_escape(startunit))

    awful.util.spawn_with_shell(shcmd)
end


-- get cgroup from pid
function systemd.cgroup (pid)
    local f = io.open(string.format("/proc/%s/cgroup", tostring(pid)), 'rb')
    local cgroup = string.match(f:read("*all"), "systemd:(.*)$")
    f:close()
    return cgroup
end


return systemd
