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
local table = table

rules = require("awful.rules")

local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}


local systemd = { cgroup = {}, rules = {} }



-----------------------------------
-- Utilities                     --
-----------------------------------

local function shell_escape(s)
    local ret = tostring(s) or ''
    ret = ret:gsub('\\', '\\\\')
    ret = ret:gsub('"', '\\"')
    return '"' .. ret .. '"'
end


local function pid_cgroup (pid)
    local cgroup = nil
    local f = io.open(string.format("/proc/%s/cgroup", tostring(pid)), 'rb')
    if f then
        cgroup = string.match(f:read("*all"), "systemd:(.-)%s*$")
        f:close()
    end
    return cgroup
end

local function unit_cgroup (unit)
    local cgroup = nil
    local f = io.popen(string.format("systemctl --user show -p ControlGroup %s", s), 'r')
    if f then
        cgroup = string.match(f:read("*all"), "^ControlGroup=(.-)%s*$")
        f:close()
    end
    return cgroup
end

local function free_unit_instance (unit, sep)
    sep = sep or '@'
    local newunit
    for i=0,100 do
        newunit = string.gsub(unit, '@', string.format('%s%d', sep, i))

        if not systemd.is_active(newunit) then
            return newunit
        end
    end
    return nil
end



-----------------------------------
-- Process execution             --
-----------------------------------

-- Execute an external program and connect the output to systemd journal
function systemd.exec (cmd, name)
    local pid = awful.util.spawn_with_shell(cmd .. string.format(' 2>&1 | systemd-cat -t %s', name))
    return pid
end


-- Execute an external program as a transient systemd scope or service
function systemd.run (cmd, name, scope, slice)
    local sdcmd = "systemd-run --user "

    local unitname
    if name then
        if scope then
            unitname = free_unit_instance('run-' .. name .. '@.scope', '-')
        else
            unitname = free_unit_instance('run-' .. name .. '@.service', '-')
        end
    else
        unitname = nil
    end

    if scope    then sdcmd = sdcmd .. "--scope " end
    if slice    then sdcmd = sdcmd .. string.format("--slice=\"%s\" ", slice) end
    if name     then sdcmd = sdcmd .. string.format("--description=\"%s\" ", name) end
    if unitname then sdcmd = sdcmd .. string.format("--unit=\"%s\" ", unitname) end

    local pid = nil
    if scope then
        -- capture output to journal
        if name then cmd = string.format('%s 2>&1 | systemd-cat -t \"%s\"', cmd, name)
        else         cmd = string.format('%s &> /dev/null', cmd)
        end

        -- do not catch stdout. The process does NOT end immediately
        local pid = awful.util.spawn_with_shell(string.format('%s sh -c %s &> /dev/null',
                                                              sdcmd,
                                                              shell_escape(cmd)))
    else
        sdcmd = sdcmd .. cmd
        -- launch systemd service and capture the service name
        -- TODO: capture stderr to get the pid
        local f = io.popen(string.format("%s 2>&1", sdcmd), "r")
        if f ~= nil then
            local raw = f:read("*all")
            local ret
            if unitname then
                ret = unitname
            else
                ret = string.match(raw, "run%-[0-9]*%.service")
            end
            f:close()
            return ret
        end
    end
end


-- start a systemd unit. If unit is an unspecified instance of an @ unit
-- instantiate it as the first nonexistent instance it finds!
function systemd.start (unit)
    local shcmd
    local startunit
    if string.match(unit, '.*@%.service') then
        startunit = free_unit_instance(unit, '@')
    else
        startunit=unit
    end

    if startunit then
        shcmd = string.format('systemctl --user start %s',
                              shell_escape(startunit))

        awful.util.spawn_with_shell(shcmd)
    end
    return startunit
end



-----------------------------------
-- State checking                --
-----------------------------------

-- check whether a unit is active
function systemd.is_active(unit)
    local cmd = string.format("systemctl --user -q is-active %s",
                              shell_escape(unit))
    return os.execute(cmd)
end


-- get cgroup from pid or unit name
function systemd.get_cgroup (s)
    s = tostring(s)
    local cgroup = nil

    -- we got a pid
    if string.match(s, '^[0-9]*$') then
        local f = io.open(string.format("/proc/%s/cgroup", tostring(s)), 'rb')
        if f then
            cgroup = string.match(f:read("*all"), "systemd:(.*)$")
            f:close()
        end

    -- we got a unit name
    else
        local f = io.popen(string.format("systemctl --user show -p ControlGroup %s", s), 'r')
        if f then
            cgroup = string.match(f:read("*all"), "^ControlGroup=(.*)$")
            f:close()
        end
    end
    return cgroup
end



-----------------------------------
-- Client matching               --
-----------------------------------

function systemd.matching_clients(pat)
    local cgroup
    local clist = {}
    for i, c in pairs(capi.client.get()) do
        cgroup = systemd.cgroup[c.window]

        -- manage the client if we got no cgroup
        if not cgroup then
            systemd.manage_client(c)
            cgroup = systemd.cgroup[c.window]
        end

        -- if cgroup is not nil and pattern matches
        if cgroup and cgroup:match(pat) then
            table.insert(clist, c)
        end
    end
    return clist
end



-----------------------------------
-- Client matching               --
-----------------------------------

local function match_cgroup(c, cgroup)
    if cgroup == nil then return false end

    local clientcgroup = systemd.cgroup[c.window]
    if clientcgroup == nil then return false end

    return clientcgroup:match(cgroup) ~= nil
end


local function matching_rules(c, _rules)
    local result = {}
    for _, entry in ipairs(_rules) do
        if match_cgroup(c, entry.cgroup) then
            table.insert(result, entry)
        end
    end
    return result
end


function systemd.rules_apply(c)
    local props = {}
    local callbacks = {}

    for _, entry in ipairs(matching_rules(c, systemd.rules)) do
        if entry.properties then
            for property, value in pairs(entry.properties) do
                props[property] = value
            end
        end
        if entry.callback then
            table.insert(callbacks, entry.callback)
        end
    end

    rules.execute(c, props, callbacks)
end



-----------------------------------
-- Signals                       --
-----------------------------------

function systemd.manage_client(c)
    local cgroup = pid_cgroup(c.pid)
    if cgroup then
        systemd.cgroup[c.window] = cgroup
        systemd.rules_apply(c)
    end
end


function systemd.unmanage_client(c)
    systemd.cgroup[c.window] = nil
end



return systemd
