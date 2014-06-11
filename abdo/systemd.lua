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
local posix = posix

rules = require("awful.rules")

local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}


local systemd = { client = {}, rules = {} }



-----------------------------------
-- Utilities                     --
-----------------------------------

local function shell_escape(s)
    local ret = tostring(s) or ''
    ret = ret:gsub('\\', '\\\\')
    ret = ret:gsub('"', '\\"')
    return '"' .. ret .. '"'
end


local function path_exists(path)
    return posix.stat(path) ~= nil
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

local function cgroup_mainpid(cgroup)
    if cgroup then
        local f = io.open(string.format("/sys/fs/cgroup/systemd%s/cgroup.procs", cgroup), 'rb')
        if f then
            local mainpid = f:read("*l")
            f:close()
            return tonumber(mainpid)
        end
    end
    return nil
end

local function free_unit_instance (unit, sep)
    sep = sep or '@'
    local newunit
    local unitpath
    for i=0,100 do
        newunit  = string.gsub(unit, '@', string.format('%s%d', sep, i))
        unitpath = string.format("%s/.config/systemd/user/%s", os.getenv("HOME"), newunit)

        if not systemd.is_active(newunit) and
           not path_exists(unitpath) and
           not path_exists(string.format("%s.d", unitpath)) then
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


-- stop a systemd unit.
function systemd.stop (unit)
    local shcmd
    local stopunit = unit

    if stopunit then
        shcmd = string.format('systemctl --user stop %s',
                              shell_escape(stopunit))

        awful.util.spawn_with_shell(shcmd)
    end
    return stopunit
end


function systemd.list_units()
    local unitlist = {}
    local f = io.popen('systemctl --user --no-pager --no-legend list-unit-files', 'r')
    if f then
        local raw = f:read("*all")
        -- TODO: fix this. need to parse raw.
        for un in raw:gmatch('^[^ ]*') do
            table.insert(unitlist, un)
        end
        f:close()
    end
    return unitlist
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


-- check if client matches rule
function systemd.match_cgroup(c, rule)
    if rule == nil then return false end
    if c == nil then return false end

    local dt = systemd.client[c.window]
    if dt == nil then return false end
    if dt.cgroup == nil then return false end

    return dt.cgroup:match(rule.cgroup) ~= nil and (not rule.main or dt.mainpid == c.pid)
end



-----------------------------------
-- Rule application              --
-----------------------------------

local function matching_rules(c, _rules)
    local result = {}
    for _, entry in ipairs(_rules) do
        if (rules.match(c, entry.rule) or rules.match_any(c, entry.rule_any)) and
            (not rules.match(c, entry.except) and not rules.match_any(c, entry.except_any)) and
            systemd.match_cgroup(c, entry.process) then
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
-- Client matching               --
-----------------------------------

function systemd.matching_clients(rule, main)
    local dt
    local clist = {}
    for i, c in pairs(capi.client.get()) do
        dt = systemd.client[c.window]

        -- manage the client if we got no cgroup
        if not dt then
            systemd.manage_client(c)
            dt = systemd.client[c.window]
        end

        -- if cgroup is not nil and pattern matches
        if systemd.match_cgroup(c, rule) then
            table.insert(clist, c)
        end
    end
    return clist
end



-----------------------------------
-- Signals                       --
-----------------------------------

function systemd.manage_client(c)
    local cgroup = pid_cgroup(c.pid)
    local mainpid = cgroup_mainpid(cgroup)

    -- util.debug("client", { transient_for = c.transient_for, modal = c.modal,
    --                        window = c.window, leader_window = c.leader_window })

    if cgroup then
        systemd.client[c.window] = { cgroup = cgroup, mainpid = mainpid}
        systemd.rules_apply(c)
    end
end


function systemd.unmanage_client(c)
    systemd.client[c.window] = nil
end


function systemd.focus_client(c)

end



return systemd
