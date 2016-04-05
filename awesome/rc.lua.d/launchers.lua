---------------------------------------------------------------
-- File:    launchers.lua         Launcher functions         --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


-----------------------------------
-- Requires                      --
-----------------------------------

local os = os
local string = string
local coroutine = coroutine
local table = table
local awful = awful

local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}

local apps = apps

local util      = require("abdo.util")
local pickle    = require("abdo.pickle")           -- some crude pickling routines
local luaeval   = require("abdo.prompt.luaeval")   -- evaluation of lua code
local promptl   = require("abdo.prompt.list")      -- user choice from a list
local idoprompt = require("abdo.prompt.idoprompt")
local systemd   = require("abdo.systemd")



-----------------------------------
-- Initialization                --
-----------------------------------

listsrc = {
    app  = awful.util.getdir("config") .. "/lists/app-list.lua",
    doc  = awful.util.getdir("config") .. "/lists/doc-list.lua",
    dd   = awful.util.getdir("config") .. "/lists/dd-list.lua",
}

execslice = {
    app  = "apps",
    doc  = "apps",
    dd   = "dropdown",
}

execlist = {}
for k, path in pairs(listsrc) do
    execlist[k] = pickle.load(path)
end



-----------------------------------
-- Useful functions              --
-----------------------------------

-- Execute an external program
function exec (cmd)
    awful.spawn.spawn(cmd, false)
end

-- Execute an external program inside a shell
function shexec (cmd)
    awful.spawn.with_shell(cmd)
end



-----------------------------------
-- Run a command                 --
-----------------------------------

function run(name, opts)

    if name == nil then return end

    opts = opts or {ask = false}
    local ask       = opts['ask']
    local slice     = opts['slice']
    local unit_name = opts['unit_name']
    local scope     = opts['scope']

    -- get the right thing
    local ns, entry
    ns, entry = name:match("^([^:]*):(.*)$")

    -- look for commands at the lists. This redefines the following variables: name, ns, entry.
    if execlist[ns] ~= nil then
        name = execlist[ns][entry]
        slice = slice or execslice[ns]
        unit_name = unit_name or entry

        if not name then
            naughty.notify({title="Error in run",
                            text=string.format("command name '%s' not found on the command lists", entry)})
            return
        end

        -- now that we have dereferenced the command through the list, let's compute ns and entry again.
        ns, entry = name:match("^([^:]*):(.*)$")
    end

    -- if no namespace is given, attempt to guess it
    if ns == nil then
        if string.match(name, '^.*%.service%s*$') or string.match(name, '^.*%.target%s*$') then
            ns = 'sd'
        else
            ns = 'cm'
        end
        entry = name
    end

    -- prepare the command executing function
    local exec_func
    if ns == 'sd' then
        exec_func = function() systemd.start(entry) end

    elseif ns == 'sh' then
        exec_func = function () shexec(entry) end

    elseif ns == 'cm' or ns == 'tm' then
        local cmd
        if ns == 'tm' then
            cmd = apps.termcmd(entry, unit_name)
        else
            cmd = entry
        end

        local props = {scope=false,
                       slice=slice or "apps"}
        exec_func = function () systemd.run(cmd, unit_name, props) end

    else
        naughty.notify({title="Unknown command prefix",
                        text=string.format("command prefix '%s' is not recognized", ns)})

    end

    -- run the command
    if ask then
        local lst = { yes = true, no = false }

        promptl.run(myw.promptbox[awful.screen.focused()].widget,
                    string.format("Run %s? ", unit_name),
                    function (c) if c then exec_func() end end,
                    lst, nil)

    else
        exec_func()
    end
end



-----------------------------------
-- Client manipulations          --
-----------------------------------

-- dropdown data
dropdown = { last_client = nil, clients = {} }

function hide_client(c)
    if c then
        c.hidden = true
        local ctags = c:tags()
        for i, t in pairs(ctags) do
            ctags[i] = nil
        end
        c:tags(ctags)
    end
end


function kill_client(c)
    if c then
        c:kill()
    end
end


function show_client(c)
    if c then
        -- move to the right tag
        awful.client.movetotag(awful.tag.selected(awful.screen.focused()), c)

        -- unhide
        c.hidden = false

        -- reapply rules
        awful.rules.apply(c)

        -- raise
        c:raise()
        capi.client.focus = c

        -- remember last client
        dropdown.last_client = c
    end
end


function is_visible_client(c)
    if c == nil then return false end
    if c.hidden then return false end

    local ctags = c:tags()
    local tag = awful.tag.selected(awful.screen.focused())

    for i, t in pairs(ctags) do
        if t == tag then
            return true
        end
    end

    return false
end


function dropdown.get_dropdown_name(c)
    if c == nil then return nil end

    for n, d in pairs(dropdown.clients) do
        if d == c then
            return n
        end
    end

    return nil
end


function toggle_client(c)
    if is_visible_client(c) then
        hide_client(c)
    else
        show_client(c)
    end
end


function dropdown.is_dropdown_client(c)
    return dropdown.get_dropdown_name(c) ~= nil
end


function dropdown.manage_client(name, c)
    if c == nil then return end

    -- add client to the dropdown list
    if dropdown.clients[name] == nil then
        dropdown.clients[name] = c
    end

    -- mark as last client
    dropdown.last_client = c
end


function dropdown.focus_client(c)
    if c == nil then return end

    if dropdown.is_dropdown_client(c) then
        dropdown.last_client = c
    end
end


function dropdown.unmanage_client(c)
    if c == nil then return end

    -- remove from dropdown list
    local name = dropdown.get_dropdown_name(c)
    if name then
        dropdown.clients[name] = nil
    end

    -- forget about last client
    if dropdown.last_client == c then
        dropdown.last_client = nil
    end
end



-----------------------------------
-- Dropdown manipulations        --
-----------------------------------

function ddtoggle(name, launch)
    if name == nil then return end

    local ns, entry
    ns, entry = name:match("^([^:]*):(.*)$")
    if ns == nil then entry = name end

    local cmd = nil
    if launch then cmd = name end

    if dropdown.clients[entry] then
        toggle_client(dropdown.clients[entry])

    elseif launch then
        run(cmd)
    end
end

function ddshow(name, launch)
    if name == nil then return end

    local ns, entry
    ns, entry = name:match("^([^:]*):(.*)$")
    if ns == nil then entry = name end

    local cmd = nil
    if launch then cmd = name end

    if dropdown.clients[entry] then
        show_client(dropdown.clients[entry])

    elseif launch then
        run(cmd)
    end
end

function ddhide(name)
    if name == nil then return end

    local ns, entry
    ns, entry = name:match("^([^:]*):(.*)$")
    if ns == nil then entry = name end

    if dropdown.clients[entry] then
        hide_client(dropdown.clients[entry])
    end
end

function ddhide_all()
    for n, c in pairs(dropdown.clients) do
        hide_client(c)
    end
end

function ddshow_last()
    show_client(dropdown.last_client)
end

function ddhide_last()
    hide_client(dropdown.last_client)
end

local function ddshow_doc(url)
    if url == nil then return end

    local cmd = string.format("dwb -p docs %s", util.shell_escape(url))
    local props = {scope=false, slice="apps"}
    systemd.run(cmd, "docs", props)
end



-----------------------------------
-- Dropdown naughty boxes        --
-----------------------------------

box = {}

box.naughtylog = require("abdo.box.naughtylog")  -- log naughty notifications
box.orgtasks   = require("abdo.box.orgtasks")    -- org todo list
box.calendar   = require("abdo.box.calendar")    -- calendar



-----------------------------------
-- Prompts                       --
-----------------------------------
prompt = {}



function prompt.wikipedia()
    idoprompt.run(myw.promptbox[awful.screen.focused()].widget,
                  "Wikipedia: ",
                  function (cmd)
                      local url = string.format("http://en.wikipedia.org/wiki/Special:Search?go=Go&search=\"%s\"", cmd)
                      ddshow_doc(url)
                  end,
                  nil,
                  awful.util.getdir("cache") .. "/history_wikipedia")
end


function prompt.mathscinet()
    idoprompt.run(myw.promptbox[awful.screen.focused()].widget,
                  "Mathscinet: ",
                  function (cmd)
                      local url = string.format("http://www.ams.org/mathscinet/search/publications.html?review_format=html&pg4=ALLF&s4=\"%s\"", cmd)
                      ddshow_doc(url)
                  end,
                  nil,
                  awful.util.getdir("cache") .. "/history_mathscinet")
end


function prompt.docs()
    execlist.doc = pickle.load(listsrc.doc)
    if not execlist.doc then
        naughty.notify({title="Error in document prompt",
                        text=string.format("Can't load file '%s'", listsrc.doc)})
        return
    end

    promptl.run(myw.promptbox[awful.screen.focused()].widget,
                "Doc: ",
                ddshow_doc,
                execlist.doc,
                awful.util.getdir("cache") .. "/history_docs")
end


function prompt.command()
    execlist.app = pickle.load(listsrc.app)
    if not execlist.app then
        naughty.notify({title="Error in command prompt",
                        text=string.format("Can't load file '%s'", listsrc.app)})
        return
    end

    -- want the prompt to pass progname, not prog command to the callback
    local proglist = {}
    for name, cmd in pairs(execlist.app) do
        proglist[name] = string.format("app:%s", name)
    end

    promptl.run(myw.promptbox[awful.screen.focused()].widget,
                "Run: ",
                function (nm) run(nm) end,
                proglist,
                awful.util.getdir("cache") .. "/history_cmd")
end


function prompt.dropdown()
    execlist.dd = pickle.load(listsrc.dd)
    if not execlist.dd then
        naughty.notify({title="Error in command prompt",
                        text=string.format("Can't load file '%s'", listsrc.dd)})
        return
    end

    -- want the prompt to pass progname, not prog command to the callback
    local proglist = {}
    for name, cmd in pairs(execlist.dd) do
        proglist[name] = string.format("dd:%s", name)
    end

    promptl.run(myw.promptbox[awful.screen.focused()].widget,
                "Dropdown: ",
                function (nm) ddshow(nm, true) end,
                proglist,
                awful.util.getdir("cache") .. "/history_dropdown")
end


-- prompt with a list of systemd user units to activate.
function prompt.systemd()
    execlist.sd = systemd.list_units()

    local units = {}
    for i, u in ipairs(execlist.sd) do
        units[u] = u
    end

    -- TODO: pretty print the table to debug
    -- naughty.notify({text=pickle.dumps(units)})

    promptl.run(myw.promptbox[awful.screen.focused()].widget,
                "Unit: ",
                function (nm) systemd.start(nm) end,
                units,
                awful.util.getdir("cache") .. "/history_systemd")
end


function prompt.lua()
    luaeval.run(myw.promptbox[awful.screen.focused()].widget)
end


-----------------------------------
-- Switch                        --
-----------------------------------

switch = {}

-- start targets consecutively. Each invocation detects the first active target on the list
-- stops it and starts the next.
function switch.systemd_switch(units, step)
    local step = step or 1
    local num = #units

    if num == 0 or step == 0 then
        return
    end

    local ifi, ila
    if step > 0 then
        ifi = 1
        ila = num
    else
        ifi = num
        ila = 1
    end

    local start = units[1]
    local stop = nil

    for i = ifi, ila, step do
        if systemd.is_active(units[i]) then
            stop = units[i]
            start = units[(i % num) + 1]
            break
        end
    end

    if stop ~= nil and stop ~= start then
        systemd.stop(stop)
    end

    if start ~= nil then
        systemd.start(start)
        naughty.notify({title="Systemd Switch", text=string.format("Switching to unit '%s'", start)})
    end
end


function switch.orientation_mode(step)
    local hostname = hostname
    local step = step or 1
    local tgtlist = {}

    tgtlist = {'landscape.target', 'portrait.target'}

    switch.systemd_switch(tgtlist, step)
end


function switch.machine_mode(step)
    local hostname = hostname
    local step = step or 1
    local tgtlist = {}

    if hostname == 'galois' then
        tgtlist = {'laptop.target', 'tablet.target'}
    else
        tgtlist = {'desktop.target'}
    end

    switch.systemd_switch(tgtlist, step)
end


function switch.output_mode(step)
    local hostname = hostname
    local step = step or 1
    local tgtlist = {}

    if hostname == 'galois' then
        tgtlist = {'xrandr-single.service', 'xrandr-dual.service'}
    else
        tgtlist = {}
    end

    switch.systemd_switch(tgtlist, step)
end
