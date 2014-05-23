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

dropdown = {last = nil}



-----------------------------------
-- Useful functions              --
-----------------------------------

-- Execute an external program
function exec (cmd)
    awful.util.spawn(cmd)
end

-- Execute an external program inside a shell
function shexec (cmd)
    awful.util.spawn_with_shell(cmd)
end



-----------------------------------
-- Run a command                 --
-----------------------------------

function run(name, opts)

    if name == nil then return end

    opts = opts or {ask = false}
    local ask   = opts['ask']
    local slice = opts['slice']

    -- get the right thing
    ns, entry = name:match("^([^:]*):(.*)$")

    -- if no namespace, just run a nameless command
    if ns == nil then
        ns = 'cmd'
        entry = name
    end

    -- prepare the command executing function
    local exec_func
    if ns == 'sd' then
        exec_func = function() systemd.start(entry) end

    elseif ns == 'sh' then
        exec_func = function () shexec(entry) end

    elseif ns == 'cmd' then
        exec_func = function () systemd.run(entry, nil, false, slice or 'apps') end

    elseif execlist[ns] ~= nil then
        local cmd = execlist[ns][entry]
        if not cmd then
            naughty.notify({title="Error in run",
                            text=string.format("Unknown command name '%s'", name)})
            return
        end

        -- detect whether cmd is a systemd unit or a command
        if string.match(cmd, '^.*%.service%s*$') or string.match(cmd, '^.*%.target%s*$') then
            exec_func = function() systemd.start(cmd) end
        else
            slice = slice or execslice[ns] or "apps"
            exec_func = function() systemd.run(cmd, entry, false, slice) end
        end
    end

    -- run the command
    if ask then
        local lst = { yes = true, no = false }

        promptl.run(myw.promptbox[mouse.screen].widget,
                    string.format("Run %s? ", entry),
                    function (c) if c then exec_func() end end,
                    lst, nil)

    else
        exec_func()
    end
end



-----------------------------------
-- Client manipulations          --
-----------------------------------

function hide_client(c)
    c.hidden = true
    local ctags = c:tags()
    for i, t in pairs(ctags) do
        ctags[i] = nil
    end
    c:tags(ctags)
end


function kill_client(c)
    c:kill()
end


function show_client(c)
    -- move to the right tag
    awful.client.movetotag(awful.tag.selected(mouse.screen), c)

    -- raise client
    c.hidden = false
    c:raise()
    capi.client.focus = c

    -- reapply rules
    systemd.rules_apply(c)
end


function is_visible_client(c)
    if c.hidden then return false end

    local ctags = c:tags()
    local tag = awful.tag.selected(mouse.screen)

    for i, t in pairs(ctags) do
        if t == tag then
            return true
        end
    end

    return false
end


function toggle_client(c)
    if is_visible_client(c) then
        hide_client(c)
    else
        show_client(c)
    end
end



-----------------------------------
-- Client cgroup manipulations   --
-----------------------------------

-- TODO: handle launching client when none is matching

function show_cgroup(rule, cmd)
    local list = systemd.matching_clients(rule)
    if #list > 0 then
        for i, c in ipairs(list) do
            show_client(c)
        end
    elseif cmd ~= nil then
        run(cmd)
    end
end


function hide_cgroup(rule)
    local list = systemd.matching_clients(rule)
    for i, c in ipairs(list) do
        hide_client(c)
    end
end


function kill_cgroup(rule)
    local list = systemd.matching_clients(rule)
    for i, c in ipairs(list) do
        kill_client(c)
    end
end


function toggle_cgroup(rule, cmd)
    local list = systemd.matching_clients(rule)
    if #list > 0 then
        for i, c in ipairs(list) do
            toggle_client(c)
        end
    elseif cmd ~= nil then
        run(cmd)
    end
end


-----------------------------------
-- Dropdown manipulations        --
-----------------------------------
-- We put all dropdowns under dropdown.slice. This is done either on the
-- individual units or by run for the dd namespace.

function ddtoggle(name)
    if name == nil then return end
    ns, entry = name:match("^([^:]*):(.*)$")

    if ns == nil then
        entry = name
    end

    dropdown.last = name
    toggle_cgroup({ cgroup = string.format('dropdown.slice/.*%s', entry), main = true }, name)
end

function ddshow(name)
    if name == nil then return end
    ns, entry = name:match("^([^:]*):(.*)$")

    if ns == nil then
        entry = name
    end

    dropdown.last = name
    show_cgroup({ cgroup = string.format('dropdown.slice/.*%s', entry), main = true }, name)
end

function ddshow_last()
    ddshow(dropdown.last)
end

function ddhide(name)
    if name == nil then return end
    ns, entry = name:match("^([^:]*):(.*)$")

    if ns == nil then
        entry = name
    end

    hide_cgroup({ cgroup = string.format('dropdown.slice/.*%s', entry), main = true })
end

function ddhide_all()
    hide_cgroup({ cgroup = 'dropdown.slice/.*', main = true })
end

local function ddshow_doc(url)
    if url == nil then return end
    systemd.run(string.format("dwb -p docs %s", util.shell_escape(url)), "docs", false, "dropdown")
    local list = systemd.matching_clients({ cgroup = 'dropdown.slice/.*docs' })
    for i, c in ipairs(list) do
        show_client(c)
    end
end



-----------------------------------
-- Dropdown naughty boxes        --
-----------------------------------

box = {}

box.naughtylog = require("abdo.box.naughtylog")  -- log naughty notifications
box.orgtasks   = require("abdo.box.orgtasks")    -- org todo list
box.calendar   = require("abdo.box.calendar")    -- calendar
box.syslog     = require("abdo.box.syslog")      -- system log
box.userlog    = require("abdo.box.userlog")     -- user log



-----------------------------------
-- Prompts                       --
-----------------------------------
prompt = {}



function prompt.wikipedia()
    idoprompt.run(myw.promptbox[mouse.screen].widget,
                  "Wikipedia: ",
                  function (cmd)
                      local url = string.format("http://en.wikipedia.org/wiki/Special:Search?go=Go&search=\"%s\"", cmd)
                      ddshow_doc(url)
                  end,
                  nil,
                  awful.util.getdir("cache") .. "/history_wikipedia")
end


function prompt.mathscinet()
    idoprompt.run(myw.promptbox[mouse.screen].widget,
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

    promptl.run(myw.promptbox[mouse.screen].widget,
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

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Run: ",
                run,
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

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Dropdown: ",
                run,
                proglist,
                awful.util.getdir("cache") .. "/history_dropdown")
end


function prompt.lua()
    luaeval.run(myw.promptbox[mouse.screen].widget)
end
