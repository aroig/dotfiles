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
local dropdown  = require("abdo.dropdown")         -- dropdown clients
local luaeval   = require("abdo.prompt.luaeval")   -- evaluation of lua code
local promptl   = require("abdo.prompt.list")      -- user choice from a list
local idoprompt = require("abdo.prompt.idoprompt")
local systemd   = require("abdo.systemd")



-----------------------------------
-- Initialization                --
-----------------------------------

ddclient = {}

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

-- initialize systemd signals to capture cgroups
systemd.init()



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
    if ns == 'systemd' then
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
    awful.client.movetotag(awful.tag.selected(c.screen), c)

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

function show_cgroup(pat, cmd)
    local list = systemd.matching_clients(pat)
    if #list > 0 then
        for i, c in ipairs(list) do
            show_client(c)
        end
    elseif cmd ~= nil then
        run(cmd)
    end
end


function hide_cgroup(pat)
    local list = systemd.matching_clients(pat)
    for i, c in ipairs(list) do
        hide_client(c)
    end
end


function kill_cgroup(pat)
    local list = systemd.matching_clients(pat)
    for i, c in ipairs(list) do
        kill_client(c)
    end
end


function toggle_cgroup(pat, cmd)
    local list = systemd.matching_clients(pat)
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
    ns, entry = name:match("^([^:]*):(.*)$")

    if ns == nil then
        entry = name
    end

    toggle_cgroup(string.format('dropdown.slice/.*%s', entry), name)
end

function ddshow(name)
    ns, entry = name:match("^([^:]*):(.*)$")

    if ns == nil then
        entry = name
    end

    show_cgroup(string.format('dropdown.slice/.*%s', entry), name)
end

function ddhide(name)
    ns, entry = name:match("^([^:]*):(.*)$")

    if ns == nil then
        entry = name
    end

    hide_cgroup(string.format('dropdown.slice/.*%s', entry))
end



-----------------------------------
-- Dropdown apps on the top      --
-----------------------------------

local ddgeometry = {
    top   = { vert="top",    horiz="center", width=1.0, height=0.4 },
    left  = { vert="center", horiz="left",   width=0.6, height=1.0 },
    right = { vert="center", horiz="right",  width=0.6, height=1.0 },
    full  = { vert="center", horiz="center", width=1.0, height=1.0 },
}

ddclient.terminal = dropdown.new("terminal",
                                 apps.termcmd(nil, "dropdown-terminal"),
                                 ddgeometry['top'])

ddclient.ranger   = dropdown.new("ranger",
                                 apps.termcmd("ranger", "dropdown-ranger"),
                                 ddgeometry['top'])


function ddclient.ranger.newtab(dd, path)
    if dd.run.client then
        dd.run.onraise_hook = function (run)
            naughty.notify({title = "New Tab", text=path, timeout=3})

            awful.util.spawn_with_shell("sleep 0.1;"
                                        .. "xdotool key --delay 100 Escape;"
                                        .. string.format("xdotool type --delay 15 ':tab_new %s';", path)
                                        .. 'xdotool key --delay 20 Return;')
            run.onraise_hook = nil
        end
        dd:show()
    else
        -- TODO: launch and go to the right directory
        dd:show()
    end

end

function ddclient.terminal.newtab(dd, path)
    if dd.run.client then
        dd.run.onraise_hook = function (run)
            naughty.notify({title = "New Tab", text=path, timeout=3})

            awful.util.spawn_with_shell("sleep 0.1;"
                                        .. "xdotool key --delay 15 Control+a;"
                                        .. "xdotool key --delay 15 c;"
                                        .. string.format("xdotool type --delay 15 'cd \"%s\"';", path)
                                        .. 'xdotool key --delay 20 Return;'
                                        .. 'xdotool type clear;'
                                        .. 'xdotool key --delay 20 Return;'
                                       )
            run.onraise_hook = nil
        end
        dd:show()
    else
        -- TODO: launch and go to the right directory
        dd:show()
    end
end



-----------------------------------
-- Dropdown apps on the left     --
-----------------------------------

ddclient.document = dropdown.new("browser", nil,
                                 {vert="center", horiz="right", width=0.7, height=1})



-- NOTE: If using a browser supporting tabs
-- do not kill old client if command changes, as chromium opens new tab
-- ddclient.document.kill_old = False



-----------------------------------
-- Functions                     --
-----------------------------------

function ddclient.hide_all()
    dropdown.hide_all()
end

function ddclient.kill_all()
    dropdown.kill_all()
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
                      ddclient.document:show(string.format("%s '%s'", apps.docbrowser, url))
                  end,
                  nil,
                  awful.util.getdir("cache") .. "/history_wikipedia")
end


function prompt.mathscinet()
    idoprompt.run(myw.promptbox[mouse.screen].widget,
                  "Mathscinet: ",
                  function (cmd)
                      local url = string.format("http://www.ams.org/mathscinet/search/publications.html?review_format=html&pg4=ALLF&s4=\"%s\"", cmd)
                      ddclient.document:show(string.format("%s '%s'", apps.docbrowser, url))
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
                function (cmd) ddclient.document:show(cmd) end,
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


function prompt.lua()
    luaeval.run(myw.promptbox[mouse.screen].widget)
end
