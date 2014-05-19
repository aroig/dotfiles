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

local apps = apps

local util      = require("abdo.util")
local pickle    = require("abdo.pickle")           -- some crude pickling routines
local dropdown  = require("abdo.dropdown")         -- dropdown clients
local luaeval   = require("abdo.prompt.luaeval")   -- evaluation of lua code
local promptl   = require("abdo.prompt.list")      -- user choice from a list
local idoprompt = require("abdo.prompt.idoprompt")

local cmd_list_file = awful.util.getdir("config") .. "/lists/cmd-list.lua"
local doc_list_file = awful.util.getdir("config") .. "/lists/doc-list.lua"



-----------------------------------
-- Global variables              --
-----------------------------------

ddclient = {}
cmdlist  = pickle.load(cmd_list_file)
doclist  = pickle.load(doc_list_file)



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
-- Systemd stuff                 --
-----------------------------------

-- Execute an external program and connect the output to systemd journal
function sdexec (cmd, name)
    awful.util.spawn_with_shell(cmd .. string.format(' 2>&1 | systemd-cat -t %s', name))
end

-- Execute an external program as a systemd scope or service
function sdrun (cmd, name, scope, slice)
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


function sd_isactive(unit)
    local cmd = string.format("systemctl --user -q is-active %s",
                              util.shell_escape(unit))
    return os.execute(cmd)
end


-- start a systemd unit. If unit is an unspecified instance of an @ unit
-- instantiate it as the first nonexistent instance it finds!
function sdstart (unit)
    local shcmd
    local startunit=unit
    if string.match(unit, '.*@%.service') then
        for i=0,100 do
            local newunit = string.gsub(unit, '@%.',
                                        string.format('@%d.', i))

            if not sd_isactive(newunit) then
                startunit = newunit
                break
            end
        end
    end

    shcmd = string.format('systemctl --user start %s',
                          util.shell_escape(startunit))

    awful.util.spawn_with_shell(shcmd)
end



-----------------------------------
-- Dropdown apps on the top      --
-----------------------------------

local dropdown_geometry = {vert="top", horiz="center", width=1, height=0.4}

ddclient.terminal = dropdown.new("terminal",
                                 apps.termcmd(nil, "dropdown-terminal"),
                                 dropdown_geometry)

ddclient.ranger   = dropdown.new("ranger",
                                 apps.termcmd("ranger", "dropdown-ranger"),
                                 dropdown_geometry)

ddclient.sage     = dropdown.new("sage",
                                 apps.termcmd("sage", "dropdown-sage"),
                                 dropdown_geometry)

ddclient.octave   = dropdown.new("octave",
                                 apps.termcmd("octave", "dropdown-octave"),
                                 dropdown_geometry)

ddclient.notes    = dropdown.new("notes",
                                 apps.notes,
                                 dropdown_geometry)

ddclient.syslog   = dropdown.new("syslog",
                                 apps.termcmd(apps.syslog, "dropdown-syslog"),
                                 dropdown_geometry)



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


ddclient.dict    = dropdown.new("dictionary", apps.dictionary,
                                {vert="center", horiz="right", width=0.5, height=1})

ddclient.calibre = dropdown.new("calibre", apps.library,
                                {vert="center", horiz="right", width=1,   height=1})

ddclient.chat    = dropdown.new("chat", apps.chat,
                                {vert="center", horiz="left", width=0.6, height=1})

ddclient.orgmode = dropdown.new("orgmode", apps.orgmode,
                                {vert="center", horiz="left", width=1, height=1})

ddclient.mail    = dropdown.new("mail", apps.mail,
                                {vert="center", horiz="left", width=1, height=1})

ddclient.music   = dropdown.new("music", apps.music,
                                {vert="center", horiz="right", width=0.7, height=1})

ddclient.document = dropdown.new("browser", nil,
                                 {vert="center", horiz="right", width=0.7, height=1})

ddclient.xournal = dropdown.new("xournal", apps.xournal,
                                {vert="center", horiz="left", width=0.5, height=1})


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
-- Run a command                 --
-----------------------------------

function run(name, opts)

    opts = opts or {ask = false}
    local ask = opts['ask']

    local cmd = cmdlist[name]
    if not cmd then
        naughty.notify({title="Error in run",
                        text=string.format("Unknown command name '%s'", name)})
        return
    end

    -- detect mode of execution
    if string.match(cmd, '^.*%.service%s*$') or string.match(cmd, '^.*%.target%s*$') then
        mode = 'sdstart'
    else
        mode = 'sdrun'
    end

    -- prepare the function that executes cmd
    if mode == 'sdrun' then
        exec_cmd = function() sdrun(cmd, name, true, 'apps') end

    elseif mode == 'sdstart' then
        exec_cmd = function() sdstart(cmd) end

    elseif mode == 'shrun' then
        exec_cmd = function () shexec(cmd) end

    end

    -- run the command
    if ask then
        local lst = { yes = true, no = false }

        promptl.run(myw.promptbox[mouse.screen].widget,
                    string.format("Run %s? ", name),
                    function (c) if c then exec_cmd() end end,
                    lst, nil)

    else
        exec_cmd()
    end
end



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
    doclist = pickle.load(doc_list_file)
    if not doclist then
        naughty.notify({title="Error in document prompt",
                        text=string.format("Can't load file '%s'", cmd_list_file)})
        return
    end

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Doc: ",
                function (cmd) ddclient.document:show(cmd) end,
                doclist,
                awful.util.getdir("cache") .. "/history_docs")
end


function prompt.command()
    cmdlist = pickle.load(cmd_list_file)
    if not cmdlist then
        naughty.notify({title="Error in command prompt",
                        text=string.format("Can't load file '%s'", cmd_list_file)})
        return
    end

    -- want the prompt to pass progname, not prog command to the callback
    local proglist = {}
    for name, cmd in pairs(cmdlist) do
        proglist[name] = name
    end

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Run: ",
                function (name) run(name) end,
                proglist,
                awful.util.getdir("cache") .. "/history_cmd")
end


function prompt.lua()
    luaeval.run(myw.promptbox[mouse.screen].widget)
end
