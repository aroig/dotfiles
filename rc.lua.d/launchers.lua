---------------------------------------------------------------
-- File:    launchers.lua         Launcher functions         --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


-----------------------------------
-- Requires                      --
-----------------------------------

local os = os
local apps = apps

local pickle    = require("abdo.pickle")           -- some crude pickling routines
local dropdown  = require("abdo.dropdown")         -- dropdown clients
local luaeval   = require("abdo.prompt.luaeval")   -- evaluation of lua code
local promptl   = require("abdo.prompt.list")      -- user choice from a list
local idoprompt = require("abdo.prompt.idoprompt")


ddclient = {}


-----------------------------------
-- Useful functions              --
-----------------------------------

function shell_escape(s)
    s = (tostring(s) or ''):gsub('"', '\\"')

    if s:find('[^A-Za-z0-9_."/-]') then
        s = '"' .. s .. '"'
    elseif s == '' then
        s = '""'
    end

    return s
end


-- Execute an external program
function exec (cmd, screen)
    awful.util.spawn(cmd, false, screen)
end

-- Execute an external program inside a shell
function shexec (cmd, screen)
    awful.util.spawn_with_shell(cmd, screen)
end

-- Execute an external program and connect the output to systemd journal
function sdexec (cmd, screen, name)
    awful.util.spawn_with_shell(cmd .. string.format(' 2>&1 | systemd-cat -t %s', name), screen)
end

-- Execute an external program as a systemd scope
function sdrun (cmd, screen, name, scope, slice)
    local sdcmd = "systemd-run --user "
    if scope then sdcmd = sdcmd .. "--scope " end
    if slice then sdcmd = sdcmd .. string.format("--slice=\"%s\" ", slice) end
    if name  then sdcmd = sdcmd .. string.format("--description=\"%s\" ", name) end

    local pid = nil
    if scope then
        -- capture output to journal
        if name then cmd = string.format('%s 2>&1 | systemd-cat -t \"%s\"', cmd, name)
        else         cmd = string.format('%s 2>&1 | /dev/null', cmd)
        end

        -- do not catch stdout. The process does NOT end immediately
        awful.util.spawn_with_shell(string.format('%s sh -c %s 2>&1 > /dev/null', sdcmd, shell_escape(cmd)), screen)
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

-- executes a shell command on a terminal
function termcmd (cmd, title)
    if title then
        return string.format("%s -t \"%s\" -e \"%s\"", apps.terminal, title, cmd)
    else
        return string.format("%s -e \"%s\"", apps.terminal, cmd)
    end
end



-----------------------------------
-- Dropdown apps on the top      --
-----------------------------------

local dropdown_geometry = {vert="top", horiz="center", width=1, height=0.4}

ddclient.terminal = dropdown.new("terminal",
                                 termcmd("tmux-session drop", "dropdown-terminal"),
                                 dropdown_geometry)

ddclient.ranger   = dropdown.new("ranger",
                                 termcmd("ranger", "dropdown-ranger"),
                                 dropdown_geometry)

ddclient.sage     = dropdown.new("sage",
                                 termcmd("sage", "dropdown-sage"),
                                 dropdown_geometry)

ddclient.octave   = dropdown.new("octave",
                                 termcmd("octave", "dropdown-octave"),
                                 dropdown_geometry)

ddclient.notes    = dropdown.new("notes",
                                 apps.notes,
                                 dropdown_geometry)

ddclient.syslog   = dropdown.new("syslog",
                                 termcmd(apps.syslog, "dropdown-syslog"),
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
    docs = pickle.load(awful.util.getdir("config") .. "/lists/docs.list")
    if not docs then return end

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Doc: ",
                function (cmd) ddclient.document:show(cmd) end,
                docs,
                awful.util.getdir("cache") .. "/history_docs")
end


function prompt.command()
    cmds = pickle.load(awful.util.getdir("config") .. "/lists/commands.list")
    if not cmds then return end

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Run: ",
                function (cmd) sdrun(cmd, nil, nil, true, 'apps') end,
                cmds,
                awful.util.getdir("cache") .. "/history_cmd")
end


function prompt.lua()
    luaeval.run(myw.promptbox[mouse.screen].widget)
end


function prompt.ask_run(prompt, cmd)
    opts = {
        yes = cmd,
        no = "true",
    }

    promptl.run(myw.promptbox[mouse.screen].widget,
                string.format("%s? ", prompt),
                shexec,
                opts,
                nil)
end
