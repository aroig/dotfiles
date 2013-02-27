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


-- Execute an external program
function exec (cmd, screen)
    awful.util.spawn(cmd, false, screen)
end

-- Execute an external program inside a shell
function shexec (cmd, screen)
    awful.util.spawn_with_shell(cmd, screen)
end


-- executes a shell command on a terminal
function termcmd (cmd)
    return string.format("%s -e %s", apps.terminal, cmd)
end



-----------------------------------
-- Dropdown apps on the top      --
-----------------------------------

ddclient.terminal = dropdown.new(string.format("%s -e tmux-session drop", apps.terminal),
                                 {vert="top", horiz="center", width=1, height=0.4})

ddclient.ranger   = dropdown.new(termcmd("ranger"),
                                 {vert="top", horiz="center", width=1, height=0.4})

ddclient.sage     = dropdown.new(termcmd("sage"),
                                 {vert="top", horiz="center", width=1, height=0.4})

ddclient.octave   = dropdown.new(termcmd("octave"),
                                 {vert="top", horiz="center", width=1, height=0.4})

ddclient.notes    = dropdown.new(apps.notes,
                                 {vert="top", horiz="center", width=1, height=0.4})

function ddclient.ranger.newtab(dd, path)
    if dd.run.client then
        dd.run.onraise_hook = function (run)
            naughty.notify({title = "New Tab", text=path, timeout=3})

            awful.util.spawn_with_shell("sleep 0.1;"
                                        .. string.format("xdotool key --delay 100 Escape type --delay 15 ':tab_new %s';", path)
                                        .. 'xdotool key --delay 20 Return;')
            run.onraise_hook = nil
        end
        dd:show()
    else
        dd:show(termcmd('ranger "%s"', path))
    end

end

function ddclient.terminal.newtab(dd, path)
    if dd.run.client then
        dd.run.onraise_hook = function (run)
            naughty.notify({title = "New Tab", text=path, timeout=3})

            awful.util.spawn_with_shell("sleep 0.1;"
                                        .. string.format("xdotool key --delay 15 Shift+Down;")
                                        .. string.format("xdotool type --delay 15 'cd \"%s\"';", path)
                                        .. 'xdotool key --delay 20 Return;'
                                        .. 'xdotool type clear;'
                                        .. 'xdotool key --delay 20 Return;'
                                       )
            run.onraise_hook = nil
        end
        dd:show()
    else
        dd:show(string.format('%s -pe tabbed -cd "%s"', apps.terminal, path))
    end
end



-----------------------------------
-- Dropdown apps on the right    --
-----------------------------------

ddclient.dict    = dropdown.new(apps.dictionary,
                                {vert="center", horiz="right", width=0.5, height=1})

ddclient.calibre = dropdown.new(apps.library,
                                {vert="center", horiz="right", width=1,   height=1})

ddclient.chat    = dropdown.new(apps.chat,
                                {vert="center", horiz="right", width=300, height=1})

ddclient.irc     = dropdown.new(apps.irc,
                                {vert="center", horiz="right", width=0.5, height=1})

ddclient.music   = dropdown.new(apps.music,
                                {vert="center", horiz="right", width=0.6, height=1})

ddclient.twitter = dropdown.new(apps.twitter,
                                {vert="center", horiz="right", width=0.4, height=1})

ddclient.document = dropdown.new(nil, {vert="center", horiz="right", width=0.7, height=1})
-- do not kill old client if command changes, as chromium opens new tab
ddclient.document.kill_old = False


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
                exec,
                cmds,
                awful.util.getdir("cache") .. "/history_cmd")
end


function prompt.lua()
    luaeval.run(myw.promptbox[mouse.screen].widget)
end
