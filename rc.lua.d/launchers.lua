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

local pickle    = require("abdo.pickle")           -- Some crude pickling routines
local drop      = require("abdo.drop")             -- Dropdown clients
local luaeval   = require("abdo.prompt.luaeval")   -- Evaluation of lua code
local promptl   = require("abdo.prompt.list")      -- User choice from a list
local idoprompt = require("abdo.prompt.idoprompt")

-- Setup drop manage signal
drop.setup()


-----------------------------------
-- Dropdown apps                 --
-----------------------------------
dropdown = {}

-- Dropdowns on the top of the screen

function dropdown.terminal(path)
    local matchcmd = (path ~= nil)
    local path = path or os.getenv("PWD")
    drop.toggle("terminal", apps.terminal .. " -cd " .. path,
                "top", "center", 1, 0.4, nil, nil, matchcmd)
end

function dropdown.ranger(path)
    local matchcmd = (path ~= nil)
    local path = path or os.getenv("PWD")
    drop.toggle("ranger", apps.terminal .. " -e ranger " .. path,
                "top", "center", 1, 0.4, nil, nil, matchcmd)
end

function dropdown.sage()
    drop.toggle("sage", apps.terminal .. " -e sage",
                "top", "center", 1, 0.4, nil, nil, false)
end

function dropdown.octave()
    drop.toggle("octave", apps.terminal .. " -e octave",
                "top", "center", 1, 0.4, nil, nil, false)
end

function dropdown.notes()
    drop.toggle("notes", apps.notes,
                "top", "center", 1, 0.4, nil, nil, false)
end


-- Other dropdown style clients

function dropdown.doc(cmd)
    local matchcmd = (cmd ~= nil)
    drop.toggle("doc", cmd,
                "center", "right", 0.7, 1, nil, nil, matchcmd)
end

function dropdown.dict()
    drop.toggle("goldendict", "goldendict",
                "center", "right", 0.5, 1, nil, nil, false)
end

function dropdown.calibre()
    drop.toggle("calibre", "calibre",
                "center", "right", 1, 1, nil, nil, false)
end

function dropdown.chat()
    drop.toggle("chat", apps.chat,
                "center", "right", 300, 1, nil, nil, false)
end

function dropdown.irc()
    drop.toggle("irc", apps.irc,
                "center", "right", 0.5, 1, nil, nil, false)
end

function dropdown.music()
    drop.toggle("music", apps.music,
                "center", "right", 0.6, 1, nil, nil, false)
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
                      dropdown.doc(apps.docbrowser .. " " .. url)
                  end,
                  nil,
                  awful.util.getdir("cache") .. "/history_wikipedia")
end


function prompt.docs()
    docs = pickle.load(awful.util.getdir("config") .. "/lists/docs.list")
    if not docs then return end

    promptl.run(myw.promptbox[mouse.screen].widget,
                "Doc: ",
                function (cmd) dropdown.doc(cmd) end,
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
