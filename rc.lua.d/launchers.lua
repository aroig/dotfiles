---------------------------------------------------------------
-- File:    launchers.lua         Launcher functions         --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


-----------------------------------
-- Loading Stuff                 --
-----------------------------------

drop = require("abdo.drop")                    -- Dropdown windows
luaeval = require("abdo.prompt.luaeval")       -- Evaluation of lua code
promptl = require("abdo.prompt.list")          -- Returns a selection from a list
idoprompt = require("abdo.prompt.idoprompt")


-- Setup drop manage signal
drop.setup()


-----------------------------------
-- Prompts                       --
-----------------------------------

prompt = {}

function prompt.wikipedia ()
   idoprompt.run(myw.promptbox[mouse.screen].widget,
		 "Wikipedia: ",
		 function (cmd)
		    local url = string.format(" http://en.wikipedia.org/wiki/Special:Search?go=Go&search=\"%s\"", cmd)
		    drop("doc", apps.docbrowser .. url, "center", "right", 0.6, 1)
		 end,
		 nil,
		 awful.util.getdir("cache") .. "/history_wikipedia")
end


function prompt.docs()
   docs = pickle.load(awful.util.getdir("config") .. "/lists/docs.list")
   if not docs then return end

   promptl.run(myw.promptbox[mouse.screen].widget,
	       "Doc: ",
	       function (cmd)
		  drop.toggle("doc", cmd, "center", "right", 0.7, 1)
	       end,
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

