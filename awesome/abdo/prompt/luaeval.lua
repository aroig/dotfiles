---------------------------------------------------------------
-- File:    luaeval.lua    Prompts for lua code to evaluate  --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


local prompt = require("abdo.prompt.prompt")
local beautiful = require("beautiful")
local ascreen = require("awful.screen")

local theme = beautiful.get()
local luaeval = {}

local function lua_completion (line, cur_pos, ncomp)
   -- Only complete at the end of the line, for now
   if cur_pos ~= #line + 1 then
      return line, cur_pos
   end

   -- We're really interested in the part following the last (, [, comma or space
   local lastsep = #line - (line:reverse():find('[[(, ]') or #line)
   local lastidentifier
   if lastsep ~= 0 then
      lastidentifier = line:sub(lastsep + 2)
   else
      lastidentifier = line
   end

   local environment = _G

   -- String up to last dot is our current environment
   local lastdot = #lastidentifier - (lastidentifier:reverse():find('.', 1, true) or #lastidentifier)
   if lastdot ~= 0 then
      -- We have an environment; for each component in it, descend into it
      for env in lastidentifier:sub(1, lastdot):gmatch('([^.]+)') do
         if not environment[env] then
            -- Oops, no such subenvironment, bail out
            return line, cur_pos
         end
         environment = environment[env]
      end
   end

   local tocomplete = lastidentifier:sub(lastdot + 1)
   if tocomplete:sub(1, 1) == '.' then
      tocomplete = tocomplete:sub(2)
   end

   local completions = {}
   for k, v in pairs(environment) do
      if type(k) == "string" and k:sub(1, #tocomplete) == tocomplete then
         table.insert(completions, k)
      end
   end

   if #completions == 0 then
      return line, cur_pos
   end

   while ncomp > #completions do
      ncomp = ncomp - #completions
   end

   local str = ""
   if lastdot + lastsep ~= 0 then
      str = line:sub(1, lastsep + lastdot + 1)
   end
   str = str .. completions[ncomp]
   cur_pos = #str + 1
   return str, cur_pos
end


local function prompt_output(s)
   naughty.notify({title = "Output", text=s , screen=awful.screen.focused() })
end


local function nice_eval(s)
   local f, err = loadstring("return "..s);
   if not f then
      f, err = loadstring(s);
   end

   if f then
      setfenv(f, _G);
      local ret = { pcall(f) };
      if ret[1] then
	 -- Ok
	 table.remove(ret, 1)
	 local highest_index = #ret;
	 for k, v in pairs(ret) do
	    if type(k) == "number" and k > highest_index then
	       highest_index = k;
	    end
	    ret[k] = select(2, pcall(tostring, ret[k])) or "<no value>";
	 end
	 -- Fill in the gaps
	 for i = 1, highest_index do
	    if not ret[i] then
	       ret[i] = "nil"
	    end
	 end
	 if highest_index > 0 then
	    prompt_output(awful.util.escape(tostring(table.concat(ret, ", "))));
	 else
	    prompt_output("Nothing");
	 end
      else
	 err = ret[2];
      end
   end
   if err then
      prompt_output(awful.util.escape(string.format("Error: %d", err)));
   end
end


function luaeval.run(promptwidget)
   prompt.run({ prompt = "<b>Lua:</b> ", font = theme.font_mono },
	      promptwidget,
	      nice_eval,
	      lua_completion,
	      awful.util.getdir("cache") .. "/history_eval")
end

return luaeval
