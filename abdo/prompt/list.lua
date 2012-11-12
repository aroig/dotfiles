---------------------------------------------------------------
-- File:    list.lua       Prompts for a choice from a list  --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


local io     = require("io")
local table  = require("table")

local util   = require("abdo.util")
local prompt = require("abdo.prompt.idoprompt")

local naughty = naughty



local list = {}

local function read_completion_table(file)
   fd = io.open(file, 'r')
   s = fd:read("*a")
   local gentables = loadstring("return "..s)
   return gentables() 
end





local function make_item_callbacks(list, callback)
   local comp_table = list
   local function item_completion (text)
      local matches = {}

      local pattern = util.literalize(text)
      pattern = string.gsub(pattern, "%s", ".*")

      for k, v in pairs(comp_table) do
	 if k:find(pattern) then
	    table.insert(matches, k)
	 end     
      end
      return matches
   end
   
   local function item_eval(key)
      val = comp_table[key]

      if val ~= nil then
	 callback(val)
      end
   end

   return item_eval, item_completion
end





function list.run(promptwidget, promptstr, callback, list, histfile)
   eval_callback, comp_callback = make_item_callbacks(list, callback)

   prompt.run(promptwidget,
	      promptstr,
	      eval_callback,
	      comp_callback,
	      histfile)
end


return list