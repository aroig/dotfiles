---------------------------------------------------------------------------
-- @author Abdo Roig-Maranges &lt;abdo.roig@gmail.com&gt;
-- @copyright 2012 Abdo Roig-Maranges
-- @release v3.4.10
---------------------------------------------------------------------------



-- Grab environment we need
local assert = assert
local io = io
local table = table
local math = math
local ipairs = ipairs
local pcall = pcall
local capi =
{
    selection = selection
}
local naughty = naughty

local keygrabber = require("awful.keygrabber")
local common = require("abdo.prompt.common")
local string = require("string")
local util = require("awful.util")
local beautiful = require("beautiful")

-- Config stuff
local theme = beautiful.get()
local config = {}
config.font = theme.font_mono or "sans 8"
config.history_max = nil
config.completion_max = 6
config.text_color = theme.fg_normal
config.selected_color = theme.fg_urgent

-- Data
local data = {}
data.history = {}

local idoprompt = {}

-- ido prompt formating
local function format_completion_string(matches, sel)
   local compstr = ""
   local max = #matches
   local min = max
   for i, c in ipairs(matches) do
      if min > i and sel - i < config.completion_max then min = i end
      if i - min < config.completion_max then max = i end

      if i >= min and i <= max then
	 if i > min then  compstr = compstr .. " | " end
	 if i == sel then
	    compstr = compstr .. string.format('<span weight="bold" color="%s">%s</span>', config.selected_color,  util.escape(c))
	 else
	    compstr = compstr .. util.escape(c)
	 end
      end
   end
   if min > 1 then compstr = "... " .. compstr end
   if max < #matches then compstr = compstr .. " ..." end

   return compstr
end



local function prompt_text_with_cursor(args, data)
   local char, spacer, text_start, text_end, ret
   local text = data.text or ""
   local prompt = args.prompt or ""
   local underline = args.cursor_ul or "none"

   ret = string.format('<span weight="bold">%s</span>%s ', prompt, util.escape(text))
   if #data.matches == 1 then
      ret = ret .. string.format('[<span weight="bold" color="%s">%s</span>]  ', config.selected_color, data.matches[1])
   elseif #data.matches > 1 then
      ret = ret .. string.format("{%s}  ", format_completion_string(data.matches, data.sel))
   end
--   ret = ret .. "cmd: " .. data.command
   if config.font then
      ret = string.format('<span font="%s" color="%s">%s</span>', config.font, config.text_color, ret)
   end

   return ret
end




--- Run a prompt in a box.
-- @param args A table with optional arguments: fg_cursor, bg_cursor, ul_cursor, prompt, text, selectall, font, autoexec.
-- @param textbox The textbox to use for the prompt.
-- @param exe_callback The callback function to call with command as argument when finished.
-- @param completion_callback The callback function to call to get completion.
-- @param history_path Optional parameter: file path where the history should be saved, set nil to disable history
-- @param history_max Optional parameter: set the maximum entries in history file, 50 by default
-- @param done_callback Optional parameter: the callback function to always call without arguments, regardless of whether the prompt was cancelled.
function idoprompt.run(textbox, prompt, exe_callback, completion_callback, history_path, done_callback)

   data.text = ""          -- Current text
   data.cur_pos = 1        -- cursor position
   data.history = {}       -- History
   data.matches = {}       -- Completion list
   data.command = ""       -- The command to be run
   data.sel = 1            -- Current completion

   common.history_check_load(data.history, history_path, config.history_max)
   local history_index = common.history_items(data.history, history_path) + 1

   local function update(p, d)
      textbox:set_markup(prompt_text_with_cursor({prompt=p}, d))
   end

   local exec = function(d)
      textbox:set_markup("")
      common.history_add(data.history, history_path,  d.command)
      keygrabber.stop(grabber)
      exe_callback(d.command)
      if done_callback then done_callback() end
   end

   local completions = function(d)
      if completion_callback ~= nil and #d.text >= 1 then
	 d.matches = completion_callback(d.text)
	 d.sel = 1
      else
	 d.matches = {}
	 d.sel = 1
      end
      if #d.matches > 0 then
	 d.command = d.matches[1]
      else
	 d.command = d.text
      end
   end

   update(prompt, data)

   grabber = keygrabber.run(
      function (modifiers, key, event)
	 if event ~= "press" then
	    return true
	 end

	 -- Convert index array to hash table
	 local mod = {}
	 for k, v in ipairs(modifiers) do mod[v] = true end

	 -- Get out cases
	 if (mod.Control and (key == "c" or key == "g")) or (not mod.Control and key == "Escape") then
            keygrabber.stop(grabber)
	    textbox:set_markup("")
	    if done_callback then done_callback() end
	    return false
	 elseif (mod.Control and (key == "j" or key == "m"))
	    or (not mod.Control and key == "Return")
            or (not mod.Control and key == "KP_Enter") then
            exec(data)
            -- We already unregistered ourselves so we don't want to return
            -- true, otherwise we may unregister someone else.
            return
	 end

	 -- Control cases
	 if mod.Control then
	    a=1+2                  -- TODO: Some cases here?
	 else

                                   -- TODO: Tab cases

            -- Typing cases
	    if mod.Shift and key == "Insert" then
	       local selection = capi.selection()
	       if selection then
		  -- Remove \n
		  local n = selection:find("\n")
		  if n then
		     selection = selection:sub(1, n - 1)
		  end
		  data.text = data.text:sub(1, data.cur_pos - 1) .. selection .. command:sub(data.cur_pos)
		  data.cur_pos = data.cur_pos + #selection
	       end
--          elseif key == "Home" then
--              data.cur_pos = 1
--          elseif key == "End" then
--              data.cur_pos = #data.text + 1
            elseif key == "BackSpace" then
                if data.cur_pos > 1 then
                    data.text = data.text:sub(1, data.cur_pos - 2) .. data.text:sub(data.cur_pos)
                    data.cur_pos = data.cur_pos - 1
		 end
                 completions(data)
--          elseif key == "Delete" then
--              data.text = data.text:sub(1, data.cur_pos - 1) .. data.text:sub(data.cur_pos + 1)
            elseif key == "Left" then
	       if  #data.matches > 1 and data.sel > 1 then
		  data.sel = data.sel - 1
		  data.command = data.matches[data.sel]
	       end
--             data.cur_pos = data.cur_pos - 1

            elseif key == "Right" then
	       if #data.matches > 1 and data.sel < #data.matches then
		  data.sel = data.sel + 1
		  data.command = data.matches[data.sel]
	       end
--	       data.cur_pos = data.cur_pos + 1
            elseif key == "Up" then
                if history_index > common.history_items(data.history, history_path) then
                    data.text_bak = data.text
                end
                if history_index > 1 then
                    history_index = history_index - 1

                    data.text = data.history[history_path].table[history_index]
                    data.command = data.text
                    data.cur_pos = #data.text + 2
		    completions(data)
                end
            elseif key == "Down" then
               if history_index < common.history_items(data.history, history_path) then
                    history_index = history_index + 1
                    data.text = data.history[history_path].table[history_index]
                    data.command = data.text
                    data.cur_pos = #data.text + 2
		    completions(data)
                elseif history_index == common.history_items(data.history, history_path) then
                    history_index = history_index + 1
                    data.text = data.text_bak or data.text
                    data.cur_pos = #data.text + 2
		    completions(data)
                end
            else
                -- wlen() is UTF-8 aware but #key is not,
                -- so check that we have one UTF-8 char but advance the cursor of # position
                if key:wlen() == 1 then
                    data.text = data.text:sub(1, data.cur_pos - 1) .. key .. data.text:sub(data.cur_pos)
                    data.cur_pos = data.cur_pos + #key
		 end
                 completions(data)
            end
            if data.cur_pos < 1 then
                data.cur_pos = 1
            elseif data.cur_pos > #data.text + 1 then
                data.cur_pos = #data.text + 1
            end
        end

	update(prompt, data)
	return true
     end)
 end

return idoprompt
