
local pairs = pairs
local ipairs = ipairs
local io = io
local os = os
local tonumber = tonumber
local string = string
local table = table
local theme = theme or {}
local util = require("awful.util")
local abdoutil = require("abdo.util")

local agenda_files = "/home/abdo/Work/wiki/etc/agenda-files"

local char_width = 7.3

local text_color = theme.fg_normal or "#FFFFFF"
local today_color = theme.fg_focus or "#00FF00"
local event_color = theme.fg_urgent or "#FF0000"
local priority_color = theme.fg_org_priority

-- font = theme.font_mono or 'sans 8'
local font = theme.font_box

local parse_on_show = true
local limit_todo_length = nil

local orgtasks = {}

orgtasks.todo = nil
orgtasks.data = nil
orgtasks.files = {}

local function read_filelist(filelist)
   local files = {}
   local fd = io.open(filelist, "r")
   for line in fd:lines() do
      table.insert(files, line)
   end
   return files
end

local function pop_spaces(s1, s2, maxsize)
   local sps = ""
   for i = 1, maxsize - string.len(s1) - string.len(s2) do
      sps = sps .. " "
   end
   return s1 .. sps .. s2
end

local function parse_agenda(files)
   local today = os.date("%Y-%m-%d")
   local data = { tasks = {}, dates = {}, maxlen = 30 }

   local task_name
   for _, file in pairs(files) do
      local fd = io.open(file, "r")
      if not fd then
         print("W: orglendar: cannot find " .. file)
      else
         for line in fd:lines() do
            local scheduled = string.find(line, "SCHEDULED:")
            local closed    = string.find(line, "CLOSED:")
            local deadline  = string.find(line, "DEADLINE:")

            if (scheduled and not closed) or (deadline and not closed) then
               local _, _, y, m, d  = string.find(line, "(%d%d%d%d)%-(%d%d)%-(%d%d)")

	       local task_date
	       if y ~= nil or d ~= nil or m ~= nil then
		  task_date = y .. "-"  .. m .. "-" .. d
	       end


               if d and task_name and (task_date >= today) then
                  local find_begin, task_start = string.find(task_name, "[A-Z]+%s+")
                  if task_start and find_begin == 1 then
                     task_name = string.sub(task_name, task_start + 1)
                  end
                  local task_end, _, task_tags = string.find(task_name,"%s+(:.+):")
                  if task_tags then
                     task_name = string.sub(task_name, 1, task_end - 1)
                  else
                     task_tags = " "
                  end

                  local len = string.len(task_name) + string.len(task_tags)
                  if (len > data.maxlen) and (task_date >= today) then
                     data.maxlen = len
                  end
                  table.insert(data.tasks, { name = task_name,
                                             tags = task_tags,
                                             date = task_date})
                  data.dates[y .. tonumber(m) .. tonumber(d)] = true
               end
            end
            _, _, task_name = string.find(line, "%*+%s+(.+)")
         end
      end
   end
   table.sort(data.tasks, function (a, b) return a.date < b.date end)
   return data
end


local function create_todo(data)
   local result = ""
   local maxlen = data.maxlen + 3
   if limit_todo_length and limit_todo_length < maxlen then
      maxlen = limit_todo_length
   end
   local prev_date, limit, tname
   for i, task in ipairs(data.tasks) do
      if prev_date ~= task.date then
         result = result ..
            string.format('<br><span weight = "bold" style = "italic" foreground = "%s">%s</span>\n',
                          event_color,
                          pop_spaces(task.date, "", maxlen))
      end
      tname = task.name
      limit = maxlen - string.len(task.tags) - 3
      if limit < string.len(tname) then
         tname = string.sub(tname, 1, limit - 3) .. "..."
      end
      result = result .. pop_spaces(tname, task.tags, maxlen)

      if i ~= table.getn(data.tasks) then
         result = result .. "\n"
      end
      prev_date = task.date
   end
   if result == "" then
      result = " "
   end

   for p, col in pairs(priority_color) do
      result = string.gsub(result, abdoutil.literalize(p), string.format('<span color="%s">%s</span>', col, p))
   end


   return string.format('<span font="%s" foreground="%s">%s</span>',
                        font, text_color, result), data.maxlen + 3
end




function orgtasks.add_todo()
   orgtasks.remove_todo()
   orgtasks.files = read_filelist(agenda_files)
   orgtasks.data = parse_agenda(orgtasks.files)
   local datastr = create_todo(orgtasks.data)
   orgtasks.todo = naughty.notify({ title = "Tasks",
			   text = datastr,
			   timeout = 0
			})
end


function orgtasks.remove_todo()
   if orgtasks.todo ~= nil then
      naughty.destroy(orgtasks.todo)
      orgtasks.todo = nil
      orgtasks.files = {}
   end
end

function orgtasks.toggle_todo()
   if orgtasks.todo == nil then
       orgtasks.add_todo()
   else
      orgtasks.remove_todo()
   end
end


return orgtasks