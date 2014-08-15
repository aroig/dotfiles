
local pairs = pairs
local ipairs = ipairs
local io = io
local os = os
local tonumber = tonumber
local string = string
local table = table
local beautiful = beautiful

local util = require("awful.util")
local abdoutil = require("abdo.util")

local wiki_dir = os.getenv("AB2_WIKI_DIR") .. "/org"

local char_width = 7.3
local parse_on_show = true
local limit_todo_length = nil
local max_lines = 40

local orgtasks = {}

orgtasks.todo = nil
orgtasks.data = nil
orgtasks.files = {}


local function get_org_filelist()
    local files = {}

    local fd = io.popen(string.format("find %s \\( -name '*.org' ! -name '.*' \\)", wiki_dir), "r")
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
    local text_color = beautiful.fg_normal or "#FFFFFF"
    local today_color = beautiful.fg_focus or "#00FF00"
    local event_color = beautiful.fg_urgent or "#FF0000"
    local priority_color = beautiful.color_org_priority
    local font = beautiful.font_box

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
                        task_date = y .. '-' .. m .. '-' .. d
                        pretty_date = os.date("%a %d %b %Y", os.time({year = tonumber(y),
                                                                      month = tonumber(m),
                                                                      day = tonumber(d)}))
                    end

                    if d and task_name then
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
                        if task_date < today then
                            pretty_date = "Overdue"
                        end

                        local len = string.len(task_name) + string.len(task_tags)
                        if (len > data.maxlen) then
                            data.maxlen = len
                        end
                        table.insert(data.tasks, { name = task_name,
                                                   tags = task_tags,
                                                   date = task_date,
                                                   pretty_date = pretty_date})
                        data.dates[y .. tonumber(m) .. tonumber(d)] = true
                    end
                end
                _, _, task_name = string.find(line, "%*+%s+TODO%s+(.+)")
            end
        end
    end
    table.sort(data.tasks, function (a, b) return a.date < b.date end)
    return data
end


local function create_todo(data)
    local text_color = beautiful.fg_normal or "#FFFFFF"
    local today_color = beautiful.fg_focus or "#00FF00"
    local event_color = beautiful.fg_urgent or "#FF0000"
    local priority_color = beautiful.color_org_priority
    local font = beautiful.font_box

    local result = ""
    local maxlen = data.maxlen + 3
    if limit_todo_length and limit_todo_length < maxlen then
        maxlen = limit_todo_length
    end

    local today = os.date("%Y-%m-%d")
    local prev_date, limit, tname, date
    local nlines = 0
    for i, task in ipairs(data.tasks) do
        if task.date < today then
            date = "overdue"
        else
            date = task.date
        end

        if prev_date ~= date then
            result = result ..
                string.format('<br><span weight = "bold" style = "italic" foreground = "%s">%s</span>\n',
                              event_color,
                              pop_spaces(task.pretty_date, "", maxlen))
            nlines = nlines + 2
        end
        tname = task.name
        limit = maxlen - string.len(task.tags) - 3
        if limit < string.len(tname) then
            tname = string.sub(tname, 1, limit - 3) .. "..."
        end
        result = result .. pop_spaces(tname, task.tags, maxlen)
        nlines = nlines + 1

        if i < #data.tasks and nlines <= max_lines then
            result = result .. "\n"
        else
            break
        end

        prev_date = date
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
    orgtasks.files = get_org_filelist()
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
