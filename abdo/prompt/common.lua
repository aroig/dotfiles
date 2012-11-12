
local table = table
local util = require("awful.util")

local common = {}

-- Load history file in history table
-- @param id The history identifier which is the path to the filename
-- @param max Optionalbasdasd parameter, the maximum number of entries in file
function common.history_check_load(history, id, max)
    if id and id ~= ""
        and not history[id] then
	history[id] = { max = 50, table = {} }

	if max then
            history[id].max = max
	end

	local f = io.open(id, "r")

	-- Read history file
	if f then
            for line in f:lines() do
                if util.table.hasitem(history[id].table, line) == nil then
                        table.insert(history[id].table, line)
                        if #history[id].table >= history[id].max then
                           break
                        end
                end
            end
            f:close()
	end
    end
end

-- Save history table in history file
-- @param id The history identifier
function common.history_save(history, id)
    if history[id] then
        local f = io.open(id, "w")
        if not f then
            local i = 0
            for d in id:gmatch(".-/") do
                i = i + #d
            end
            util.mkdir(id:sub(1, i - 1))
            f = assert(io.open(id, "w"))
        end
	for i = 1, math.min(#history[id].table, history[id].max) do
            f:write(history[id].table[i] .. "\n")
        end
       f:close()
    end
end

-- Return the number of items in history table regarding the id
-- @param id The history identifier
-- @return the number of items in history table, -1 if history is disabled
function common.history_items(history, id)
    if history[id] then
        return #history[id].table
    else
        return -1
    end
end

-- Add an entry to the history file
-- @param id The history identifier
-- @param command The command to add
function common.history_add(history, id, command)
    if history[id] and command ~= "" then
        local index = util.table.hasitem(history[id].table, command)
        if index == nil then
            table.insert(history[id].table, command)

            -- Do not exceed our max_cmd
            if #history[id].table > history[id].max then
                table.remove(history[id].table, 1)
            end

            common.history_save(history, id)
        else
            -- Bump this command to the end of history
            table.remove(history[id].table, index)
            table.insert(history[id].table, command)
            common.history_save(history, id)
        end
    end
end



return common