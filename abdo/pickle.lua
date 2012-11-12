---------------------------------------------------------------
-- File:    pickle.lua            Pickling lua objects       --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

-- Rather crude serialization thing from the internets


local pickle = {}

local objects = {}
setmetatable(objects, {__index={["subset"]=function(object, proxies)
    for _,o in ipairs(proxies) do
        if object == o then return true end
    end
end}})

function pickle._pickle(object, seen, indent)
    --if not seen then seen = {} end
    if not indent then indent = "" end

    local serialize_key = function(key)
        if type(key) == "string" then
            return "[\""..key.."\"]"
        elseif type(key) == "table" then
            return "[".. pickle._pickle(key):gsub("\n"," ").."]"
        else
            return "["..key.."]"
        end
        return key
    end

    local escape = function(o)
        return o:gsub("\\","\\\\"):gsub("'","\\'"):gsub('"','\\"')
    end

    --Switch Object type:
    if type(object) == "table" then
        local serialize = "{\n"
        for key, value in pairs(object) do
            serialize = serialize .. indent.."\t" .. serialize_key(key) .. " = " .. tostring(pickle._pickle(value, seen, indent.."\t")) .. ",\n"
        end
        serialize = serialize .. indent .. "}"

        return serialize
    elseif type(object) == "string" then
        return '"' .. escape(object) .. '"'
    elseif type(object) == "function" then
        return "loadstring([["..string.dump(object).."]])"
    elseif objects.subset(object, {"userdata"}) then
        return nil
    end
    return tostring(object)
end

function pickle.dumps(object)
    return "return ".. pickle._pickle(object)
end

function pickle.dump(object, filename)
    local dump = pickle.dumps(object)
    local _file = io.open(filename, "wb")
    _file:write(dump)
    _file:close()
    return dump
end

function pickle.loads(object)
    local fn = loadstring(object)
    if fn then
        return fn()
    end
end

function pickle.load(filename)
    local _file = io.open(filename, "rb")
    local dump = _file:read("*all")
    local object = pickle.loads(dump)
    _file:close()
    return object
end


return pickle
