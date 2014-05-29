---------------------------------------------------------------
-- File:    util.lua            Personal utility functions   --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------


local math = require("math")

local util = {}

function util.dbg(vars)
    local text = ""
    for i=1, #vars do text = text .. vars[i] .. " | " end
    naughty.notify({ title = "Debug", text = text, timeout = 0 })
end


function util.set(t)
   local s = {}
   for _, l in ipairs(t) do
      s[l] = true
   end
   return s
end


function util.literalize(str)
    return str:gsub("[%(%)%.%%%+%-%*%?%[%]%^%$]", function(c) return "%" .. c end)
 end



function util.file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then
      io.close(f)
      return true
   else
      return false
   end
end



function util.shell_escape(s)
    local ret = tostring(s) or ''
    ret = ret:gsub('\\', '\\\\')
    ret = ret:gsub('"', '\\"')
    return '"' .. ret .. '"'
end


function util.pattern_escape(s)
    local ret = tostring(s)
    ret = s:gsub(".",
                 {
                     ["^"] = "%^",
                     ["$"] = "%$",
                     ["("] = "%(",
                     [")"] = "%)",
                     ["%"] = "%%",
                     ["."] = "%.",
                     ["["] = "%[",
                     ["]"] = "%]",
                     ["*"] = "%*",
                     ["+"] = "%+",
                     ["-"] = "%-",
                     ["?"] = "%?",
                     ["\0"] = "%z",
    })
    return ret
end


function util.debug(name, obj)
    local str = ""
    if type(obj) == "table" then
        for k, v in pairs(obj) do
            str = str .. string.format("%s = %s,\n", k, tostring(v))
        end

    else
        str = tostring(obj)
    end

    naughty.notify({title=string.format("Debug output: %s", name), text=str})
end


-----------------------------------
-- Graphic stuff                 --
-----------------------------------

function util.gradient(gradcols, min, max, value)
   local function color2dec(c)
      return tonumber(c:sub(2,3),16), tonumber(c:sub(4,5),16), tonumber(c:sub(6,7),16)
   end

   if value == nil then value = min end
   if value < min  then value = min end
   if value > max  then value = max end

   local nsegments = #gradcols - 1
   local width = max - min
   local t = nsegments * (value - min) / width
   local segment = math.floor(t)

   if segment == nsegments then return gradcols[#gradcols] end

   local colorA = gradcols[segment+1]
   local colorB = gradcols[segment+2]

   local redA, greenA, blueA = color2dec(colorA)
   local redB, greenB, blueB = color2dec(colorB)

   local red   = redA   + (t - segment) * (redB   - redA)
   local green = greenA + (t - segment) * (greenB - greenA)
   local blue  = blueA  + (t - segment) * (blueB  - blueA)

   -- dec2color
   return string.format("#%02x%02x%02x", red, green, blue)
end



return util
