---------------------------------------------------------------
-- File: autoload.lua       Autoloading programs             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local os = os
local string = string
local gears = gears
local hostname = hostname

local math = require("math")
local lfs = require("lfs")

local wallpapers_dir = awful.util.getdir("config") .. "/wallpapers/" .. hostname .. '/'

local path, mode
local wallpapers = {}
for wp in lfs.dir(wallpapers_dir) do
    path = wallpapers_dir .. wp
    mode = lfs.attributes(path, 'mode')
    if string.match(wp, ".+%.jpg$") and (mode == 'link' or mode == 'file') then
        table.insert(wallpapers, path)
    end
end

local i = math.random(1, #wallpapers)
local dual = string.match(wallpapers[i], ".+-dual%.jpg") ~= nil

if dual then
    gears.wallpaper.maximized(wallpapers[i], nil, nil)
else
    for s = 1, screen.count() do
        gears.wallpaper.maximized(wallpapers[i], s, nil)
    end
end
