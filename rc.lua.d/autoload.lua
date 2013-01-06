---------------------------------------------------------------
-- File: autoload.lua       Autoloading programs             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

local hostname = hostname

exec("killall cairo-compmgr")                     -- kill all cairo instances

-- no compositing on galois. saves battery!
if hostname ~= "galois" then
    exec("cairo-compmgr -n")                      -- compositing manager
end

exec("xset dpms 0 0 0")                           -- disable power saving

-- not needed anymore
-- exec("xsetroot -cursor_name left_ptr")         -- set cursor right

-- Host specific stuff
-- if hostname == "grothendieck" then
-- elseif hostname == "hodge" then
-- elseif hostname == "galois" then
-- end
