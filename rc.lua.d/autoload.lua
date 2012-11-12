---------------------------------------------------------------
-- File: autoload.lua       Autoloading programs             --
-- Version:                                                  --
-- Author: Abdó Roig<abdo.roig@gmail.com>                    --
---------------------------------------------------------------

-- exec("compton --config /home/abdo/.compton.conf") -- compositing manager
exec("killall cairo-compmgr")                     -- kill all cairo instances
exec("cairo-compmgr -n")                          -- compositing manager
exec("xset dpms 0 0 0")                           -- disable power saving

-- not needed anymore
-- exec("xsetroot -cursor_name left_ptr")         -- set cursor right

-- Host specific stuff
-- if hostname == "grothendieck" then
-- elseif hostname == "hodge" then
-- elseif hostname == "galois" then
-- end
