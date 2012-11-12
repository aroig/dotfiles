-------------------------------------------------------------------
-- Drop-down applications manager for the awesome window manager
-- Abdo.
-- I've added two parameters for the dropdown, a name and a command.
-- If name & command coincide with a current window, raise it
-- If name coincides but command is diferent, kill old and create new
-------------------------------------------------------------------
-- Coded  by: * Lucas de Vries <lucas@glacicle.com>
-- Hacked by: * Adrian C. (anrxc) <anrxc@sysphere.org>
-- Licensed under the WTFPL version 2
--   * http://sam.zoy.org/wtfpl/COPYING
-------------------------------------------------------------------
-- To use this module add:
--   require("scratch")
-- to the top of your rc.lua, and call it from a keybinding:
--   scratch.drop(prog, vert, horiz, width, height, sticky, screen)
--
-- Parameters:
--   prog   - Program to run; "urxvt", "gmrun", "thunderbird"
--   vert   - Vertical; "bottom", "center" or "top" (default)
--   horiz  - Horizontal; "left", "right" or "center" (default)
--   width  - Width in absolute pixels, or width percentage
--            when <= 1 (1 (100% of the screen) by default)
--   height - Height in absolute pixels, or height percentage
--            when <= 1 (0.25 (25% of the screen) by default)
--   sticky - Visible on all tags, false by default
--   screen - Screen (optional), mouse.screen by default
-------------------------------------------------------------------

-- Grab environment
local pairs = pairs
local awful = require("awful")
local naughty = require("naughty")
local os = require("os")

local setmetatable = setmetatable
local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}

local util = require("abdo.util")

local drop = {}
drop.data = {}

local function client_geometry(c, vert, horiz, width, height, screen)
   local screengeom = capi.screen[screen].workarea

   if width  <= 1 then width  = screengeom.width  * width end
   if height <= 1 then height = screengeom.height * height end

   if     horiz == "left"  then x = screengeom.x
   elseif horiz == "right" then x = screengeom.x + screengeom.width - width
   else   x =  screengeom.x + (screengeom.width - width)/2
   end

   if     vert == "bottom" then y = screengeom.y + screengeom.height - height
   elseif vert == "top" then    y = screengeom.y
   else                         y = screengeom.y + (screengeom.height - height)/2
   end

   c:geometry({ x = x, y = y, width = width - 2*c.border_width, height = height - 2*c.border_width })
end


local function spawn_client_pid(prog, cmd, vert, horiz, width, height, sticky, screen)

   local time = os.time()

   -- Spawn a new instance
   if drop.data[prog] == nil then
      drop.data[prog] = {}
      drop.data[prog].command=cmd
      drop.data[prog].time=time
      naughty.notify({title = "Spawning", text=prog, timeout=3})
      drop.data[prog].pid = awful.util.spawn(cmd, false)
      drop.data[prog].callback = function (c)
	 drop.data[prog].client=c

	 -- Scratchdrop clients are floaters
	 awful.client.floating.set(c, true)

	 -- Client properties
	 c.ontop = true
	 c.above = true
	 c.skip_taskbar = true
	 if sticky then c.sticky = true end
	 if c.titlebar then awful.titlebar.remove(c) end

	 c:raise()
	 capi.client.focus = c

	 -- Put it in correct screen
	 awful.client.movetoscreen(c, screen)

	 -- Client geometry
	 client_geometry(c, vert, horiz, width, height, screen)
      end
   end
end



local function spawn_client(prog, cmd, vert, horiz, width, height, sticky, screen)
   if drop.data[prog] == nil then
      drop.data[prog] = {}
      drop.data[prog].command=cmd
   end

   spawnw = function (c)
               -- Remove manage signal
               local _spawnw = spawnw

	       capi.client.disconnect_signal("manage", _spawnw)

	       drop.data[prog].client=c

	       -- Scratchdrop clients are floaters
	       awful.client.floating.set(c, true)

	       -- Client properties
	       c.ontop = true
	       c.above = true
	       c.skip_taskbar = true
	       if sticky then c.sticky = true end
	       if c.titlebar then awful.titlebar.remove(c) end

	       c:raise()
	       capi.client.focus = c

	       -- Put it in correct screen
	       awful.client.movetoscreen(c, screen)

	       -- Client geometry
	       client_geometry(c, vert, horiz, width, height, screen)

	       -- Add unmanage signal
	       capi.client.connect_signal("unmanage",
					  function (cc)
					     if c == cc then
						drop.data[prog] = nil
					     end
					  end)
	    end

   -- Add manage signal and spawn the program
   capi.client.connect_signal("manage", spawnw)
   local pid = awful.util.spawn(cmd, false)
   drop.data[prog].pid = pid
   naughty.notify({title = "Spawning", text=prog, timeout=3})
end


local function raise_client(c, vert, horiz, width, height, sticky, screen)

   -- Make sure it is centered
--   if vert  == "center" then awful.placement.center_vertical(c)   end
--   if horiz == "center" then awful.placement.center_horizontal(c) end

   c.hidden = false
   c:raise()
   capi.client.focus = c
   -- Client geometry
   client_geometry(c, vert, horiz, width, height, screen)
end


local function hide_client(c)
   c.hidden = true
   local ctags = c:tags()
   for i, t in pairs(ctags) do
      ctags[i] = nil
   end
   c:tags(ctags)
end

local function kill_client(c)
   if c ~= nil then
      c:kill()
   end
end


local function process_alive(pid)
   f = io.popen(string.format("ps -eo pid | grep %d", pid))
   ret = f:read("*all")
   f:close()
   return ret ~= ""
end


-- Create a new window for the drop-down application when it doesn't
-- exist, or toggle between hidden and visible states when it does
function drop.toggle(prog, cmd, vert, horiz, width, height, sticky, screen)
    vert   = vert   or "top"
    horiz  = horiz  or "center"
    width  = width  or 1
    height = height or 0.40
    sticky = sticky or false
    screen = screen or capi.mouse.screen

     -- if cmd is nil, use the one of running client, if exist, otherwise do nothing
     if not cmd then
	if drop.data[prog] then
	   cmd = drop.data[prog].command
	else
	   return
	end
     end

     -- If the process is not running, clean data.
     if drop.data[prog] and not process_alive(drop.data[prog].pid) then
	drop.data[prog] = nil
     end

     -- If there is a client
     if drop.data[prog] and drop.data[prog].client then

        -- Get a running client
        c = drop.data[prog].client

        -- If running client was launched with same command as requested
        if cmd == drop.data[prog].command then
	   -- Switch the client to the current workspace
	   if c:isvisible() == false then c.hidden = true
	      awful.client.movetotag(awful.tag.selected(screen), c)
	   end

	   if c.hidden then
	      raise_client(c, vert, horiz, width, height, sticky, screen)
	   else
	      hide_client(c)
	   end

	-- If not the same command, kill it.
	else
	   kill_client(c)
	   drop.data[prog] = nil
        end
     end

     -- Spawn a new client if necessary
     if not drop.data[prog] then
	spawn_client_pid(prog, cmd, vert, horiz, width, height, sticky, screen)
     end
end

-- Sets up the signal
function drop.setup()
   client.connect_signal("manage",
			 function (c)
			    for k,v in pairs(drop.data) do
			       if v.pid == c.pid and v.client == nil then
				  v.client = c

				  -- Signal to umanage client
				  capi.client.connect_signal("unmanage",
							     function (cc)
								if c == cc and drop.data[k] then
								   drop.data[k].client = nil
								end
							     end)

				  v.callback(c)
				  break
			       end
			    end

			 end)
end


return drop
