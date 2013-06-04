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
local os = os
local setmetatable = setmetatable

local capi = {
    mouse = mouse,
    client = client,
    screen = screen
}

local awful   = awful
local naughty = naughty
local util    = require("abdo.util")


local dropdown = { mt = {}, data = {} }


local function raise_client(run)
    if not run.client then
        return
    end

    local geom = run.geom
    local screengeom = capi.screen[run.screen].workarea
    local width, height, x, y

    -- move to the right tag
    awful.client.movetotag(awful.tag.selected(run.screen), run.client)

    -- if width or height values in the interval [0,1] represent a fraction of screen width or height.
    if geom.width  <= 1 then width  = screengeom.width  * geom.width
    else                     width  = geom.width
    end

    if geom.height <= 1 then height = screengeom.height * geom.height
    else                     height = geom.height
    end

    -- horizontal alignment: top, bottom, center
    if     geom.horiz == "left"  then x = screengeom.x
    elseif geom.horiz == "right" then x = screengeom.x + screengeom.width - width
    else                              x =  screengeom.x + (screengeom.width - width)/2
    end

    -- vertical alignment: top, bottom, center
    if     geom.vert == "bottom" then y = screengeom.y + screengeom.height - height
    elseif geom.vert == "top" then    y = screengeom.y
    else                              y = screengeom.y + (screengeom.height - height)/2
    end

    -- set the geometry of the client
    local bw = run.client.border_width
    run.client:geometry({ x = x, y = y, width = width - 2*bw, height = height - 2*bw })

    -- raise the client
    run.client.hidden = false
    run.client:raise()
    capi.client.focus = run.client

    -- run onraise hook
    if run.onraise_hook then
        run:onraise_hook()
    end

    run.visible = true
end


local function hide_client(run)
    if run.client then
        run.visible = false
        run.client.hidden = true
        local ctags = run.client:tags()
        for i, t in pairs(ctags) do
            ctags[i] = nil
        end
        run.client:tags(ctags)
    end
end


local function refresh_state(run)
    if run.pid then
        local pid = run.pid
        local f = io.popen(string.format("ps -eo pid | grep %d", pid))
        local ret = f:read("*all")
        f:close()

        -- if the process is dead
        if ret == "" then
            run.pid = nil
            run.client = nil
            dropdown.data[pid] = nil
        end
    end
end


local function kill_client(run)
    if run.client then
        run.client:kill()
        run.client = nil
        run.pid = nil
        run.cmd = nil
    end
end



function dropdown.hide_all()
    for pid, run in pairs(dropdown.data) do
        refresh_state(run)
        hide_client(run)
    end
end



function dropdown.hide(dd)
    hide_client(dd.run)
end



function dropdown.kill_all()
    for pid, run in pairs(dropdown.data) do
        kill_client(run)
    end
end



function dropdown.toggle(dd, cmd, screen)
    refresh_state(dd.run)

    if dd.run.visible then    dd:hide()
    else                      dd:show(cmd, screen)
    end
end

-- auxiliar function to capture a client on a dropdown
function _capture(dd, cmd, pid, c)
    local screen = screen or dd.screen or capi.mouse.screen
    dd.run.cmd      = cmd
    dd.run.geom     = dd.geom
    dd.run.sticky   = dd.sticky or false
    dd.run.time     = os.time()
    dd.run.client   = c
    dd.run.screen   = screen
    dd.run.visible  = false

    if not dd.run.pid then
        dd.run.pid = pid
        dropdown.data[dd.run.pid] = dd.run
    end
end


-- show a dropdown. Launch it if not running
function dropdown.show(dd, cmd, screen)
    refresh_state(dd.run)

    -- kill old client if necessary
    if dd.kill_old and cmd and dd.run.cmd ~= cmd then
        kill_client(dd.run)
    end

    -- run command if need to
    local cmd = cmd or dd.cmd
    if cmd and dd.run.cmd ~= cmd then
        naughty.notify({title = "Spawning", text=cmd, timeout=3})
        local pid  = awful.util.spawn_with_shell(cmd)

        if not dd.run.client then
            -- sets to capture by pid. when the client gets managed, we get it.
            _capture(dd, cmd, pid, nil)
        end
    end

    -- raise an existing client.
    local screen = screen or dd.screen or capi.mouse.screen
    if dd.run.client then
        dd.run.screen = screen
        raise_client(dd.run)
    end
end


-- capture a client for a given dropdown
function dropdown.capture(dd, c)
    refresh_state(dd.run)
    if not dd.run.client then
        _capture(dd, dd.cmd, c.pid, c)
    end
end


-- geom: vert horiz width height.
function dropdown.new(cmd, geom, sticky, screen)
    local newdd = {}
    newdd.cmd      = cmd
    newdd.screen   = screen
    newdd.sticky   = sticky  or false
    newdd.run      = {}
    newdd.kill_old = true

    newdd.geom = {}
    newdd.geom.vert   = geom.vert   or "top"
    newdd.geom.horiz  = geom.horiz  or "center"
    newdd.geom.width  = geom.width  or 1
    newdd.geom.height = geom.height or 0.40

    for name, func in pairs(dropdown) do
        if type(func) == "function" then
            newdd[name] = func
        end
    end

    return newdd
end


-- manage action for clients in an active dropdown
function dropdown.on_manage(c)
    for pid, run in pairs(dropdown.data) do
        if run and pid == c.pid and not run.client then
            run.client = c

            -- dropdown clients are floaters
            awful.client.floating.set(c, true)

            -- Client properties
            c.ontop = true
            c.above = true
            c.skip_taskbar = true
            c.sticky = run.sticky

            -- get rid of titlebar
            if c.titlebar then
                awful.titlebar.remove(c)
            end

            -- raise
            raise_client(run)

            break
        end
    end
end

-- unmanage action for clients in an active dropdown
function dropdown.on_unmanage(c)
    for pid, run in pairs(dropdown.data) do
        if run and run.client and run.client.window == c.window then
            run.client  = nil
            run.visible = false
            run.cmd = nil
            break
        end
    end
end

-- Connects the signals
capi.client.connect_signal("manage", dropdown.on_manage)
capi.client.connect_signal("unmanage", dropdown.on_unmanage)


return dropdown
