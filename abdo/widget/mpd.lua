---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, Adrian C. <anrxc@sysphere.org>
---------------------------------------------------

-- {{{ Grab environment
local tonumber = tonumber
local io = { popen = io.popen, open=io.open}
local setmetatable = setmetatable
local string = string
local helpers = require("helpers")
-- }}}


-- Mpd: provides Music Player Daemon information
-- vicious.widgets.mpd
local mpd = {}


-- {{{ MPD widget type
local function worker(format, warg)
    local mpd_state  = {
        ["{volume}"]  = "N/A",
        ["{state}"]  = "stopped",
        ["{artist}"] = "N/A",
        ["{title}"]  = "N/A",
        ["{album}"]  = "N/A",
        ["{genre}"]  = "N/A",
        ["{file}"] = "N/A",
    }

    -- get mpd process state
    local pid_file = os.getenv("XDG_RUNTIME_DIR") .. "/mpd/pid"
    local f = io.open(pid_file, "r")
    if f == nil then
        return mpd_state
    else
        f:close()
    end

    -- get current song via mpc
    local fmt = "artist:%artist%\ntitle:%title%\nalbum:%album%\n" ..
        "genre:%genre%\nfile:%file%\ntime:%time%\nposition:%position%"
    local f = io.popen(string.format("mpc current -f \"%s\" 2> /dev/null", fmt))

    -- parse the data
    local k, v
    for line in f:lines() do
        for k, v in string.gmatch(line, "([%w]+):(.*)$") do
            mpd_state["{"..k.."}"] = v
        end
    end
    f:close()

    -- get current state via mpc
    local f = io.popen("mpc status 2> /dev/null")
    for line in f:lines() do
        for k, v in string.gmatch(line, "%[([%w]+)%].*%((.*)%%%)$") do
            mpd_state["{state}"] = k
            mpd_state["{volume}"] = tonumber(v)
        end
    end
    f:close()

    return mpd_state
end
-- }}}

return setmetatable(mpd, { __call = function(_, ...) return worker(...) end })
