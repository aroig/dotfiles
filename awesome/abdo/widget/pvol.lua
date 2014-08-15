---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2012, Abdó Roig-Maranges <abdo.roig@gmail.com>
---------------------------------------------------

local tonumber = tonumber
local io = { open = io.open }
local helpers = require("helpers")

local pvol_dir = os.getenv("XDG_RUNTIME_DIR") .. "/pvol"

local function worker(format)
    local pvol = helpers.pathtotable(pvol_dir)
    local vol = tonumber(pvol.volume or '0')
    local port = pvol.port

    return { vol=vol, port=port }
end

return worker

-- setmetatable(_M, { __call = function(_, ...) return worker(...) end })
