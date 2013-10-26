---------------------------------------------------------------------------
-- @author Uli Schlachter &lt;psychon@znc.in&gt;
-- @copyright 2009 Uli Schlachter
-- @copyright 2008 Julien Danjou
-- @release v3.4-759-gec66ac8
---------------------------------------------------------------------------

-- Grab environment we need
local ipairs = ipairs
local math = math

-- awful.layout.suit.spiral
local spiral = {}

local function do_spiral(p, _spiral)
    local wa = p.workarea
    local cls = p.clients
    local n = math.min(5, #cls)

    for k, c in ipairs(cls) do
	if k > n then break end
        if k < n then
            if k % 2 == 0 then
                wa.height = wa.height / 2
            else
                wa.width = wa.width / 2
            end
        end

        if k % 4 == 0 and _spiral then
            wa.x = wa.x - wa.width
        elseif k % 2 == 0 or
            (k % 4 == 3 and k < n and _spiral) then
            wa.x = wa.x + wa.width
        end

        if k % 4 == 1 and k ~= 1 and _spiral then
            wa.y = wa.y - wa.height
        elseif k % 2 == 1 and k ~= 1 or
            (k % 4 == 0 and k < n and _spiral) then
            wa.y = wa.y + wa.height
        end

        local g = {
            x = wa.x,
            y = wa.y,
            width = wa.width - 2 * c.border_width,
            height = wa.height - 2 * c.border_width
        }
        c:geometry(g)
    end
end

--- Dwindle layout
spiral.dwindle = {}
spiral.dwindle.name = "dwindle"
function spiral.dwindle.arrange(p)
    return do_spiral(p, false)
end

--- Spiral layout
spiral.name = "spiral"
function spiral.arrange(p)
    return do_spiral(p, true)
end

return spiral

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
