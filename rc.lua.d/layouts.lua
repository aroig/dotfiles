
-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.tile,
    awful.layout.suit.fair,
    awful.layout.suit.floating,
}

-- Bindings
globalkeys = gears.table.join(
    globalkeys,

    -- Layout cycling
    awful.key({ modkey,           }, "space", function () awful.layout.inc(1) end),
    awful.key({ modkey, shiftkey  }, "space", function () awful.layout.inc(-1) end),

    -- Layout manipulation
    awful.key({ modkey,           }, "+",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "-",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, shiftkey  }, "-",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, shiftkey  }, "+",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, ctrlkey   }, "-",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, ctrlkey   }, "+",     function () awful.tag.incncol(-1)         end)
)
