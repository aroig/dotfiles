
local capi = {
    screen = screen
}

-- Variables
local numtags = 9
local maxtags = 30

local persistent_tags = {}
for i = 0, numtags do
    table.insert(persistent_tags, string.format("%d", i))
end

-- Tags
awful.screen.connect_for_each_screen(function(s)
        awful.tag(persistent_tags, s, awful.layout.layouts[1])
end)

local function new_numeric_tag(s)
    local s = s or awful.screen.focused()

    for i = 0, maxtags do
        name = string.format("%d", i)
        if not awful.tag.find_by_name(s, name) then
            return awful.tag.add(name,
                                 { screen=s,
                                   layout = awful.layout.layouts[1],
                                   volatile=true })
        end
    end
end

function get_tag(name, s)
    local s = (s and capi.screen[s]) or awful.screen.focused()

    tag = awful.tag.find_by_name(s, name)
    if not tag then
        tag = awful.tag.add(name,
                            { screen=s,
                              layout = awful.layout.layouts[1],
                              volatile=true })
    end
    return tag
end

local function delete_tag(t)
    local t = t or awful.screen.focused().selected_tag
    if t then
        t:delete()
    end
end

local function focused_screen()
    return awful.screen.focused()
end

local function next_screen()
    s = awful.util.cycle(screen.count(), awful.screen.focused().index + 1)
    return capi.screen[s]
end


local function place_in_tag(c, name)
    tag = get_tag(name)
    c:move_to_tag(tag)
    tag:view_only()
end


-- Bindings
globalkeys = gears.table.join(
    globalkeys,

    awful.key({ modkey, ctrlkey   }, "Up",     function () awful.tag.viewnext(focused_screen()) end,
        { description = "Next tag on focused screen", group = "tag" }),

    awful.key({ modkey, ctrlkey   }, "Down",   function () awful.tag.viewprev(focused_screen()) end,
        { description = "Previous tag on focused screen", group = "tag" }),

    awful.key({ modkey, ctrlkey   }, "k",      function () awful.tag.viewnext(focused_screen()) end,
        { description = "Next tag on focused screen", group = "tag" }),

    awful.key({ modkey, ctrlkey   }, "j",      function () awful.tag.viewprev(focused_screen()) end,
        { description = "Previous tag on focused screen", group = "tag" }),

    awful.key({ modkey, metakey   }, "Up",     function () awful.tag.viewnext(next_screen()) end),
    awful.key({ modkey, metakey   }, "Down",   function () awful.tag.viewprev(next_screen()) end),

    awful.key({ modkey, metakey   }, "k",      function () awful.tag.viewnext(next_screen()) end),
    awful.key({ modkey, metakey   }, "j",      function () awful.tag.viewprev(next_screen()) end),

    awful.key({ modkey, metakey, ctrlkey  }, "Up",    function () for s in screen do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "Down",  function () for s in screen do awful.tag.viewprev(s) end end),

    awful.key({ modkey, metakey, ctrlkey  }, "k",     function () for s in screen do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "j",     function () for s in screen do awful.tag.viewprev(s) end end),

    awful.key({ modkey, ctrlkey   }, ".",
        function ()
            local tag = new_numeric_tag(focused_screen())
            tag:view_only()
        end,
        { description = "New tag on focused screen", group = "tag" }),

    awful.key({ modkey, metakey   }, ".",
        function ()
            local tag = new_numeric_tag(next_screen())
            tag:view_only()
        end,
        { description = "New tag on next screen", group = "tag" }),

    awful.key({ modkey, ctrlkey, shiftkey }, ".",
        function ()
            if client.focus then
                local tag = new_numeric_tag(focused_screen())
                client.focus:move_to_tag(tag)
                tag:view_only()
            end
        end,
        { description = "Move to new tag", group = "tag" }),

    -- Previous tag
    awful.key({ modkey,           }, "p",
        function () awful.tag.history.restore(focused_screen()) end),

    -- Screen cycling by direction
    awful.key({ modkey, ctrlkey   }, "Left",   function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "Right",  function () awful.screen.focus_bydirection("right") end),

    awful.key({ modkey, ctrlkey   }, "h",      function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "l",      function () awful.screen.focus_bydirection("right") end)
)

for i = 0, 9 do
    local key = string.format("%d", (i % 10))
    globalkeys = gears.table.join(
        globalkeys,

        awful.key({ modkey }, key,
            function ()
                local tag = get_tag(key, focused_screen())
                tag:view_only()
            end,
            { description = "Switch to tag " .. key, group = "tag" }
        ),

        awful.key({ modkey, shiftkey }, key,
            function ()
                if client.focus then
                    local tag = get_tag(key, focused_screen())
                    client.focus:move_to_tag(tag)
                    tag:view_only()
                end
            end,
            { description = "Move to tag " .. key, group = "tag" }
        )
    )
end


-- Rules
clientrules = gears.table.join(
    clientrules,

    {
        -- { rule_any = { instance = { "chromium", "firefox" } },
        --   callback = function(c) place_in_tag(c, "web") end },

        { rule_any = { instance = { "journal$" } },
          callback = function(c) place_in_tag(c, "log") end },

        { rule_any = { instance = { "glances$" } },
          callback = function(c) place_in_tag(c, "top") end }
    }
)
