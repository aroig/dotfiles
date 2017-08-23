
local capi = {
    screen = screen
}

-- Variables
numtags = 9

-- Tags
awful.screen.connect_for_each_screen(function(s)
        awful.tag({ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" },
            s, awful.layout.layouts[1])
end)

function new_numeric_tag(s)
    local s = s or awful.screen.focused()

    for i = 0, 20 do
        name = string.format("%d", i)
        if not awful.tag.find_by_name(s, name) then
            tag = awful.tag.add(name, { screen=s, layout = awful.layout.layouts[1], volatile=true })
            tag:view_only()
            break
        end
    end
end

function get_tag(s, name)
    local s = (s and capi.screen[s]) or awful.screen.focused()

    tag = awful.tag.find_by_name(s, name)
    if not tag then
        tag = awful.tag.add(name, { screen=s, layout = awful.layout.layouts[1], volatile=true })
    end
    return tag
end

function delete_tag(t)
    local t = t or awful.screen.focused().selected_tag
    if t then
        t:delete()
    end
end

function focused_screen()
    return awful.screen.focused()
end

function next_screen()
    s = awful.util.cycle(screen.count(), awful.screen.focused().index + 1)
    return capi.screen[s]
end


-- Bindings
globalkeys = gears.table.join(
    globalkeys,

    awful.key({ modkey, ctrlkey   }, "Up",     function () awful.tag.viewnext() end,
        { description = "Next tag", group = "tag" }),

    awful.key({ modkey, ctrlkey   }, "Down",   function () awful.tag.viewprev() end,
        { description = "Previous tag", group = "tag" }),

    awful.key({ modkey, ctrlkey   }, "k",      function () awful.tag.viewnext() end,
        { description = "Next tag", group = "tag" }),

    awful.key({ modkey, ctrlkey   }, "j",      function () awful.tag.viewprev() end,
        { description = "Previous tag", group = "tag" }),

    awful.key({ modkey, metakey   }, "Up",     function () awful.tag.viewnext(next_screen()) end),
    awful.key({ modkey, metakey   }, "Down",   function () awful.tag.viewprev(next_screen()) end),

    awful.key({ modkey, metakey   }, "k",      function () awful.tag.viewnext(next_screen()) end),
    awful.key({ modkey, metakey   }, "j",      function () awful.tag.viewprev(next_screen()) end),

    awful.key({ modkey, metakey, ctrlkey  }, "Up",    function () for s in screen do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "Down",  function () for s in screen do awful.tag.viewprev(s) end end),

    awful.key({ modkey, metakey, ctrlkey  }, "k",     function () for s in screen do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "j",     function () for s in screen do awful.tag.viewprev(s) end end),

    awful.key({ modkey, ctrlkey   }, "n",      function () new_numeric_tag(focused_screen()) end,
        { description = "New numeric tag", group = "tag" }),

    awful.key({ modkey, ctrlkey, shiftkey }, "n",     function () new_numeric_tag(focused_screen()) end,
        { description = "New numeric tag", group = "tag" }),

    -- Screen cycling by direction
    awful.key({ modkey, ctrlkey   }, "Left",   function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "Right",  function () awful.screen.focus_bydirection("right") end),

    awful.key({ modkey, ctrlkey   }, "h",      function () awful.screen.focus_bydirection("left") end),
    awful.key({ modkey, ctrlkey   }, "l",      function () awful.screen.focus_bydirection("right") end)
)

for i = 0, numtags do
    local key = string.format("%d", (i % 10))
    globalkeys = gears.table.join(
        globalkeys,

        awful.key({ modkey }, key,
            function ()
                local tag = get_tag(focused_screen(), i)
                tag:view_only()
            end,
            { description = "Switch to tag " .. key, group = "tag" }
        ),

        awful.key({ modkey, shiftkey }, key,
            function ()
                if client.focus then
                    local tag = get_tag(focused_screen(), i)
                    client.focus:move_to_tag(tag)
                end
            end,
            { description = "Move to tag " .. key, group = "tag" }
        )
    )
end


