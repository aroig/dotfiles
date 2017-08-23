
-- Variables
numtags = 10

-- Tags
awful.screen.connect_for_each_screen(function(s)
        awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" },
            s, awful.layout.layouts[1])
end)

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

    awful.key({ modkey, metakey   }, "Up",     function () awful.tag.viewnext(awful.util.cycle(screen.count(), awful.screen.focused().index + 1)) end),
    awful.key({ modkey, metakey   }, "Down",   function () awful.tag.viewprev(awful.util.cycle(screen.count(), awful.screen.focused().index + 1)) end),

    awful.key({ modkey, metakey   }, "k",      function () awful.tag.viewnext(awful.util.cycle(screen.count(), awful.screen.focused().index + 1)) end),
    awful.key({ modkey, metakey   }, "j",      function () awful.tag.viewprev(awful.util.cycle(screen.count(), awful.screen.focused().index + 1)) end),

    awful.key({ modkey, metakey, ctrlkey  }, "Up",    function () for s in screen do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "Down",  function () for s in screen do awful.tag.viewprev(s) end end),

    awful.key({ modkey, metakey, ctrlkey  }, "k",     function () for s in screen do awful.tag.viewnext(s) end end),
    awful.key({ modkey, metakey, ctrlkey  }, "j",     function () for s in screen do awful.tag.viewprev(s) end end)
)

for i = 1, numtags do
    local key = string.format("%d", (i % 10))
    globalkeys = gears.table.join(
        globalkeys,

        awful.key({ modkey }, key,
            function ()
                local tag = awful.screen.focused().tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            { description = "Switch to tag " .. key, group = "tag" }
        ),

        awful.key({ modkey, ctrlkey }, key,
            function ()
                local tag = awful.screen.focused().tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end
        ),

        awful.key({ modkey, shiftkey }, key,
            function ()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            { description = "Move to tag " .. key, group = "tag" }
        ),

        awful.key({ modkey, ctrlkey, shiftkey }, key,
            function ()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end
    ))
end


