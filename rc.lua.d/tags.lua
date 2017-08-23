
-- Variables
numtags = 10

-- Tags
awful.screen.connect_for_each_screen(function(s)
        awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9", "0" },
            s, awful.layout.layouts[1])
end)


-- Bindings
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
            end
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
            end
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


