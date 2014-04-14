
---------------------------------------------------------------
-- File:    mywidgets.lua     PersonalWidgets                --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

-----------------------------------
-- Module loading                --
-----------------------------------

-- require("awful")
local capi  = { timer = timer }

local wibox = require("wibox")
local beautiful = beautiful

local os = os

-----------------------------------
-- Timers                        --
-----------------------------------

local timers = {}
timers.slow    = timer({ timeout = 60 })
timers.normal  = timer({ timeout = 10 })
timers.fast    = timer({ timeout = 2 })

function start_widgets()
    for _,t in pairs(timers) do
        t:start()
        t:emit_signal("timeout")
    end
end

function stop_widgets()
    for _,t in pairs(timers) do
        t:stop()
    end
end

-----------------------------------
-- Convenience stuff             --
-----------------------------------

local scount = screen.count()

local gradcols = {beautiful.fg_grad1_widget, beautiful.fg_grad2_widget,
		  beautiful.fg_grad3_widget, beautiful.fg_grad4_widget}

local gradcols_rev = {beautiful.fg_grad4_widget, beautiful.fg_grad3_widget,
		      beautiful.fg_grad2_widget, beautiful.fg_grad1_widget}

myw = {}


-----------------------------------
-- Spacers                       --
-----------------------------------

myw.spacer = wibox.widget.textbox()
myw.spacer:set_markup("<span>   </span>")

myw.separator = wibox.widget.textbox()
myw.separator:set_markup(string.format("<span color='%s'>  |  </span>", beautiful.fg_widget))


-----------------------------------
-- Temperature sensors           --
-----------------------------------

myw.temp = {}
myw.temp.src = require("abdo.widget.thermal")

myw.temp.icon = wibox.widget.imagebox()
myw.temp.icon:set_image(beautiful.widget_temp)

myw.temp.widget = wibox.widget.textbox()
myw.temp.value = '?'

function myw.temp.update()
    local args = myw.temp.src(nil, {"coretemp.0", "core"})
    if args[1] ~= myw.temp.value then
        local color = util.gradient(gradcols, 35, 70, args[1])
        local text = string.format("<span color='%s'>%sºC</span>", color, args[1])
        myw.temp.widget:set_markup(text)
    end
    myw.temp.value = args[1]
end

timers.fast:connect_signal("timeout", myw.temp.update)


-----------------------------------
-- CPU Usage                     --
-----------------------------------

myw.cpu = {}
myw.cpu.src = require("abdo.widget.cpu")

myw.cpu.icon = wibox.widget.imagebox()
myw.cpu.icon:set_image(beautiful.widget_cpu)

myw.cpu.widget = wibox.widget.textbox()
myw.cpu.value = "?"

function myw.cpu.update()
    local args = myw.cpu.src(nil)
    if args[1] ~= myw.cpu.value then
        local color = util.gradient(gradcols, 0, 100, args[1])
        local text = string.format("<span color='%s'>%s%%</span>", color, args[1])
        myw.cpu.widget:set_markup(text)
    end
    myw.cpu.value = args[1]
end

timers.fast:connect_signal("timeout", myw.cpu.update)


-----------------------------------
-- Memory                        --
-----------------------------------

myw.mem = {}
myw.mem.src = require("abdo.widget.mem")

myw.mem.icon = wibox.widget.imagebox()
myw.mem.icon:set_image(beautiful.widget_mem)

myw.mem.widget = wibox.widget.textbox()
myw.mem.value = "?"

function myw.mem.update()
    local args = myw.mem.src(nil)
    if args[1] ~= myw.mem.value then
        local color = util.gradient(gradcols, 0, 100, args[1])
        local text = string.format("<span color='%s'>%s%%</span>", color, args[1])
        myw.mem.widget:set_markup(text)
    end
    myw.mem.value = args[1]
end

timers.fast:connect_signal("timeout", myw.mem.update)


-----------------------------------
-- Network Usage                 --
-----------------------------------

myw.net = {}
myw.net.src = require("abdo.widget.net")

-- myw.net.icon = wibox.widget.imagebox()
myw.net.dnicon = wibox.widget.imagebox()
myw.net.upicon = wibox.widget.imagebox()

-- myw.net.icon:set_image(beautiful.widget_net)
myw.net.dnicon:set_image(beautiful.widget_netdw)
myw.net.upicon:set_image(beautiful.widget_netup)

myw.net.widget = wibox.widget.textbox()
myw.net.value = { up=-1, down=-1 }

function myw.net.update()
    local args = myw.net.src(nil)
    local up = 0.0
    local down = 0.0

    -- loop over all interfaces except loopback
    for k, v in pairs(args) do
        if not string.match(k, "{lo .*}") then
            if string.match(k, "{.* up_kb}") then
                up = up + v
            end

            if string.match(k, "{.* down_kb}") then
                down = down + v
            end
        end
    end

    if up ~= myw.net.value.up or down ~= myw.net.value.down then
        local uptxt = string.format('<span color="%s">%.0f</span>', beautiful.fg_netup_widget, up)

        local downtxt = string.format('<span color="%s">%.0f</span>', beautiful.fg_netdn_widget, down)

        local sep = string.format(' <span color="%s">\\</span> ', beautiful.fg_widget)
        -- local sep = string.format('<span color="%s" font="%s"> 🔄 </span>',
        --                beautiful.fg_green, beautiful.font_symbol)

        myw.net.widget:set_markup(downtxt .. sep .. uptxt)
    end
    myw.net.value.up = up
    myw.net.value.down = down
end

timers.fast:connect_signal("timeout", myw.net.update)



-----------------------------------
-- Mail                          --
-----------------------------------

myw.mail = {}
myw.mail.src = require("abdo.widget.mutag")

local mail_empty = string.format('<span color="%s" font="%s">📧 </span>',
                                 beautiful.fg_green, beautiful.font_symbol)

local mail_full = string.format('<span color="%s" font="%s">📧 </span>',
                                beautiful.fg_red, beautiful.font_symbol)

myw.mail.icon = wibox.widget.imagebox()
myw.mail.icon:set_image(beautiful.widget_maile)

myw.mail.widget = wibox.widget.textbox()
myw.mail.tooltip = awful.tooltip({ objects = {myw.mail.icon, myw.mail.widget}})

myw.mail.num_inbox = -1
myw.mail.num_queue = -1

function myw.mail.update()
    local args
    local icon = beautiful.widget_maile

    args = myw.mail.src(nil)
    local mail = args[1]
    local num_inbox = #mail
    local color_inbox = beautiful.fg_green_widget

    local queue = args[2]
    local num_queue = #queue
    local color_queue = beautiful.fg_green_widget

    local text = ""
    if num_inbox ~= myw.mail.num_inbox or num_queue ~= myw.mail.num_queue then
        if num_inbox == nil then
            color_inbox = beautiful.fg_red_widget
            icon = beautiful.widget_mailf
            num_inbox = "?"
        elseif num_inbox == 0 then
            color_inbox = beautiful.fg_green_widget
            icon = beautiful.widget_maile
        elseif num_inbox > 0 then
            color_inbox = beautiful.fg_red_widget
            icon = beautiful.widget_mailf
        end

        if num_queue == nil then
            color_queue = beautiful.fg_red_widget
            icon = beautiful.widget_mailf
            num_queue = "?"
        elseif num_queue == 0 then
            color_queue = beautiful.fg_green_widget
        else
            color_queue = beautiful.fg_red_widget
        end

        text = text .. string.format("<span color='%s'>%s</span>",
                                   color_inbox, tostring(num_inbox))
        text = text .. string.format(' <span color="%s">\\</span> ',
                                     beautiful.fg_widget)
        text = text .. string.format("<span color='%s'>%s</span>",
                                   color_queue, tostring(num_queue))
        myw.mail.icon:set_image(icon)
        myw.mail.widget:set_markup(text)
    end

    myw.mail.tooltip.textbox:set_text(table.concat(mail, '\n'))

    myw.mail.num_inbox = num_inbox
    myw.mail.num_queue = num_queue
end

myw.mail.widget:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec(apps.mail) end)))
myw.mail.widget:buttons(myw.mail.widget:buttons())

timers.normal:connect_signal("timeout", myw.mail.update)



-----------------------------------
-- Music                         --
-----------------------------------

myw.mpd = {}
myw.mpd.src  = require("abdo.widget.mpd")
myw.mpd.path = os.getenv("AB2_MUSIC_DIR")

myw.mpd.icon = wibox.widget.textbox()
myw.mpd.icon:set_markup(string.format("<span color='%s' font='%s'>♫ </span>",
                                      beautiful.fg_green, beautiful.font_symbol))

myw.mpd.stateicon = wibox.widget.textbox()

myw.mpd.current = { ['{file}'] = nil,
                    ['{state}'] = nil}

function myw.mpd.notify_song(args)
    if args['{file}'] ~= nil then
        local parentdir=string.gsub(args['{file}'], '/[^/]+$', '')
        local cover=parentdir .. '/' .. 'cover.jpg'

        naughty.notify({title = args['{title}'],
                        text = string.format("%s\n%s",
                                             args['{album}'],
                                             args['{artist}']),
                        icon = myw.mpd.path .. '/' .. cover,
                        appname = "mpd"})
    end
end

function myw.mpd.update()
    local args = myw.mpd.src(nil)

    local play_icon = string.format("<span color='%s' font='%s'>▶</span>",
                                    beautiful.fg_red, beautiful.font_symbol)

    local stop_icon = string.format("<span color='%s' font='%s'>■</span>",
                                    beautiful.fg_green, beautiful.font_symbol)

    local icon = stop_icon

    if args['{state}'] == 'playing' then
        icon = play_icon
    elseif args['{state}'] == 'stopped' then
        icon = stop_icon
    elseif args['{state}'] == 'paused' then
        icon = stop_icon
    end

    if myw.mpd.current['{file}'] ~= args['{file}'] or
    myw.mpd.current['{state}'] ~= args['{state}'] then
        if args['{state}'] == "playing" then
            myw.mpd.notify_song(args)
        end

        if myw.mpd.current['{state}'] ~= args['{state}'] then
            myw.mpd.stateicon:set_markup(icon)
        end
    end

    myw.mpd.current = args
end

myw.mpd.icon:buttons(awful.util.table.join( awful.button({ }, 1,
                      function () myw.mpd.notify_song(myw.mpd.current) end)))
myw.mpd.stateicon:buttons(myw.mpd.icon:buttons())

timers.fast:connect_signal("timeout", myw.mpd.update)


-----------------------------------
-- Volume                        --
-----------------------------------

myw.vol = {}
myw.vol.src = require("abdo.widget.pvol")

myw.vol.icon = wibox.widget.textbox()
myw.vol.icon:set_markup(string.format("<span color='%s' font='%s'>🔊 </span>",
                                      beautiful.fg_green, beautiful.font_symbol))

myw.vol.widget = wibox.widget.textbox()
myw.vol.value = -1

function myw.vol.update()
    local args = myw.vol.src(nil)
    if args[1] ~= myw.vol.value then
        local color = util.gradient(gradcols, 0, 100, args[1])
        local text = string.format("<span color='%s'>%s%%</span>", color, args[1])
        myw.vol.widget:set_markup(text)
    end
    myw.vol.value = args[1]
end

myw.vol.widget:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec("pavucontrol") end)))
myw.vol.icon:buttons(myw.vol.widget:buttons())

timers.fast:connect_signal("timeout", myw.vol.update)


-----------------------------------
-- Battery State                 --
-----------------------------------

myw.bat = {}
myw.bat.src = require("abdo.widget.bat")

myw.bat.icon = wibox.widget.imagebox()
myw.bat.icon:set_image(beautiful.widget_bat)

myw.bat.pcwidget = wibox.widget.textbox()
myw.bat.rtwidget = wibox.widget.textbox()
myw.bat.tooltip = awful.tooltip({ objects = {myw.bat.pcwidget, myw.bat.rtwidget, myw.bat.icon}})

myw.bat.percent = -1
myw.bat.rate = -1
myw.bat.time = -1

function myw.bat.update()
    local args = myw.bat.src(nil, "BAT0")
    if args.rate ~= myw.bat.rate then
        local color_rate = beautiful.fg_green

        if args.state == 'charging' then
            color_rate = util.gradient(gradcols_rev, 0, 40, args.rate)
        else
            color_rate = util.gradient(gradcols, 7, 30, args.rate)
        end
        local text = string.format("<span color='%s'>%4.1fW</span>", color_rate, args.rate)
        myw.bat.rtwidget:set_markup(text)
    end

    if args.percent ~= myw.bat.percent then
        local color_percent = util.gradient(gradcols_rev, 0, 100, args.percent)

        local battery_state = {
            full       = "⚡",
            unknown    = "?",
            charged     = "⚡",
            charging    = "+",
            discharging = "-"
        }

        local text_state = string.format("<span color='%s' font='%s'>%s</span>",
                                         color_percent, beautiful.font_symbol, battery_state[args.state])
        local text_percent = string.format("<span color='%s'>%s%s%%</span>",
                                           color_percent, args.state, args.percent)

        myw.bat.pcwidget:set_markup(text_state .. text_percent)

        if args.percent > 0 and args.percent <= 5 and args.state ~= 'charging' then
            naughty.notify({title="Low Battery", text="Hey, plug the power cord!",
                            appname="battery"})
        end
    end

    if args.time ~= myw.bat.time then
        myw.bat.tooltip.textbox:set_markup("<b>Time remaining:</b> " .. args.time)
    end

    myw.bat.rate = args.rate
    myw.bat.percent = args.percent
    myw.bat.time = args.time
end

myw.bat.rtwidget:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec("gnome-power-statistics") end)))
myw.bat.pcwidget:buttons(myw.bat.rtwidget:buttons())
myw.bat.icon:buttons(myw.bat.rtwidget:buttons())

timers.fast:connect_signal("timeout", myw.bat.update)


-----------------------------------
-- System                        --
-----------------------------------

myw.sys = {}
myw.sys.src = require("abdo.widget.fileval")
myw.sys.sync = wibox.widget.textbox()
myw.sys.priv = wibox.widget.textbox()

function myw.sys.update()
    local priv_state = myw.sys.src(nil, {os.getenv("HOME") .. "/priv/README", false})
    local sync_state = true

    if priv_state then
        myw.sys.priv:set_markup(string.format("<span color='%s' font='%s'> 🔒 </span>",
                                              beautiful.fg_green, beautiful.font_symbol))
    else
        myw.sys.priv:set_markup(string.format("<span color='%s' font='%s'> 🔒 </span>",
                                              beautiful.fg_red, beautiful.font_symbol))
    end

    if sync_state then
        myw.sys.sync:set_markup(string.format("<span color='%s' font='%s'> ☢ </span>",
                                              beautiful.fg_green, beautiful.font_symbol))
    else
        myw.sys.sync:set_markup(string.format("<span color='%s' font='%s'> ☢ </span>",
                                              beautiful.fg_red, beautiful.font_symbol))
    end
end

timers.normal:connect_signal("timeout", myw.sys.update)


-----------------------------------
-- Clock                         --
-----------------------------------

myw.textclock = awful.widget.textclock()


-----------------------------------
-- Systray                       --
-----------------------------------

myw.systray = wibox.widget.systray()


-----------------------------------
-- Taglist                       --
-----------------------------------

myw.taglist = {}

myw.taglist.buttons = awful.util.table.join(
    awful.button({ }, 1, awful.tag.viewonly),
    awful.button({ }, 3, awful.tag.viewtoggle)
--   awful.button({ modkey }, 1, awful.client.movetotag),
--   awful.button({ modkey }, 3, awful.client.toggletag)
--                    awful.button({ }, 4, awful.tag.viewnext),
--                    awful.button({ }, 5, awful.tag.viewprev)
)

for s = 1, screen.count() do
    myw.taglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, myw.taglist.buttons)
end


-----------------------------------
-- Tasklist                      --
-----------------------------------

myw.tasklist = {}

myw.tasklist.buttons = awful.util.table.join(
    awful.button({ }, 1,
        function (c)
            if c == client.focus then
                c.minimized = true
            else
                if not c:isvisible() then
                    awful.tag.viewonly(c:tags()[1])
                end
                -- This will also un-minimize
                -- the client, if needed
                client.focus = c
                c:raise()
            end
        end),

--   awful.button({ }, 3, function ()
--			   if instance then
--			      instance:hide()
--			      instance = nil
--			   else
--			      instance = awful.menu.clients({ width=250 })
--			   end
--			end),

    awful.button({ }, 4,
        function ()
            awful.client.focus.byidx(1)
            if client.focus then client.focus:raise() end
        end),

    awful.button({ }, 5,
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end)
)

for s = 1, screen.count() do
   myw.tasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, myw.tasklist.buttons)

end


-----------------------------------
-- Layout box                    --
-----------------------------------

myw.layoutbox = {}

for s = 1, screen.count() do
   myw.layoutbox[s] = awful.widget.layoutbox(s)
   myw.layoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
 end


-----------------------------------
-- Prompt box                    --
-----------------------------------

myw.promptbox = {}
for s = 1, screen.count() do
    myw.promptbox[s] = awful.widget.prompt()
end



-----------------------------------
-- Start the widgets             --
-----------------------------------

start_widgets()
