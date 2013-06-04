
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

    if args["{eth0 up_kb}"] then  up = up + args["{eth0 up_kb}"] end
    if args["{wlan0 up_kb}"] then up = up + args["{wlan0 up_kb}"] end

    if args["{eth0 down_kb}"] then  down = down + args["{eth0 down_kb}"] end
    if args["{wlan0 down_kb}"] then down = down + args["{wlan0 down_kb}"] end

    if up ~= myw.net.value.up or down ~= myw.net.value.down then
        local uptxt = string.format('<span color="%s">%.0f</span>', beautiful.fg_netup_widget, up)

        local downtxt = string.format('<span color="%s">%.0f</span>', beautiful.fg_netdn_widget, down)

        local sep = string.format(' <span color="%s">\\</span> ', beautiful.fg_widget)
        myw.net.widget:set_markup(downtxt .. sep .. uptxt)
    end
    myw.net.value.up = up
    myw.net.value.down = down
end

myw.netctl = {}
myw.netctl.src = require("abdo.widget.netctl")

myw.netctl.tooltip = awful.tooltip({objects = {myw.net.dnicon, myw.net.upicon, myw.net.widget}})
myw.netctl.value = ""

function myw.netctl.update()
    local args = myw.netctl.src(nil)
    local prf = ""
    local line
    for _,line in ipairs(args) do
        if prf ~= "" then
            prf = prf .. ", "
        end
        prf = prf .. line
    end
    if myw.netctl.value ~= prf then
        myw.netctl.tooltip.widget:set_markup("<b>Networks:</b> " .. prf)
    end
    myw.netctl.value = prf
end

timers.fast:connect_signal("timeout", myw.net.update)
timers.normal:connect_signal("timeout", myw.netctl.update)


-----------------------------------
-- Gmail                         --
-----------------------------------

myw.mail = {}
myw.mail.src = require("abdo.widget.sheval")

myw.mail.icon = wibox.widget.imagebox()
myw.mail.icon:set_image(beautiful.widget_maile)

myw.mail.widget = wibox.widget.textbox()
myw.mail.count = -1

function myw.mail.update()
    local args = myw.mail.src(nil, "mutag -C -p mail -q 'flag:unread AND tag:\\\\Inbox'")

    local num = tonumber(args[1])
    local color
    local icon
    local text

    if num ~= myw.mail.count then
        if num == 0 or num == nil then
            color = beautiful.fg_green_widget
            if num == nil then
                color = beautiful.fg_red_widget
                icon = beautiful.widget_mailf
                num = "?"
            else
                color = beautiful.fg_green_widget
                icon = beautiful.widget_maile
            end
        else
            if num ~= myw.mail.count then
                if num == 1 then
                    text = "There is one new message"
                else
                    text = string.format("There are %d new messages", num)
                end
            end
            icon = beautiful.widget_mailf
            color = beautiful.fg_red_widget
        end

        local text = string.format("<span color='%s'>%s</span>", color, tostring(num))
        myw.mail.icon:set_image(icon)
        myw.mail.widget:set_markup(text)
    end
    myw.mail.count = num
end

myw.mail.widget:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec(apps.mail) end)))
myw.mail.widget:buttons(myw.mail.widget:buttons())

timers.slow:connect_signal("timeout", myw.mail.update)


-----------------------------------
-- Music                         --
-----------------------------------

myw.mpd = {}
myw.mpd.src  = require("abdo.widget.mpd")
myw.mpd.path = "/home/abdo/music/"

myw.mpd.icon = wibox.widget.imagebox()
myw.mpd.stateicon = wibox.widget.imagebox()

myw.mpd.icon:set_image(beautiful.widget_music)
myw.mpd.stateicon:set_image(beautiful.widget_stop)

myw.mpd.current = { ['{file}'] = nil,
                    ['{state}'] = nil}

function myw.mpd.notify_song(args)
    if args['{file}'] ~= nil then
        local parentdir=string.gsub(args['{file}'], '/[^/]+$', '')
        local cover=parentdir .. '/' .. 'cover.jpg'

        naughty.notify({title = args['{Title}'],
                        text = string.format("%s\n%s",
                                             args['{Album}'],
                                             args['{Artist}']),
                        icon = myw.mpd.path .. '/' .. cover,
                        appname = "mpd"})
    end
end

function myw.mpd.update()
    local args = myw.mpd.src(nil)

    local icon = beautiful.widget_stop

    if args['{state}'] == 'play' then
        icon = beautiful.widget_play
    elseif args['{state}'] == 'stop' then
        icon = beautiful.widget_stop
    elseif args['{state}'] == 'pause' then
        icon = beautiful.widget_pause
    end

    if myw.mpd.current['{file}'] ~= args['{file}'] or
    myw.mpd.current['{state}'] ~= args['{state}'] then
        if args['{state}'] == "play" then
            myw.mpd.notify_song(args)
        end

        if myw.mpd.current['{state}'] ~= args['{state}'] then
            myw.mpd.stateicon:set_image(icon)
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

myw.vol.icon = wibox.widget.imagebox()
myw.vol.icon:set_image(beautiful.widget_vol)

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
        if args.state == '+' then
            color_rate = util.gradient(gradcols_rev, 0, 40, args.rate)
        else
            color_rate = util.gradient(gradcols, 7, 30, args.rate)
        end
        local text = string.format("<span color='%s'>%4.1fW</span>", color_rate, args.rate)
        myw.bat.rtwidget:set_markup(text)
    end

    if args.percent ~= myw.bat.percent then
        local color_percent = util.gradient(gradcols_rev, 0, 100, args.percent)
        local text = string.format("<span color='%s'>%s%s%%</span>", color_percent, args.state, args.percent)

        myw.bat.pcwidget:set_markup(text)
    end

    if args.time ~= myw.bat.time then
        myw.bat.tooltip.widget:set_markup("<b>Time remaining:</b> " .. args.time)
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
