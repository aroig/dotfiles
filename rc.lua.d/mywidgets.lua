
---------------------------------------------------------------
-- File:    mywidgets.lua     PersonalWidgets                --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

-----------------------------------
-- Module loading                --
-----------------------------------

-- require("awful")

local wibox = require("wibox")
local vicious = require("vicious")

local source = {}
source.cpu    = require("abdo.widget.cpu")
source.bat    = require("abdo.widget.bat")
source.mem    = require("abdo.widget.mem")
source.net    = require("abdo.widget.net")
source.netctl = require("abdo.widget.netctl")
source.mpd    = require("abdo.widget.mpd")

source.temp   = require("abdo.widget.thermal")     -- My thermal widget
source.pvol   = require("abdo.widget.pvol")        -- My volume widget
source.sheval = require("abdo.widget.sheval")      -- Get value from a file

local beautiful = beautiful


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

myw.temp.icon = wibox.widget.imagebox()
myw.temp.icon:set_image(beautiful.widget_temp)

myw.temp.text = wibox.widget.textbox()
myw.temp.refresh = 2

function myw.temp.callback(widget, args)
    local color = util.gradient(gradcols, 35, 70, args[1])
    return string.format("<span color='%s'>%sºC</span>", color, args[1])
end

vicious.register(myw.temp.text, source.temp, myw.temp.callback, myw.temp.refresh,
                 {"coretemp.0", "core"})


-----------------------------------
-- CPU Usage                     --
-----------------------------------

myw.cpu = {}

myw.cpu.icon = wibox.widget.imagebox()
myw.cpu.icon:set_image(beautiful.widget_cpu)

myw.cpu.text = wibox.widget.textbox()
myw.cpu.refresh = 2

function myw.cpu.callback(widget, args)
    local color = util.gradient(gradcols, 0, 100, args[1])
    return string.format("<span color='%s'>%s%%</span>", color, args[1])
end

vicious.register(myw.cpu.text,  source.cpu,  myw.cpu.callback,  myw.cpu.refresh)


-----------------------------------
-- Memory                        --
-----------------------------------

myw.mem = {}
myw.mem.icon = wibox.widget.imagebox()
myw.mem.icon:set_image(beautiful.widget_mem)

myw.mem.text = wibox.widget.textbox()
myw.mem.refresh = 4

function myw.mem.callback(widget, args)
    local color = util.gradient(gradcols, 0, 100, args[1])
    return string.format("<span color='%s'>%s%%</span>", color, args[1])
end

vicious.register(myw.mem.text,  source.mem,  myw.mem.callback,  myw.mem.refresh)


-----------------------------------
-- Network Usage                 --
-----------------------------------

myw.net = {}

myw.net.icon = wibox.widget.imagebox()
myw.net.dnicon = wibox.widget.imagebox()
myw.net.upicon = wibox.widget.imagebox()

myw.net.icon:set_image(beautiful.widget_net)
myw.net.dnicon:set_image(beautiful.widget_netdw)
myw.net.upicon:set_image(beautiful.widget_netup)

myw.net.text = wibox.widget.textbox()
myw.net.tooltip = awful.tooltip({objects = {myw.net.icon, myw.net.dnicon, myw.net.upicon, myw.net.text}})
myw.refresh = 4

function myw.net.callback(widget, args)
    local up = 0.0
    local down = 0.0

    if args["{eth0 up_kb}"] then  up = up + args["{eth0 up_kb}"] end
    if args["{wlan0 up_kb}"] then up = up + args["{wlan0 up_kb}"] end

    if args["{eth0 down_kb}"] then  down = down + args["{eth0 down_kb}"] end
    if args["{wlan0 down_kb}"] then down = down + args["{wlan0 down_kb}"] end

    local downtxt = string.format('<span color="%s">%.0f</span>', beautiful.fg_netdn_widget, down)
    local uptxt =   string.format('<span color="%s">%.0f</span>', beautiful.fg_netup_widget, up)
    local sep = string.format(' <span color="%s">\\</span> ', beautiful.fg_widget)
    return downtxt .. sep .. uptxt
end

function myw.net.tooltip_callback(widget, args)
    local prf = ""
    local line
    for _,line in ipairs(args) do
        if prf ~= "" then
            prf = prf .. ", "
        end
        prf = prf .. line
    end
    return "Networks: " .. prf
end

vicious.register(myw.net.text, source.net, myw.net.callback, myw.net.refresh)
vicious.register(myw.net.tooltip.widget, source.netctl, myw.net.tooltip_callback, myw.net.refresh)


-----------------------------------
-- Gmail                         --
-----------------------------------

myw.mail = {}
myw.mail.text = wibox.widget.textbox()

myw.mail.icon = wibox.widget.imagebox()
myw.mail.icon:set_image(beautiful.widget_maile)

myw.mail.count = 0
myw.mail.refresh = 60

function myw.mail.callback(widget, args)
    local color
    local icon
    local text
    local num = tonumber(args[1])

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

    if num ~= myw.mail.count then
        myw.mail.icon:set_image(icon)
    end

    myw.mail.count = num

    return string.format("<span color='%s'>%s</span>", color, tostring(num))
end

vicious.register(myw.mail.text, source.sheval, myw.mail.callback, myw.mail.refresh,
                 "mutag -C -p mail -q 'flag:unread AND tag:\\\\Inbox'")

myw.mail.text:buttons(awful.util.table.join(awful.button({ }, 1,
                      function () exec(apps.mail) end)))
myw.mail.icon:buttons(myw.mail.text:buttons())


-----------------------------------
-- Music                         --
-----------------------------------

myw.mpd = {}
myw.mpd.path="/home/abdo/music/"

function myw.mpd.notify_song(args)
    if args['{file}'] ~= nil then
        local parentdir=string.gsub(args['{file}'], '/[^/]+$', '')
        local cover=parentdir .. '/' .. 'cover.jpg'

        naughty.notify({title = args['{Title}'],
                        text = string.format("%s\n%s",
                                             args['{Album}'],
                                             args['{Artist}']),
                        icon = myw.mpd.path .. '/' .. cover})
    end
end

myw.mpd.icon = wibox.widget.imagebox()
myw.mpd.stateicon = wibox.widget.imagebox()

myw.mpd.icon:set_image(beautiful.widget_music)
myw.mpd.stateicon:set_image(beautiful.widget_stop)

myw.mpd.text = wibox.widget.textbox()
myw.mpd.refresh = 2

myw.mpd.current = { ['{file}'] = nil,
                    ['{state}'] = nil}

function myw.mpd.callback(widget, args)
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

    return ""
end

vicious.register(myw.mpd.text, source.mpd, myw.mpd.callback, myw.mpd.refresh)

myw.mpd.text:buttons(awful.util.table.join( awful.button({ }, 1,
                      function () myw.mpd.notify_song(myw.mpd.current) end)))
myw.mpd.icon:buttons(myw.mpd.text:buttons())
myw.mpd.stateicon:buttons(myw.mpd.text:buttons())


-----------------------------------
-- Volume                        --
-----------------------------------

myw.vol = {}

myw.vol.icon = wibox.widget.imagebox()
myw.vol.icon:set_image(beautiful.widget_vol)

-- myw.vol.bar  = awful.widget.progressbar()
-- myw.vol.bar:set_vertical(true):set_ticks(true)
-- myw.vol.bar:set_height(12):set_width(8):set_ticks_size(1)
-- myw.vol.bar:set_background_color(beautiful.fg_off_widget)
-- myw.vol.bar:set_color(beautiful.fg_widget)
--
--volbar:set_gradient_colors({ beautiful.fg_widget,
--   beautiful.fg_center_widget, beautiful.fg_end_widget
--})

myw.vol.text = wibox.widget.textbox()
myw.vol.refresh = 2

function myw.vol.callback(widget, args)
    local color = util.gradient(gradcols, 0, 100, args[1])
    return string.format("<span color='%s'>%s%%</span>", color, args[1])
end

vicious.register(myw.vol.text, source.pvol, myw.vol.callback, myw.vol.refresh)
-- vicious.register(myw.volbar, widget.pvol,  "$1",  2)

myw.vol.text:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec("pavucontrol") end)))
myw.vol.icon:buttons(myw.vol.text:buttons())
-- myw.vol.bar:buttons(myw.vol.text:buttons())


-----------------------------------
-- Battery State                 --
-----------------------------------

myw.bat = {}
myw.bat.icon = wibox.widget.imagebox()
myw.bat.icon:set_image(beautiful.widget_bat)

myw.bat.text = wibox.widget.textbox()
myw.bat.rate = wibox.widget.textbox()
myw.bat.tooltip = awful.tooltip({ objects = {myw.bat.text, myw.bat.rate, myw.bat.icon}})

myw.bat.refresh = 60

function myw.bat.callback(widget, args)
    myw.bat.tooltip:set_text("Time remaining: " .. args.time)
    local color_percent = util.gradient(gradcols_rev, 0, 100, args.percent)
    return string.format("<span color='%s'>%s%s%%</span>", color_percent, args.state, args.percent)
end

function myw.bat.rate_callback(widget, args)
    local color_rate = beautiful.fg_green
    if args.state == '+' then
        color_rate = util.gradient(gradcols_rev, 0, 40, args.rate)
    else
        color_rate = util.gradient(gradcols, 7, 30, args.rate)
    end
    return string.format("<span color='%s'>%4.1fW</span>", color_rate, args.rate)
end


vicious.register(myw.bat.text, source.bat, myw.bat.callback, myw.bat.refresh, "BAT0")
vicious.register(myw.bat.rate, source.bat, myw.bat.rate_callback, myw.bat.refresh, "BAT0")

myw.bat.text:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec("gnome-power-statistics") end)))
myw.bat.icon:buttons(myw.bat.text:buttons())
myw.bat.rate:buttons(myw.bat.text:buttons())


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
