---------------------------------------------------------------
-- File:    mywidgets.lua     PersonalWidgets                --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

-----------------------------------
-- Module loading                --
-----------------------------------

-- require("awful")

local gears = gears
local wibox = wibox
local beautiful = beautiful
local keyboard = keyboard

local capi = { awesome = awesome }

local os = os

local host_config = host_config

-----------------------------------
-- Timers                        --
-----------------------------------

local timers = {}
timers.slow    = gears.timer({ timeout = 60 })
timers.normal  = gears.timer({ timeout = 10 })
timers.fast    = gears.timer({ timeout = 2 })

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

local gradcols = {}
local gradcols_rev = {}
local num = #beautiful.color_widget_gradient

for i, col in ipairs(beautiful.color_widget_gradient) do
    gradcols[i] = col
    gradcols_rev[num-i+1] = col
end

myw = {}


function wiboxcolor(min, max, value)
    local crit = 0.9
    local warn = 0.7

    local norm = (value - min) / (max - min)
    if norm < 0 then
        norm = 0
    end

    if norm > 1 then
        morm = 1
    end

    if norm < warn then
        return beautiful.color_widget_value
    elseif norm < crit then
        return beautiful.color_widget_warn
    else
        return beautiful.color_widget_alert
    end
end



function wiboxicon(name, color)

    if beautiful.wibox[name] == nil then
        return "nil"
    end

    local symbol = beautiful.wibox[name]

    if color then
        return string.format('<span color="%s" font="%s">%s</span>',
                             color,
                             beautiful.font_symbol,
                             symbol)

    else
        return string.format('<span font="%s">%s</span>',
                             beautiful.font_symbol,
                             symbol)
    end
end


function colortext(text, color)
    color = color or beautiful.color_widget
    return string.format('<span color="%s">%s</span>',
                         color, text)
end



-----------------------------------
-- Spacers                       --
-----------------------------------

myw.spacer = wibox.widget.textbox()
myw.spacer:set_markup(colortext("  "))

myw.separator = wibox.widget.textbox()
myw.separator:set_markup(colortext(" |  "))

myw.slash = wibox.widget.textbox()
myw.slash:set_markup(colortext("/ "))



-----------------------------------
-- Hardware                      --
-----------------------------------

myw.hdw = {}

myw.hdw.cpuicon = wibox.widget.textbox()
myw.hdw.cpuicon:set_markup(wiboxicon('cpu', beautiful.color_widget) .. " ")

myw.hdw.memicon = wibox.widget.textbox()
myw.hdw.memicon:set_markup(wiboxicon('memory', beautiful.color_widget) .. " ")

myw.hdw.tempicon = wibox.widget.textbox()
myw.hdw.tempicon:set_markup(wiboxicon('temp', beautiful.color_widget) .. " ")

myw.hdw.cpu  = require("abdo.widget.cpu")
myw.hdw.mem  = require("abdo.widget.mem")
myw.hdw.temp = require("abdo.widget.thermal")

myw.hdw.cpuwdg = wibox.widget.textbox()
myw.hdw.memwdg = wibox.widget.textbox()
myw.hdw.tempwdg = wibox.widget.textbox()

myw.hdw.cpuval = "?"
myw.hdw.memval = "?"
myw.hdw.tempval = '?'

function myw.hdw.update()
    local args = myw.hdw.cpu(nil)
    if args and args[1] and args[1] ~= myw.hdw.cpuval then
        local color = wiboxcolor(0, 100, args[1])
        local text = colortext(string.format("%s%% ", args[1]), color)
        myw.hdw.cpuval = args[1]
        myw.hdw.cpuwdg:set_markup(text)
    end

    local args = myw.hdw.temp(nil, host_config['thermal'])
    if args and args.temp and args.temp ~= myw.hdw.tempval then
        local color = wiboxcolor(35, 70, args.temp)
        local text = colortext(string.format("%sºC ", args.temp), color)
        myw.hdw.tempval = args.temp
        myw.hdw.tempwdg:set_markup(text)
    end

    local args = myw.hdw.mem(nil)
    if args and args[1] and args[1] ~= myw.hdw.memval then
        local color = wiboxcolor(0, 100, args[1])
        local text = colortext(string.format("%s%% ", args[1]), color)
        myw.hdw.memval = args[1]
        myw.hdw.memwdg:set_markup(text)
    end
end

timers.fast:connect_signal("timeout", myw.hdw.update)



-----------------------------------
-- Network Usage                 --
-----------------------------------

myw.net = {}
myw.net.src = require("abdo.widget.net")

myw.net.dwdg = wibox.widget.textbox()
myw.net.uwdg = wibox.widget.textbox()

myw.net.icon = wibox.widget.textbox()
myw.net.icon:set_markup(wiboxicon('network', beautiful.color_widget) .. " ")

myw.net.value = { up=-1, down=-1 }

function myw.net.update()
    local args = myw.net.src(nil, 'bd-net')
    local up = args['{up_kb}'] or 0.0
    local down = args['{down_kb}'] or 0.0

    if up ~= myw.net.value.up or down ~= myw.net.value.down then

        local upcolor = wiboxcolor(0, 100, up)
        local uptxt = colortext(string.format('%.0f', up), upcolor)

        local downcolor = wiboxcolor(0, 1700, down)
        local downtxt = colortext(string.format('%.0f', down), downcolor)

        myw.net.dwdg:set_markup(downtxt .. ' ')
        myw.net.uwdg:set_markup(uptxt .. ' ')
    end
    myw.net.value.up = up
    myw.net.value.down = down
end

timers.fast:connect_signal("timeout", myw.net.update)



-----------------------------------
-- Mail                          --
-----------------------------------

myw.mail = {}
myw.mail.src = require("abdo.widget.fileval")

myw.mail.icon = wibox.widget.textbox()

myw.mail.outwdg = wibox.widget.textbox()
myw.mail.inwdg = wibox.widget.textbox()

myw.mail.tooltip = awful.tooltip({ objects = {myw.mail.icon, myw.mail.outwdg, myw.mail.inwdg}})

myw.mail.num_inbox = -1
myw.mail.num_queue = -1

function myw.mail.update()
    local mail = {}

    local count_file = os.getenv("AB2_MAIL_DIR") .. "/stats/newcount"
    local mail_file = os.getenv("AB2_MAIL_DIR") .. "/stats/newmail"
    local num = tonumber(myw.mail.src(nil, {count_file, "0"}))

    if num ~= myw.mail.num_inbox then
        local color = beautiful.color_widget

        -- Note: From miscelaneous symbols and pictographs. Not yet standard.
        local icon_empty = wiboxicon('mail', beautiful.color_widget)

        local icon_full = wiboxicon('mail', beautiful.color_widget_alert)

        local icon = icon_empty
        if num == nil then
            color = beautiful.color_widget_alert
            icon = icon_full
            num = "?"

        elseif num == 0 then
            color = beautiful.color_widget_value
            icon = icon_empty

        elseif num > 0 then
            color = beautiful.color_widget_alert
            icon = icon_full

            -- read new mail list
            f = io.open(mail_file, 'r')
            if f ~= nil then
                for line in f:lines() do
                    table.insert(mail, line)
                end
            end
        end

        local text = colortext(string.format("%d", num), color)

        myw.mail.icon:set_markup(icon .. ' ')
        myw.mail.inwdg:set_markup(text .. ' ')
        myw.mail.num_inbox = num
    end

    local count_file = os.getenv("AB2_MAIL_DIR") .. "/stats/queuecount"
    local num = tonumber(myw.mail.src(nil, {count_file, "0"}))

    if num ~= myw.mail.num_queue then
        local color = beautiful.color_widget

        if num == nil then
            color = beautiful.color_widget_alert
            num = "?"

        elseif num == 0 then
            color = beautiful.color_widget_value

        else
            color = beautiful.color_widget_alert

        end

        local text = colortext(string.format("%d", num), color)

        myw.mail.outwdg:set_markup(text .. ' ')
        myw.mail.num_queue = num
    end

    myw.mail.tooltip.textbox:set_text(table.concat(mail, '\n'))
end

myw.mail.icon:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec(apps.mail) end)))

myw.mail.inwdg:buttons(myw.mail.icon:buttons())
myw.mail.outwdg:buttons(myw.mail.icon:buttons())

timers.normal:connect_signal("timeout", myw.mail.update)



-----------------------------------
-- Chat                          --
-----------------------------------

myw.chat = {}
myw.chat.icon = wibox.widget.textbox()

function myw.chat.update (state)
    myw.chat.state = state
    if state == 'alert' then
        myw.chat.icon:set_markup(wiboxicon('chat', beautiful.color_widget_alert))
    elseif state == 'online' then
        myw.chat.icon:set_markup(wiboxicon('chat', beautiful.color_widget))
    else
        myw.chat.icon:set_markup("")
    end
end

myw.chat.update("offline")



-----------------------------------
-- Music                         --
-----------------------------------

myw.mpd = {}
myw.mpd.src  = require("abdo.widget.mpd")
myw.mpd.path = os.getenv("AB2_MUSIC_DIR")

myw.mpd.icon = wibox.widget.textbox()
myw.mpd.icon:set_markup(wiboxicon('music', beautiful.color_widget) .. ' ')


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

    if args['{state}'] == 'playing' then
        icon = wiboxicon('play', beautiful.color_widget_alert)
    elseif args['{state}'] == 'paused' then
        icon = wiboxicon('pause', beautiful.color_widget_value)
    else
        icon = wiboxicon('stop', beautiful.color_widget_value)
    end

    if myw.mpd.current['{file}'] ~= args['{file}'] or
       myw.mpd.current['{state}'] ~= args['{state}'] then
        if args['{state}'] == "playing" then
            myw.mpd.notify_song(args)
        end

        if myw.mpd.current['{state}'] ~= args['{state}'] then
            myw.mpd.stateicon:set_markup(icon .. ' ')
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
myw.vol.widget = wibox.widget.textbox()
myw.vol.value = -1
myw.vol.port_value = -1

function myw.vol.update()
    local args = myw.vol.src(nil)
    local icon
    if args.port ~= myw.vol.port_value then
        if args.port and string.match(args.port, 'headphones') then
            icon = wiboxicon("headphones", beautiful.color_widget)
        else
            icon = wiboxicon("speaker", beautiful.color_widget)
        end

        myw.vol.icon:set_markup(icon .. ' ')
        if args.port then
            myw.vol.port_value = args.port
        end
    end

    if args.vol ~= myw.vol.vol_value then
        local color = wiboxcolor(0, 100, args.vol)
        local text = colortext(string.format("%s%% ", args.vol), color)
        myw.vol.widget:set_markup(text)
        myw.vol.vol_value = args.vol
    end
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

myw.bat.pcicon = wibox.widget.textbox()
myw.bat.pcwidget = wibox.widget.textbox()
myw.bat.rticon = wibox.widget.textbox()
myw.bat.rtwidget = wibox.widget.textbox()

myw.bat.tooltip = awful.tooltip({ objects = {myw.bat.pcwidget, myw.bat.rtwidget, myw.bat.pcicon, myw.bat.rticon}})

myw.bat.percent = -1
myw.bat.rate = -1
myw.bat.time = -1

myw.bat.pcicon:set_markup(wiboxicon("cable", beautiful.color_widget))
myw.bat.rticon:set_markup(wiboxicon("power", beautiful.color_widget))

function myw.bat.update()
    local args = myw.bat.src(nil, "BAT0")
    if args.rate ~= myw.bat.rate then
        local color_rate = beautiful.color_widget

        if args.state == 'charging' then
            color_rate = wiboxcolor(-40, 0, args.rate)
        else
            color_rate = wiboxcolor(7, 30, args.rate)
        end

        local text = colortext(string.format("%4.1fW", args.rate), color_rate)
        myw.bat.rtwidget:set_markup(text)
    end

    if args.percent ~= myw.bat.percent or args.state ~= myw.bat.state then
        local color_percent = wiboxcolor(-100, 0, args.percent)

        local battery_state = {
            full        = " ",
            unknown     = "?",
            charged     = " ",
            charging    = "+",
            discharging = "-"
        }

        local text_state = colortext(battery_state[args.state], color_percent)
        local text_percent = colortext(string.format("%s%%", args.percent), color_percent)

        myw.bat.pcwidget:set_markup(text_state .. text_percent)

        if args.percent > 0 and args.percent <= 5 and args.state ~= 'charging' then
            naughty.notify({title="Low Battery", text="Hey, plug the power cord!",
                            appname="battery"})
        end
    end

    if args.time ~= myw.bat.time then
        myw.bat.tooltip.textbox:set_markup("<b>Time remaining:</b> " .. args.time)
    end

    myw.bat.state = args.state
    myw.bat.rate = args.rate
    myw.bat.percent = args.percent
    myw.bat.time = args.time
end

myw.bat.rtwidget:buttons(awful.util.table.join(awful.button({ }, 1,
    function () exec("gnome-power-statistics") end)))
myw.bat.pcwidget:buttons(myw.bat.rtwidget:buttons())
myw.bat.pcicon:buttons(myw.bat.rtwidget:buttons())
myw.bat.rticon:buttons(myw.bat.rtwidget:buttons())

timers.fast:connect_signal("timeout", myw.bat.update)



-----------------------------------
-- System                        --
-----------------------------------

myw.sys = {}
myw.sys.sync = require("abdo.widget.filex")
-- myw.sys.priv = require("abdo.widget.filex")

myw.sys.syncwdg = wibox.widget.textbox()
myw.sys.privwdg = wibox.widget.textbox()
myw.sys.mediawdg = wibox.widget.textbox()

function myw.sys.update()
    -- obtain table of mountpoints
    local mounts = {}
    local media = {}
    local f = io.open("/proc/mounts", "r")
    local fsfilter = { vfat=true, btrfs=true, ext4=true, xfs=true }
    if f then
        for line in f:lines() do
            local dev, mp, fs = line:match("([^ ]+) ([^ ]+) ([^ ]+)")
            if fsfilter[fs] then
                mounts[mp] = fs
                if mp:match("/media/[^ ]+") then
                    media[mp] = fs
                end
            end
        end
    end

    -- obtain synced state
    local sync_state = myw.sys.sync(nil, {os.getenv("XDG_RUNTIME_DIR") .. "/synced"})

    local icon

    if mounts["/home/abdo/priv"] then
        icon = wiboxicon("unlocked", beautiful.color_widget)
    else
        icon = wiboxicon("locked", beautiful.color_widget_alert)
    end
    myw.sys.privwdg:set_markup(icon .. ' ')

    if #media > 0 then
        icon = wiboxicon("usb", beautiful.color_widget)
    else
        icon = wiboxicon("usb", beautiful.color_widget_alert)
    end
    myw.sys.mediawdg:set_markup(icon .. ' ')

    if sync_state then icon = wiboxicon('sync', beautiful.color_widget)
    else               icon = wiboxicon('sync', beautiful.color_widget_alert)
    end
    myw.sys.syncwdg:set_markup(icon .. ' ')
end

timers.fast:connect_signal("timeout", myw.sys.update)



-----------------------------------
-- Clock                         --
-----------------------------------

myw.clock = {}
myw.clock.icon = wibox.widget.textbox()
myw.clock.icon:set_markup(wiboxicon("clock", beautiful.color_widget) .. ' ')

myw.clock.clockwdg = wibox.widget.textclock(colortext("%a %d %b %H:%M ", beautiful.color_widget_value))



-----------------------------------
-- Keyboard                      --
-----------------------------------

myw.keyb = {}
myw.keyb.icon = wibox.widget.textbox()
myw.keyb.icon:set_markup(wiboxicon("keyboard", beautiful.color_widget) .. ' ')

myw.keyb.icon:buttons(awful.util.table.join(
                          awful.button({ }, 1, function () osk() end)))

myw.keyb.keybwdg = awful.widget.keyboardlayout()
myw.keyb.keybwdg.layout_name = function (v) return v.file end
capi.awesome.emit_signal("xkb::map_changed")


-----------------------------------
-- Systray                       --
-----------------------------------

myw.systray = wibox.widget.systray()



-----------------------------------
-- Taglist                       --
-----------------------------------

myw.taglist = {}

myw.taglist.buttons = awful.util.table.join(
    awful.button({ }, 1, function(t) t:view_only() end),
    awful.button({ }, 3, awful.tag.viewtoggle)
--   awful.button({ modkey }, 1, awful.client.movetotag),
--   awful.button({ modkey }, 3, awful.client.toggletag)
--                    awful.button({ }, 4, awful.tag.viewnext),
--                    awful.button({ }, 5, awful.tag.viewprev)
)

awful.screen.connect_for_each_screen(function(s)
    myw.taglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, myw.taglist.buttons)
end)



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
                    c.first_tag:view_only()
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
        end),

    awful.button({ }, 5,
        function ()
            awful.client.focus.byidx(-1)
        end)
)

awful.screen.connect_for_each_screen(function(s)
   myw.tasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, myw.tasklist.buttons)
end)



-----------------------------------
-- Layout box                    --
-----------------------------------

myw.layoutbox = {}

awful.screen.connect_for_each_screen(function(s)
   myw.layoutbox[s] = awful.widget.layoutbox(s)
   myw.layoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(1, nil, layouts) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1, nil, layouts) end),
                           awful.button({ }, 4, function () awful.layout.inc(1, nil, layouts) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1, nil, layouts) end)))
 end)


-----------------------------------
-- Prompt box                    --
-----------------------------------

myw.promptbox = {}
awful.screen.connect_for_each_screen(function(s)
    myw.promptbox[s] = awful.widget.prompt()
end)



-----------------------------------
-- Start the widgets             --
-----------------------------------

start_widgets()
