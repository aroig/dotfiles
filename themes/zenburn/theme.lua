-------------------------------
--  "Zenburn" awesome theme  --
--    By Adrian C. (anrxc)   --
--    License:  GNU GPL v2   --
-------------------------------

local util = require("awful.util")
local string = string


-- {{{ Main
theme = {}
theme.confdir = util.getdir("config") .. "/themes/zenburn"
-- }}}


-- {{{ Fonts

-- NOTE: the size of desktop and terminal fonts is adjusted via
-- fontconfig on a per-host basis.

-- basic awesome fonts
theme.font         = "desktop 11"
theme.font_mono    = "terminal 11"
theme.font_symbol  = "desktop-symbols 11"

theme.font_naughty = "desktop 14"
theme.font_box     = "terminal 12"
-- }}}

-- {{{ Named Colors
theme.color_fg     = "#dcdccc"
theme.color_bg     = "#3f3f3f"

theme.color_white  = "#dcdccc"
theme.color_gray   = "#7f7f7f"
theme.color_black  = "#1f1f1f"

theme.color_red    = "#ce8686"
theme.color_green  = "#aecf96"
theme.color_yellow = "#edd28a"
theme.color_orange = "#ce9c7b"
theme.color_blue   = "#8cd0d3"

theme.color_sat_red    = "#dd5650"
theme.color_sat_green  = "#3bb463"
theme.color_sat_orange = "#d9732f"
theme.color_sat_blue   = "#4e9de0"
-- }}}


-- {{{ Org priority colors
theme.color_org_priority = {
   ["[#1]"] = "#cd3333",
   ["[#2]"] = "#dd7621",
   ["[#3]"] = "#d0bf8f",
   ["[#4]"] = "#8fb28f",
   ["[#5]"] = "#1e90ff",
}
-- }}}

-- {{{ Widgets
theme.color_widget_gradient = {
    "#aecf96",
    "#aecf96",
    "#dc9435",
    "#ff5656",
}

theme.color_widget          = theme.color_green
theme.color_widget_alert    = theme.color_red
theme.color_widget_gray     = theme.color_gray

-- }}}


-- {{{ Colors
theme.fg_normal = theme.color_fg
theme.bg_normal = theme.color_bg

theme.fg_urgent = theme.color_red
theme.bg_urgent = theme.color_bg

theme.fg_focus  = theme.color_yellow
theme.bg_focus  = theme.color_black

theme.fg_minimize  = theme.color_blue
theme.bg_minimize  = theme.color_bg
-- }}}


-- {{{ Borders
theme.border_width  = "1"
theme.border_normal = theme.color_bg
theme.border_focus  = theme.color_sat_red
theme.border_marked = theme.color_sat_red

theme.border_naughty  = theme.color_sat_green
-- }}}

-- {{{ Titlebars
theme.titlebar_bg_focus  = theme.color_black
theme.titlebar_bg_normal = theme.color_black
-- theme.titlebar_[normal|focus]
-- }}}


-- {{{ Mouse finder
theme.mouse_finder_color = theme.color_red
-- theme.mouse_finder_[timeout|animate_timeout|radius|factor]
-- }}}

-- {{{ Tooltips
-- theme.tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- }}}

-- {{{ Taglist and Tasklist
-- theme.[taglist|tasklist]_[bg|fg]_[focus|urgent]
-- }}}

-- {{{ Menu
-- theme.menu_[height|width]
-- theme.menu_[bg|fg]_[normal|focus]
-- theme.menu_[border_color|border_width]
-- }}}
-- }}}


-- {{{ Wibox font icons from Font Awesome
theme.wibox = {}

theme.wibox.check      = "&#xf00c;"
theme.wibox.cross      = "&#xf00d;"

theme.wibox.memory     = "&#xf145;"   -- ticket: &#xf145;
theme.wibox.cpu        = "&#xf013;"
theme.wibox.disk       = "&#xf0a0;"
theme.wibox.gauge      = "&#xf0e4;"
theme.wibox.network    = "&#xf0e8;"
theme.wibox.wifi       = "&#xf012;"
theme.wibox.sync       = "&#xf021;"

theme.wibox.poweroff   = "&#xf011;"

theme.wibox.music      = "&#xf001;"
theme.wibox.headphones = "&#xf025;"
theme.wibox.speaker    = "&#xf028;"
theme.wibox.mute       = "&#xf026;"

theme.wibox.play       = "&#x23f5;"   -- unassigned unicode: &#x23f5;   fontawesome: &#xf04b;
theme.wibox.stop       = "&#x23f9;"   -- unassigned unicode: &#x23f9;   fontawesome: &#xf04d;
theme.wibox.pause      = "&#x23f8;"   -- unassigned unicode: &#x23f8;   fontawesome: &#xf04c;

theme.wibox.warning    = "&#xf06a;"
theme.wibox.locked     = "&#xf023;"
theme.wibox.unlocked   = "&#xf09c;"

theme.wibox.upload     = "&#xf093;"
theme.wibox.download   = "&#xf019;"
theme.wibox.downup     = "&#x1f504;"   -- unicode symbol
theme.wibox.mail       = "&#xf003;"    -- outline &#xf003;  filled &#xf0e0;

theme.wibox.chat       = "&#xf0e5;"    -- single  &#xf028;  multiple  &#xf086;
theme.wibox.orgmode    = "&#xf022;"

theme.wibox.desktop    = "&#xf108;"
theme.wibox.laptop     = "&#xf109;"

theme.wibox.power      = "&#xf0e7;"
theme.wibox.cable      = "&#x1f50c;"  -- unicode symbol
theme.wibox.battery    = "&#x1f50b;"  -- unicode symbol

theme.wibox.clock      = "&#xf017;"
theme.wibox.calendar   = "&#xf133;"

-- }}}


-- system icons path. I should autodetect it or something
local icon_theme            = "numix/Numix"
local icon_path             = os.getenv("HOME") .. "/.icons/" .. icon_theme .. "/64x64/"

-- {{{ Naughty icons
theme.naughty_mail_icon     = icon_path .. "emblems/emblem-mail.svg"
theme.naughty_chat_icon     = icon_path .. "emblems/emblem-people.svg"
theme.naughty_alert_icon    = icon_path .. "emblems/emblem-important.svg"
theme.naughty_event_icon    = icon_path .. "emblems/emblem-urgent.svg"
theme.naughty_app_icon      = icon_path .. "emblems/emblem-system.svg"

theme.naughty_battery_icon  = icon_path .. "devices/battery.svg"

theme.naughty_mail_sound    = theme.confdir .. "/sounds/soothing/Gentle Roll.wav"
theme.naughty_chat_sound    = theme.confdir .. "/sounds/soothing/Looking Up.wav"
theme.naughty_alert_sound   = theme.confdir .. "/sounds/soothing/Connected.wav"
-- }}}

-- {{{ Taglist icons
theme.taglist_squares_sel   = theme.confdir .. "/icons/taglist/squarefz.png"
theme.taglist_squares_unsel = theme.confdir .. "/icons/taglist/squareza.png"
--theme.taglist_squares_resize = "false"
-- }}}

-- {{{ Misc icons
theme.awesome_icon           = theme.confdir .. "/icons/wibox/awesome.png"
theme.menu_submenu_icon      = theme.confdir .. "/icons/menu/submenu.png"
theme.tasklist_floating_icon = theme.confdir .. "/icons/tasklist/floatingw.png"
-- }}}

-- {{{ Layout icons
theme.layout_tile       = theme.confdir .. "/icons/layouts/tile.png"
theme.layout_tileleft   = theme.confdir .. "/icons/layouts/tileleft.png"
theme.layout_tilebottom = theme.confdir .. "/icons/layouts/tilebottom.png"
theme.layout_tiletop    = theme.confdir .. "/icons/layouts/tiletop.png"
theme.layout_fairv      = theme.confdir .. "/icons/layouts/fairv.png"
theme.layout_fairh      = theme.confdir .. "/icons/layouts/fairh.png"
theme.layout_spiral     = theme.confdir .. "/icons/layouts/spiral.png"
theme.layout_dwindle    = theme.confdir .. "/icons/layouts/dwindle.png"
theme.layout_max        = theme.confdir .. "/icons/layouts/max.png"
theme.layout_fullscreen = theme.confdir .. "/icons/layouts/fullscreen.png"
theme.layout_magnifier  = theme.confdir .. "/icons/layouts/magnifier.png"
theme.layout_floating   = theme.confdir .. "/icons/layouts/floating.png"
-- }}}

-- {{{ Widget icons
theme.widget_temp   = theme.confdir .. "/icons/wibox/temp.png"
theme.widget_cpu    = theme.confdir .. "/icons/wibox/cpu.png"
theme.widget_bat    = theme.confdir .. "/icons/wibox/bat.png"
theme.widget_mem    = theme.confdir .. "/icons/wibox/mem.png"
theme.widget_fs     = theme.confdir .. "/icons/wibox/disk.png"
theme.widget_netdw  = theme.confdir .. "/icons/wibox/down.png"
theme.widget_netup  = theme.confdir .. "/icons/wibox/up.png"
theme.widget_net    = theme.confdir .. "/icons/wibox/sat.png"
theme.widget_maile  = theme.confdir .. "/icons/wibox/mailg.png"
theme.widget_mailf  = theme.confdir .. "/icons/wibox/mailr.png"
theme.widget_rsse   = theme.confdir .. "/icons/wibox/rssg.png"
theme.widget_rssf   = theme.confdir .. "/icons/wibox/rssr.png"
theme.widget_vol    = theme.confdir .. "/icons/wibox/vol.png"
theme.widget_org    = theme.confdir .. "/icons/wibox/cal.png"
theme.widget_date   = theme.confdir .. "/icons/wibox/time.png"
theme.widget_crypto = theme.confdir .. "/icons/wibox/crypto.png"
theme.widget_music  = theme.confdir .. "/icons/wibox/music.png"
theme.widget_pause  = theme.confdir .. "/icons/wibox/pause.png"
theme.widget_play   = theme.confdir .. "/icons/wibox/play.png"
theme.widget_stop   = theme.confdir .. "/icons/wibox/stop.png"
-- }}}

-- {{{ Titlebar icons
theme.titlebar_close_button_focus  = theme.confdir .. "/icons/titlebar/close_focus.png"
theme.titlebar_close_button_normal = theme.confdir .. "/icons/titlebar/close_normal.png"

theme.titlebar_ontop_button_focus_active    = theme.confdir .. "/icons/titlebar/ontop_focus_active.png"
theme.titlebar_ontop_button_normal_active   = theme.confdir .. "/icons/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_inactive  = theme.confdir .. "/icons/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_inactive = theme.confdir .. "/icons/titlebar/ontop_normal_inactive.png"

theme.titlebar_sticky_button_focus_active    = theme.confdir .. "/icons/titlebar/sticky_focus_active.png"
theme.titlebar_sticky_button_normal_active   = theme.confdir .. "/icons/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_inactive  = theme.confdir .. "/icons/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_inactive = theme.confdir .. "/icons/titlebar/sticky_normal_inactive.png"

theme.titlebar_floating_button_focus_active    = theme.confdir .. "/icons/titlebar/floating_focus_active.png"
theme.titlebar_floating_button_normal_active   = theme.confdir .. "/icons/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_inactive  = theme.confdir .. "/icons/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_inactive = theme.confdir .. "/icons/titlebar/floating_normal_inactive.png"

theme.titlebar_maximized_button_focus_active    = theme.confdir .. "/icons/titlebar/maximized_focus_active.png"
theme.titlebar_maximized_button_normal_active   = theme.confdir .. "/icons/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_inactive  = theme.confdir .. "/icons/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_inactive = theme.confdir .. "/icons/titlebar/maximized_normal_inactive.png"
-- }}}


return theme
