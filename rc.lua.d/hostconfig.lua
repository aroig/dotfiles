---------------------------------------------------------------
-- File:    hosts.lua     Host specific settings             --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local awful = awful

local hosts = {
    grothendieck = {
        thermal = {"hwmon1", {"temp1"} },
        wibox_height = 16,
    },

    galois = {
        thermal = {"hwmon1", { "temp2", "temp3"} },
        wibox_height = 20,
    },

    hodge = {
        thermal = {"hwmon0", { "temp2"} },
        wibox_height = 16,
    },

    default = {
        thermal = {"hwmon0", {"temp2"} },
        wibox_height = 16,
    }
}

local hostname  = awful.util.pread("hostname"):gsub("\n", "")

host_config = hosts[hostname] or host['default']
