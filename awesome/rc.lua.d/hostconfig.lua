---------------------------------------------------------------
-- File:    hosts.lua     Host specific settings             --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local awful = awful
local util  = util

local hosts = {
    ada = {
        thermal = {"hwmon1", {"temp1"} },
        wibox_height = 18,
    },

    grothendieck = {
        thermal = {"hwmon1", {"temp1"} },
        wibox_height = 18,
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

local hostname  = util.get_hostname()

host_config = hosts[hostname] or host['default']
