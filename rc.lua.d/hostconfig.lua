---------------------------------------------------------------
-- File:    hosts.lua     Host specific settings             --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local awful = awful

local hosts = {
    grothendieck = {
        thermal = {"hwmon1", {"temp1"} },
    },

    galois = {
        thermal = {"hwmon1", { "temp2", "temp3"} },
    },

    hodge = {
        thermal = {"hwmon0", { "temp2"} },
    },

    default = {
        thermal = {"hwmon0", {"temp2"} },
    }
}

local hostname  = awful.util.pread("hostname"):gsub("\n", "")

host_config = hosts[hostname] or host['default']
