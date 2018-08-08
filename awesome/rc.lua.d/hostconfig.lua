---------------------------------------------------------------
-- File:    hosts.lua     Host specific settings             --
-- Version:                                                  --
-- Author:  Abdó Roig<abdo.roig@gmail.com>                   --
---------------------------------------------------------------

local hostname = hostname

local hosts = {
    ada = {
        thermal = {"hwmon1", {"temp1"} },
    },

    grothendieck = {
        thermal = {"hwmon1", {"temp1"} },
    },

    galois = {
        thermal = {"hwmon1", { "temp2", "temp3"} },
    },

    default = {
        thermal = {"hwmon0", {"temp2"} },
    }
}

host_config = hosts[hostname] or hosts['default']
