#!/bin/bash 

# Wait until some network interface gets online status. If connman is around it
# is the one responsible for managing networks, otherwise use systemd-networkd.

if systemctl is-active -q connman.service; then
    while true; do
        dbus-send --system --type=method_call --print-reply --dest=net.connman / net.connman.Manager.GetProperties | grep "ready\|online" > /dev/null
        ret="$?"
        if [ "$ret" = "0" ]; then break; fi
        sleep 1
    done
    echo "connman is online"

elif systemctl is-active -q systemd-networkd.service; then
    /usr/lib/systemd/systemd-networkd-wait-online
    echo "networkd is online"

fi
