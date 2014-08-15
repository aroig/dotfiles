#!/bin/bash

# find the system jack port, to connect to.
device=$(extempore --term nocolor --print-devices |
    grep -E "system api\[[0-9]+\]:JACK" |
    sed 's/^audio device\[\([0-9]*\)\].*$/\1/')

echo "Device: $device"
# NOTE: this is sensitive to the position of --term nocolor ?!  if put at the
# beggining it produces non-assci characters, and only when executed from a
# systemd service... weird!
/usr/bin/extempore --device "$device" $@ --term nocolor
