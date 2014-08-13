#!/bin/bash

# find the system jack port, to connect to.
device=$(extempore --print-devices | \
    grep -E "system api\[[0-9]+\]:JACK" | \
    sed 's/^audio device\[\([0-9]*\)\].*$/\1/')

exec extempore --term nocolor --device "$device" $@
