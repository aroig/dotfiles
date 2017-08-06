#!/bin/bash

arg="$1"

new=$(comm -13 <(swaymsg -t get_workspaces | sed -n 's/.*"num":\s*\([0-9]\+\).*$/\1/p' | sort) \
               <(seq 0 9) | sort | head -1)

case "$arg" in
    move)
        swaymsg -t command move container to workspace "$new", workspace "$new" >/dev/null
        ;;

    switch)
        swaymsg -t command workspace "$new" >/dev/null
        ;;
esac

