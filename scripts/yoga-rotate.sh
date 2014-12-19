#!/bin/bash

# Script rotates screen, touchscreen and wacom input.

SCREEN_DEVICES=("eDP1")
# WACOM_DEVICES=("Wacom ISDv4 EC Pen stylus" "Wacom ISDv4 EC Pen eraser")
WACOM_DEVICES=()
TOUCH_DEVICES=("ELAN Touchscreen" "SynPS/2 Synaptics TouchPad" "TPPS/2 IBM TrackPoint")

yoga_rotate() {
    angle="$1"       
    case $angle in
        0)
            xrandr_arg="normal"
            wacom_arg="none"
            touch_arg="1 0 0 0 1 0 0 0 1"
            ;;

        90)
            xrandr_arg="left"
            wacom_arg="ccw"
            touch_arg="0 -1 1 1 0 0 0 0 1"
            ;;

        180)
            xrandr_arg="inverted"
            wacom_arg="half"
            touch_arg="-1 0 1 0 -1 1 0 0 1"
            ;;

        270)
            xrandr_arg="right"
            wacom_arg="cw"
            touch_arg="0 1 0 -1 0 1 0 0 1"
            ;;

        *)
            echo "Unknown angle '$angle'"
            exit 1
            ;;       
    esac

    # adjust monitor orientation
    for device in "${SCREEN_DEVICES[@]}"; do
	    xrandr --output "$device" --rotate $xrandr_arg
    done
        
    # adjust wacom devices
    for device in "${WACOM_DEVICES[@]}"; do
        xsetwacom set "$device" rotate $wacom_arc
    done

    # adjust touchscreen
    for device in "${TOUCH_DEVICES[@]}"; do
        xinput set-prop "$device" "Coordinate Transformation Matrix" $touch_arg
    done
}

usage() {
    echo "Usage: yoga-rotate <angle>"
}


angle="$1"

echo "Setting orientation to $angle degrees"
yoga_rotate "$angle"

