#!/bin/bash

# Script rotates screen, touchscreen and wacom input.


SCREEN_DEVICES=("eDP1")
WACOM_DEVICES=("Wacom ISDv4 EC Pen stylus" "Wacom ISDv4 EC Pen eraser")
TOUCH_DEVICES=("ELAN Touchscreen")
POINT_DEVICES=("SynPS/2 Synaptics TouchPad" "TPPS/2 IBM TrackPoint")



# Enable/disable laptop input devices 
yoga_laptop() {
    cmd="$1"
    case "$cmd" in
        on)
            touch_arg=1
            ;;

        off)
            touch_arg=0
            ;;
    esac

    for device in "${POINT_DEVICES[@]}"; do
        xinput --set-prop "$device" "Device Enabled" $touch_arg
    done
}



# Rotate Screen together with input devices
yoga_rotate() {
    angle="$1"       
    case "$angle" in
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
        xsetwacom set "$device" rotate $wacom_arg
    done

    # adjust touchscreen
    for device in "${TOUCH_DEVICES[@]}" "${POINT_DEVICES[@]}"; do
        xinput set-prop "$device" "Coordinate Transformation Matrix" $touch_arg
    done
}



usage() {
    echo "Usage: yoga <cmd> [<args>]"
}


cmd="$1"
shift
args="$@"

case "$cmd" in
    rotate)
        yoga_rotate $args
        ;;

    laptop)
        yoga_laptop $args
        ;;

    *)
        echo "Unrecognized command '$cmd'"
        exit 1
        ;;
esac

