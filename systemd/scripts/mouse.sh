#!/bin/bash


# get mouse ID
mouseid=`xinput --list | perl -ne 'm/(Mouse|Touchpad)\s*id=([0-9]*)/ && print "$2"'`

if [ "$mouseid" ]; then

    case "$1" in
        lock)   xinput --set-prop "$mouseid" "Device Enabled" "0" ;;
        unlock) xinput --set-prop "$mouseid" "Device Enabled" "1" ;;
    esac
fi
