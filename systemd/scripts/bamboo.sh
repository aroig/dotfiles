#!/bin/bash

# Wacom bamboo touch defaults
# stylusarea="0 0 9720 9200"
# toucharea="0 0 9192 4096"

# buttons: left
# screen: left screen
# rotate=none
# sylusarea="0 0 29440 9200"
# toucharea="0 0 18384 4096"

# buttons: top
# screen: left half of left screen
rotate=ccw
stylusarea="0 -27600 14720 9200"
toucharea="0 0 9192 16384"

# set stylus area to right screen
xsetwacom --set "Wacom Bamboo 16FG 4x5 Pen stylus" Area $stylusarea
xsetwacom --set "Wacom Bamboo 16FG 4x5 Pen stylus" Area $stylusarea

# set touchpad area
xsetwacom --set "Wacom Bamboo 16FG 4x5 Finger touch" Area $toucharea

# set orientation
xsetwacom --set "Wacom Bamboo 16FG 4x5 Pen stylus" Rotate $rotate
xsetwacom --set "Wacom Bamboo 16FG 4x5 Pen eraser" Rotate $rotate
xsetwacom --set "Wacom Bamboo 16FG 4x5 Finger touch" Rotate $rotate
xsetwacom --set "Wacom Bamboo 16FG 4x5 Finger pad" Rotate cw

# buttons
xsetwacom --set "Wacom Bamboo 16FG 4x5 Finger pad" Button 1 3
xsetwacom --set "Wacom Bamboo 16FG 4x5 Finger pad" Button 3 1
