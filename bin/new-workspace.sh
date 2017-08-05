#!/bin/bash


swaymsg -t get_workspaces | tr , '\n' | grep '"num":' | cut -d : -f 2 | sort -rn | head -1

swaymsg workspace "$new_workspace"
