#!/usr/bin/bash

# Launch sway with appropriate environment variables

# keyboard layout
export XKB_DEFAULT_LAYOUT=es
export XKB_DEFAULT_MODEL=pc105
export XKB_DEFAULT_VARIANT=cat

# gtk
export GDK_BACKEND=wayland

# qt5
export QT_QPA_PLATFORM=wayland-egl

# clutter
export CLUTTER_BACKEND=wayland

# SDL
export SDL_VIDEODRIVER=wayland

# display
export WAYLAND_DISPLAY=wayland-0

# export to systemd
systemctl --user import-environment GDK_BACKEND QT_QPA_PLATFORM_CLUTTER_BACKEND SDL_VIDEODRIVER WAYLAND_DISPLAY
systemctl --user unset-environment DISPLAY

# start sway
sway | systemd-cat -t sway
