#!/usr/bin/bash

export DISPLAY=:0

systemctl --user unset-environment GDK_BACKEND QT_QPA_PLATFORM_CLUTTER_BACKEND SDL_VIDEODRIVER WAYLAND_DISPLA>Y
systemctl --user import-environment DISPLAY
