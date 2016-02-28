#------------------------------
# Starting and killing X
#------------------------------

stx() {
    
    # set the vt number from current vt
    systemctl --user set-environment "XDG_VTNR=$XDG_VTNR"

    # start the desktop according to the device
    local chassis=$(hostnamectl status | awk '/Chassis/{print $2}')
    case "$chassis" in
        tablet)  systemctl --user start tablet.target ;;
        laptop)  systemctl --user start laptop.target ;;
        desktop) systemctl --user start desktop.target ;;
        *)       systemctl --user start desktop.target ;;
    esac

    # wait until X server finishes
    while [ `pgrep Xorg` ]; do
        sleep 1
        inotifywait -q -e close /tmp/X11-unix/X0
    done
    
    # reset tty and reset colors
    reset
    set_tty_colors
}

klx() { 
    systemctl --user start console.target;
}
