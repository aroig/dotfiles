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

    # TODO: implement this via the bus.
    # wait until X server finishes and reset the tty
    while [ `pgrep Xorg` ]; do
        sleep 1
    done
    
    # reset tty and reset colors
    reset
    set_tty_colors
}

klx() { 
    systemctl --user start console.target;
}

