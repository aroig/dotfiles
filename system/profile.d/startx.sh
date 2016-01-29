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

    # wait until X server finishes and reset the tty
    wait $(pgrep Xorg)
    sleep 1

    # reset tty and reset colors
    reset
    set_tty_colors
}

klx() { 
    systemctl --user start console.target;
}

