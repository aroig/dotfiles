#------------------------------
# Starting and killing X
#------------------------------

stx() {
    # export environment to systemd
    export DISPLAY=:0
    systemctl --user import-environment DISPLAY XDG_VTNR
    
    # start the desktop according to the device
    local chassis=$(hostnamectl status | awk '/Chassis/{print $2}')
    case "$chassis" in
        tablet)  systemctl --user start tablet.target ;;
        laptop)  systemctl --user start laptop.target ;;
        desktop) systemctl --user start desktop.target ;;
        *)       systemctl --user start desktop.target ;;
    esac

    # wait until X server finishes
    while [ -e "/tmp/.X11-unix/X0" ]; do
        sleep 1
        inotifywait -qq -e close "/tmp/.X11-unix/X0"
    done

    # unset systemd environment
    systemctl --user unset-environment DISPLAY XDG_VTNR

    # reset tty and reset colors
    # NOTE: I think I do not need to reset anymore
    # reset
    # set_tty_colors
}

klx() {
    systemctl --user start console.target;
}
