#------------------------------
# Starting and killing X
#------------------------------

stx() {
    # export environment to systemd
    export DISPLAY=:0
    export XAUTHORITY="$XDG_RUNTIME_DIR/xorg/Xauthority"
    systemctl --user import-environment DISPLAY XDG_VTNR XAUTHORITY

    # start the desktop according to the device
    local chassis=$(hostnamectl status | awk '/Chassis/{print $2}')
    case "$chassis" in
        tablet)  systemctl --user start tablet.target ;;
        laptop)  systemctl --user start laptop.target ;;
        desktop) systemctl --user start desktop.target ;;
        *)       systemctl --user start desktop.target ;;
    esac

    # wait synchronously until the xorg server stops
    systemctl --user start --wait xorg.service

    # unset systemd environment
    systemctl --user unset-environment DISPLAY XDG_VTNR XAUTHORITY

    # unset exported variables
    unset DISPLAY XAUTHORITY

    # reset tty and reset colors
    reset
    set_tty_colors
}

klx() {
    systemctl --user start console.target;
}
