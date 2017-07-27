#------------------------------
# Starting and killing X
#------------------------------

stx() {
    # Export environment to systemd
    export DISPLAY=:0
    export XAUTHORITY="$XDG_RUNTIME_DIR/xorg/Xauthority"
    systemctl --user import-environment DISPLAY XDG_VTNR XAUTHORITY

    # Start desktop session
    local session="${1:-default}"
    if [ -z "$session" ]; then
        printf "No graphical session given\n"
        return 1
    fi
    systemctl --user start "${session}-session.target"

    # Start device configuration target
    local chassis=$(hostnamectl status | awk '/Chassis/{print $2}')
    case "$chassis" in
        tablet)  systemctl --user start tablet.target ;;
        laptop)  systemctl --user start laptop.target ;;
        desktop) systemctl --user start desktop.target ;;
        *)       systemctl --user start desktop.target ;;
    esac

    # Wait synchronously until the xorg server stops
    # TODO: handle wayland sessions here too
    systemctl --user start --wait xorg.service

    # Unset environment
    systemctl --user unset-environment DISPLAY XDG_VTNR XAUTHORITY
    unset DISPLAY XAUTHORITY

    # Reset tty and colors
    reset
    set_tty_colors
}

klx() {
    systemctl --user start console.target;
}
