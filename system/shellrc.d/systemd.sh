
#------------------------------
# systemd utility functions
#------------------------------

sd_log() {
    systemd-cat -t "shell" echo "$1"
}

# produce an svg dependency graph
sdan_svg() {
    cmd="$1"; shift
    if [ "$cmd" = "dot" ]; then systemd-analyze dot "$@" | dot -Tsvg
    else                        systemd-analyze "$cmd" "$@"
    fi
}

# manage mounts
mnt()  {
    local unit
    local arg="$(systemd-escape -p "$1")"
    case "$1" in
        priv)    unit="home-abdo-priv.mount" ;;
        *)       unit="media-$arg.mount"     ;;
    esac
    sudo systemctl --system start "$unit"
}

umnt() {
    local unit
    local arg="$(systemd-escape -p "$1")"
    case "$1" in
        priv)    unit="home-abdo-priv.mount" ;;
        *)       unit="media-$arg.mount"     ;;
    esac
    sudo systemctl --system stop "$unit"
}

# print active target list
tls() {
    systemctl --user --no-legend --state=active --t target list-units "$@" | \
        perl -n -e'/(.*)\.target/ && print "$1\n"'
}


#------------------------------
# Systemd aliases
#------------------------------

# journal
alias jtail="jctl -f -n5"
alias jctl="jctl"
alias ectl="jctl --priority=0..3"

# systemd tools
alias nctl="networkctl"
alias mctl="sudo machinectl"
alias lctl="sudo loginctl"
alias sctl="sudo systemctl --system"
alias uctl="systemctl --user"
alias lock='systemctl --user start lock.target'

# monitoring
alias cgtop="systemd-cgtop --depth=10"

# power management
alias reboot="systemctl reboot"
alias poweroff="systemctl poweroff"
alias suspend="systemctl suspend"
