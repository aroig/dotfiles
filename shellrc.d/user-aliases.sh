#!/usr/bin/bash

#------------------------------
# Machine aliases
#------------------------------

alias vms='vmctl start'
alias vmt='vmctl start-tty'
alias vmv='vmctl start-vnc'
alias vmh='vmctl start-ssh'
alias vmk='vmctl stop'
alias vmm='vmctl mount'
alias vmu='vmctl umount'

alias nspawn='systemd-nspawn -b -n -D'


#------------------------------
# System
#------------------------------

cgls-colorize() {
    local sedrules=()
    local escape=$(printf "\e")
    for r in $(printf "%s" "${SYSTEMD_COLORS}" | tr ':' ' '); do
        local pattern="${r%=*}"
        local color="${r##*=}"

        # Only match against *.xxx things
        local pattern2="${pattern#\*.}"
        if [ ! "${pattern}" = "${pattern2}" ]; then
            rule="s/─\b\([a-zA-Z0-9.:@~/_\\-]*\.${pattern2}\)\b/─${escape}[${color}m\1${escape}[0m/g"
            sedrules+=("$rule")
        fi
    done
    sed $(printf "-e %s " "${sedrules[@]}")
}

scgls() {
    systemd-cgls --all --full "/system.slice" | cgls-colorize
}

ucgls() {
    systemd-cgls -all --full "/user.slice" | cgls-colorize
}

mcgls() {
    systemd-cgls -all --full "/machine.slice" | cgls-colorize
}


alias unls="sdls units"

# other system tools
alias cmctl="connmanctl"
alias udctl="udisksctl"

# disable rm and implement a trash
alias rm='printf >&2 "\e[31mError\e[0m: rm disabled for interactive use.\nUse trash instead.\n"; false'

trash() {
    prefix=$(date +%s)
    for f in "$@"; do
        mv -n -T "$f" "${AB2_TRASH_DIR}/${prefix}_$(basename "$f")"
    done
}

alias stx="start-desktop"
alias klx="systemctl --user start console.target"


#------------------------------
# Auxiliar functions
#------------------------------

xrandr_screens() { xrandr -q | grep " connected" | sed 's/^\([A-Za-z0-9\-]*\).*/\1/'; }

# remove trailing newline.
trimtrail() {
    cat "$1" | awk '{a = $0; while (getline > 0) {print a; a = $0}} END {printf("%s", a)}' -
}



#------------------------------
# Wine aliases
#------------------------------
alias winec='wineconsole --backend=curses'

alias kindle='wine "$HOME/.wine/drive_c/Program Files (x86)/Amazon/Kindle/Kindle.exe"'
alias digitaleditions='wine "$HOME/.wine/drive_c/Program Files (x86)/Adobe/Adobe Digital Editions/digitaleditions.exe"'

