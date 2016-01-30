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

alias cgls="sdls cgroups"
alias unls="sdls units"

# other system tools
alias cmctl="connmanctl"
alias udctl="udisksctl"



#------------------------------
# Other System management stuff
#------------------------------

# homedir synchronization
hh() {
    local syncdir="~/arch/sync"
    local cmd=("$1")
    shift

    while getopts ":r:d:" opt; do
        case $opt in
            r) cmd+="REMOTES=$OPTARG" ;;
            d) cmd+="DIRS=$OPTARG" ;;
            *) echo "Unknown argument." >&2;;
        esac
    done
    shift $((OPTIND-1))

    make --no-print-directory --warn-undefined-variables -C "$syncdir" "${cmd[@]}" "$@"
}



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

