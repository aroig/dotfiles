#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     alias.sh   Aliases                                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

#------------------------------
# Utility aliases
#------------------------------

# ls
alias ls='ls --human-readable --color=auto -F'
alias ll='ls++ --potsf'
alias lt='tree -C'

# filesystem stuff
alias df='dfc'
alias dul='cdu -dLh -i'
alias du='cdu -dh -i'

# monitoring
alias top='htop'
alias gl='glances'

# navigation
alias ud='pushd'
alias od='popd'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."

# file manipulation
alias trash='gvfs-trash'
alias rsy='rsync -avz --progress --delete'
alias rnm='/usr/bin/vendor_perl/rename'

# file viewing
alias vp='vimpager'
alias vc='vimcat'
alias diff='diff -u -d'
alias grep='grep --color=auto'

# emacs aliases
alias ediff='emacs-diff'
alias egit='emacs-git'

# development
alias mr='mr --stats --color -t'
alias make="TERM=xterm make"
alias mk='PATH="/usr/lib/ccache/bin:$PATH" MAKEFLAGS="-j 4" TERM=xterm make'

# git aliases
alias ggr='git --no-pager grep'
alias gan="git annex"
alias glg='git log'
alias gls="abdo_git_ls"

# arch package management
alias pac='sudo pacman'
alias pat='pactree -c'
alias paf='comm -13 <(pactree host-$(hostname)-cfg  -u | sort) <(pacman -Qsq | sort)'
alias cow='cower'

amr() { ( cd "$AB2_ARCH_DIR/$1"; shift; mr "$@"; ) }

# python
alias ipy='ipython'
alias ipy2='ipython2'

# software
alias wee='weechat-curses'
alias fehp='feh -Tpreview'
alias mailq='msmtp-queue'
alias uzbl="uzbl-tabbed"



#------------------------------
# Systemd aliases
#------------------------------

# journal
alias jtail="jctl -f -n5"
alias jctl="jctl"
alias ectl="jctl --priority=0..3"

# systemd tools
alias nctl="networkctl --no-pager"
alias mctl="sudo machinectl"
alias lctl="sudo loginctl"
alias sctl="sudo systemctl --system"
alias uctl="systemctl --user"
alias nspn="sudo systemd-nspawn"
alias lock='systemctl --user lock.target'

# monitoring
alias cgls="sdls cgroups"
alias unls="sdls units"
alias cgtop="systemd-cgtop"

# power management
alias reboot="systemctl reboot"
alias poweroff="systemctl poweroff"
alias suspend="systemctl suspend"

# other system tools
alias cmctl="connmanctl"
alias udctl="udisksctl"

# manage mounts
mnt()  {
    local unit
    local arg="$(systemd-escape -p "$1")"
    case "$1" in
        priv)    instance="user";   unit="mount-priv.service" ;;
        *)       instance="system"; unit="media-$arg.mount"   ;;
    esac

    case "$instance" in
          user)  systemctl --user start "$unit"               ;;        
        system)  sudo systemctl --system start "$unit"        ;;
    esac    
}

umnt() {
    local unit
    local arg="$(systemd-escape -p "$1")"
    case "$1" in
        priv)    instance="user";   unit="mount-priv.service" ;;
        *)       instance="system"; unit="media-$arg.mount"   ;;
    esac

    case "$instance" in
          user)  systemctl --user stop "$unit"                ;;        
        system)  sudo systemctl --system stop "$unit"         ;;
    esac
}

# print active target list
tlst() {
    systemctl --user --no-legend --state=active --t target list-units "$@" | \
        perl -n -e'/(.*)\.target/ && print "$1\n"'
}

# produce an svg dependency graph
sdan_svg() {
    cmd="$1"; shift
    if [ "$cmd" = "dot" ]; then systemd-analyze dot "$@" | dot -Tsvg
    else                        systemd-analyze "$cmd" "$@"
    fi
}

# firewall
fws() { sudo iptables -L firewall; echo ""; sudo iptables -n -L sshguard; }
fwban() { sudo iptables -A sshguard -s "$1" -j DROP; }

# network
gateway() { host `ip route list 0/0 | awk '{print $3}'` | awk '{print $5}'; }



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



#------------------------------
# sudo
#------------------------------

# If the argument has an alias, expand it, if it has a function, 
# run it on a root shell and if no arguments given, go to a root shell.
sudo () {   
    if [[ -n "$1" ]]; then
        if alias "$1" 2>/dev/null >/dev/null; then
            /usr/bin/sudo $SHELL -ic "$*"

        elif type "$1" | grep -q 'function' 2>/dev/null >/dev/null; then
            /usr/bin/sudo $SHELL -ic "$*"

        else
            /usr/bin/sudo "$@"
        fi
    else
        /usr/bin/sudo $SHELL
    fi
}

# alias sudo='sudo '        # Enables expanding aliases for next command. not functions though :(


#------------------------------
# Program shortcuts
#------------------------------

# NOTE1: &! detaches and disowns process. The shell no longer keeps track of it.

# NOTE2: rifle always expects arguments

# NOTE3: rifle either completely detaches or runs a terminal program in-place.

# NOTE4: The following commands are aware of the type of terminal they are on. For
# example fm launches GTK file manager on any terminal except tty's or remote logins


# file openers
op()  {
    xdg-open "$@" &> /dev/null &!
}

rf()  {
    rifle "$@"
}

# terminal editor
vi()  { $EDITOR "$@"; }

# emacs
ee()  {
    if [ "$1" ]; then rifle -p emacs "$1"
    else              rifle -p emacs "$PWD"
    fi    
}

mg() {
    if [ "$1" ]; then rifle -p magit "$1"
    else              rifle -p magit "$PWD"
    fi
}

# open tmux session
tx() {
    if [ "$TMUX" ]; then   tmux new-window
    else                   tmux_session default
    fi
}

# detach from tmux
dt() {
    if [ "$TMUX" ]; then   tmux detach-client
    fi
}

# if inside tmux close window and detach, otherwise just exit
cl() {
    if [ "$TMUX" ]; then   tmux unlink-window -k\; detach-client
    else                   exit 0
    fi
}

# new terminal
tm()  {
    if [ "$1" ]; then rifle -p terminal "$1"
    else              rifle -p terminal "$PWD"
    fi
}

# ranger session
rg()  {
    if [ "$1" ]; then rifle -p ranger "$1"
    else              rifle -p ranger "$PWD"
    fi
}

# open file manager
fm()  {
    if [ "$1" ]; then rifle -p filemanager "$1"
    else              rifle -p filemanager "$PWD"
    fi
}


# These commands open awesome dropdown clients
rgd() {
    echo "ddclient.ranger:newtab('$PWD')"   | awesome-client
}

tmd() {
    echo "ddclient.terminal:newtab('$PWD')" | awesome-client
}



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

