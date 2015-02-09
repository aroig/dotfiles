#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     alias.sh   Aliases                                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


#------------------------------
# Coloring stuff
#------------------------------
alias ls='ls --human-readable --color=auto -F'
alias ll='ls++ --potsf'
alias lt='tree -C'

alias df='pydf'
alias dul='cdu -dLh -i'
alias du='cdu -dh -i'
alias top='htop'

alias diff='diff -u -d'
alias grep='grep --color=auto'

alias gls="abdo_git_ls"


# alias ping='$HOME/Software/conf/color-wrappers/ping'
# alias unisonb='$HOME/Software/conf/color-wrappers/unison -batch'



#------------------------------
# Wine aliases
#------------------------------
alias winec='wineconsole --backend=curses'

alias kindle='wine "$HOME/.wine/drive_c/Program Files (x86)/Amazon/Kindle/Kindle.exe"'
alias digitaleditions='wine "$HOME/.wine/drive_c/Program Files (x86)/Adobe/Adobe Digital Editions/digitaleditions.exe"'


#------------------------------
# Utility aliases
#------------------------------

alias trash='gvfs-trash'
alias vp='vimpager'
alias vc='vimcat'
alias fehp='feh -Tpreview'
alias rsy='rsync -avz --progress --delete'
alias ediff='emacs -diff'
alias egit='emacs -git'
alias mr='mr --stats -t'

alias mailq='msmtp-queue'

alias ahi='ictl hi.target'
alias abye='ictl bye.target'

alias pac='sudo pacman'
alias pat='pactree -c'
alias paf='comm -13 <(pactree host-$(hostname)-cfg  -u | sort) <(pacman -Qsq | sort)'

alias cow='cower'
amr() { ( cd "$AB2_ARCH_DIR/$1"; shift; mr "$@"; ) }

alias ipy='ipython'
alias ipy2='ipython2'

alias wee='weechat-curses'

gateway() { host `ip route list 0/0 | awk '{print $3}'` | awk '{print $5}'; }

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."


#------------------------------
# Auxiliar functions
#------------------------------

xrandr_screens() { xrandr -q | grep " connected" | sed 's/^\([A-Za-z0-9\-]*\).*/\1/'; }

# remove trailing newline.
trimtrail() {
    cat "$1" | awk '{a = $0; while (getline > 0) {print a; a = $0}} END {printf("%s", a)}' -
}



#------------------------------
# Systemd aliases
#------------------------------

alias jtail="jctl -f -n5"
alias jctl="jctl"
alias ectl="jctl --priority=0..3"
alias ictl="ictl"

alias nctl="networkctl --no-pager"
alias mctl="sudo machinectl"
alias lctl="sudo loginctl"
alias sctl="sudo systemctl --system"
alias uctl="systemctl --user"

alias actl="systemd-analyze"
alias nspawn="sudo systemd-nspawn"

alias cgls="sdls cgroups"
alias unls="sdls units"
alias cgtop="systemd-cgtop"

# print active target list
tlst() { systemctl --user --no-legend --state=active --t target list-units "$@" | perl -n -e'/(.*)\.target/ && print "$1\n"'; }

alias cmctl="connmanctl"
alias udctl="udisksctl"

# Manage mounts the 
alias mount-priv="systemctl --user start mount-priv.service"
alias umount-priv="systemctl --user stop mount-priv.service"

alias mount-data="systemctl start data.mount"
alias umount-data="systemctl stop data.mount"

# old udisks way
# alias mount-data="udisksctl mount -b /dev/disk/by-label/galois:data"
# alias umount-data="udisksctl unmount -b /dev/disk/by-label/galois:data"

actl_svg() {
    cmd="$1"; shift
    if [ "$cmd" = "dot" ]; then systemd-analyze dot "$@" | dot -Tsvg
    else                        systemd-analyze "$cmd" "$@"
    fi
}

alias lock='systemctl --user lock.target'

# firewall
fws() { sudo iptables -L firewall; echo ""; sudo iptables -n -L sshguard; }
fwban() { sudo iptables -A sshguard -s "$1" -j DROP; }



#------------------------------
# development
#------------------------------

case $(hostname -s) in
    grothendieck)
        threads=10
        ;;
    galois|hodge)
        threads=3
        ;;
    *)
        threads=1
        ;;
esac

mk() { 
    PATH="/usr/lib/ccache/bin:$PATH" MAKE="make -j$threads" TERM=xterm make "$@"
}

alias make="TERM=xterm make"

alias gan="git annex"



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

vi()  { $EDITOR "$@"; }
ee()  { $EMACS "$@"; }

op()  { xdg-open "$@" &> /dev/null &!; }
rf()  { rifle "$@"; }


# The following commands are aware of the type of terminal they are on. For
# example fm launches GTK file manager on any terminal except tty's or remote logins

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
    case "$TERM" in
        screen*|linux) tx ;;
        *)             rifle -p terminal "$@" ;;
    esac
}

# ranger session
rg()  {
    case "$TERM" in
        screen*|linux) ranger "$@" ;;
        *)             rifle -p ranger "$@" ;;
    esac
}

# open file manager
fm()  {
    case "$TERM" in
        screen*|linux) ranger "$@" ;;
        *)             rifle -p filemanager "$@" ;;
    esac
}


# These commands open awesome dropdown clients
rgd() { echo "ddclient.ranger:newtab('$PWD')"   | awesome-client; }
tmd() { echo "ddclient.terminal:newtab('$PWD')" | awesome-client; }



#------------------------------
# Starting and killing X
#------------------------------

# stx() { startx awesome -- vt$(fgconsole 2>/dev/null); }
stx() {
    
    # set the vt number from current vt
    systemctl --user set-environment "XDG_VTNR=$XDG_VTNR"

    # start the desktop according to the device
    chassis=$(hostnamectl status | awk '/Chassis/{print $2}')
    case $chassis in
        tablet)  systemctl --user start tablet.target ;;
        laptop)  systemctl --user start laptop.target ;;
        desktop) systemctl --user start desktop.target ;;
        *)       systemctl --user start desktop.target ;;
    esac

    # wait until X server finishes and reset the tty
    wait $(pgrep xorg)
    sleep 1
    reset
    set_tty_colors
}

klx() { 
    systemctl --user start console.target;
}


#------------------------------
# Rebooting and halting
#------------------------------

alias reboot="systemctl reboot"
alias poweroff="systemctl poweroff"
alias suspend="systemctl suspend"

