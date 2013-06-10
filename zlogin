#!/bin/zsh
#------------------------------------------------------------------#
# File:     .zprofile   ZSH resource file for login shells         #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


# ----------------------------
# TTY Setup 
# ----------------------------

set_tty_colors() {
    # Colors from .Xresources
    _SEDCMD='s/urxvt\.color\([0-9]\{1,\}\).*#\([0-9a-fA-F]\{6\}\).*/\1 \2/p'
    for i in $(sed -n "$_SEDCMD" $HOME/.Xresources | \
               awk '$1 < 16 {printf "\\e]P%X%s", $1, $2}')
    do
        echo -en "$i"
    done
    echo -e "\\e]P0000000"
}

# Set tty colors on virtual console
if [[ "$TERM" = "linux" ]]; then
    set_tty_colors
fi 
    

# ----------------------------
# Auto tmux
# ----------------------------

# if ssh outside tmux
if [[ "$SSH_TTY" != "" && "$TMUX" == "" && "$NOTMUX" == "" ]]; then
    if which tmux 2>&1 >/dev/null; then
        tmux-session ssh && exit 0
    else
        echo "tmux not installed. Starting zsh now"
    fi
fi
