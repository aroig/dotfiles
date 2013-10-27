#!/bin/zsh
#------------------------------------------------------------------#
# File:     .zprofile   ZSH resource file for login shells         #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

source $HOME/.aliases

# ----------------------------
# TTY Setup 
# ----------------------------

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
