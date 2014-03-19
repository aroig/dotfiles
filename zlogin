#!/bin/zsh
#------------------------------------------------------------------#
# File:     .zprofile   ZSH resource file for login shells         #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


#------------------------------
# Set environment
#------------------------------

# source aliases
[[ -f $HOME/.aliases ]] && . $HOME/.aliases

# set perl path
[[ -f /etc/profile.d/perlbin.sh ]] && . /etc/profile.d/perlbin.sh



# ----------------------------
# TTY Setup 
# ----------------------------

# set tty colors on virtual console
[[ "$TERM" = "linux" ]] && set_tty_colors
    


# ----------------------------
# Auto tmux
# ----------------------------

# if ssh outside tmux
if [[ "$SSH_TTY" != "" && "$TMUX" == "" && "$NOTMUX" == "" ]]; then
    if type tmux 2>&1 >/dev/null && type tmux_session 2>&1 >/dev/null; then
        tmux_session term && exit 0
    else
        echo "tmux not installed. Falling back to plain shell."
    fi
fi
