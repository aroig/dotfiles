#!/bin/zsh
#------------------------------------------------------------------#
# File:     .zprofile   ZSH resource file for login shells         #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

#------------------------------
# Set environment
#------------------------------

# source zshrc on zsh
if [ "$ZSH_VERSION" ] && [ -e "$HOME/.zshrc" ]; then
    source "$HOME/.zshrc"
    
# source bashrc on bash
elif [ "$BASH_VERSION" ] && [ -e "$HOME/.bashrc" ]; then
    source "$HOME/.bashrc"

else
    echo "zlogin: unrecognized shell"
    exit 0
fi

# source zshenv. This sets the PATH
[ -f "$HOME/.zshenv" ] && source "$HOME/.zshenv"

# set GPG tty
export GPG_TTY=`tty`
systemctl --user set-environment "GPG_TTY=$GPG_TTY"

# set path for systemd user session
if [ "$PATH" ]; then
    systemctl --user set-environment "PATH=$PATH"
fi

# set the VT number from the session
if [ "$XDG_VTNR" ]; then
    systemctl --user set-environment "XDG_VTNR=$XDG_VTNR"
fi

# set tty colors on virtual console
if [ "$TERM" = "linux" ]; then
    set_tty_colors
fi

# start user target
systemctl --user start user.target
