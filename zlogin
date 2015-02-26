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
[ "$ZSH_VERSION" ] && [ -e "~/.zshrc" ] && source "~/.zshrc"

# source bashrc on bash
[ "$BASH_VERSION" ] && [ -e "~/.bashrc" ] && source "~/.bashrc"

# set path for systemd user session
[ "$PATH" ] && systemctl --user set-environment "PATH=$PATH"

# set the VT number from the session
[ "$XDG_VTNR" ] && systemctl --user set-environment "XDG_VTNR=$XDG_VTNR"

# set tty colors on virtual console
[ "$TERM" = "linux" ] && set_tty_colors

# if ssh session, set gpg-agent variables
# TODO: what about this in gpg 2.1?
[ "$SSH_CONNECTION" ] && gpg_agent_mode ssh



# ----------------------------
# Auto tmux
# ----------------------------

# if interactive ssh outside tmux
if [ "$SSH_TTY" ] && [ "$TMUX" = "" ] && [ "$NOTMUX" = "" ]; then
    # start tmux
    if type tmux 2>&1 >/dev/null && type tmux_session 2>&1 >/dev/null; then
        tmux_session default && exit 0
    else
        echo "tmux not installed. Falling back to plain shell."
    fi
fi

