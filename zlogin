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

# symling $XDG_RUNTIME_DIR from homedir, so we can access it via ~/.runtime
if [[ ! -d "$HOME/.runtime" ]]; then
    [[ -d "$XDG_RUNTIME_DIR" ]] && ln -sf "$XDG_RUNTIME_DIR" "$HOME/.runtime"
fi


#------------------------------
# Set systemd environment
#------------------------------

# set path for systemd user session
systemctl --user set-environment "PATH=$PATH"

# set the VT number from the session
if [ "$XDG_VTNR" ]; then
    systemctl --user set-environment "XDG_VTNR=$XDG_VTNR"
fi



# ----------------------------
# TTY Setup 
# ----------------------------

# set tty colors on virtual console
[[ "$TERM" = "linux" ]] && set_tty_colors
    


# ----------------------------
# Auto tmux
# ----------------------------

# if interactive ssh outside tmux
if [[ "$SSH_TTY" != "" && "$TMUX" == "" && "$NOTMUX" == "" ]]; then
    # start tmux
    if type tmux 2>&1 >/dev/null && type tmux_session 2>&1 >/dev/null; then
        tmux_session default && exit 0
    else
        echo "tmux not installed. Falling back to plain shell."
    fi
fi

# if ssh session, set gpg-agent variables
if [[ "$SSH_CONNECTION" != "" ]]; then
    gpg_agent_mode ssh
fi
