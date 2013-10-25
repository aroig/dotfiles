#!/bin/zsh
#------------------------------------------------------------------#
# File:     .zshrc   ZSH resource file for interactive shells      #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


# ----------------------------
# In case of tramp login
# ----------------------------

if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PS1='$ '
    return
fi


# ----------------------------
# Tty stuff
# ----------------------------

# Disable ^S ^Q to stop start the output
stty stop undef
stty start undef


# ----------------------------
# Modifying fpath
# ----------------------------
fpath=($HOME/.zsh/completions $fpath)



# ----------------------------
# Loading generic stuff
# ----------------------------

# Stop here if unknown terminal
case $TERM in
    rxvt*|screen*|xterm*|linux*)
	    ;;
    
    *)
	    return
	    ;;
esac

# Load colors if possible
autoload -U colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi

autoload -U add-zsh-hook


# ----------------------------
# Set environment
# ----------------------------

if [ -f $HOME/.zshenv ]; then
    source $HOME/.zshenv
fi


# ----------------------------
# Set variables for gpg agent
# ----------------------------

# symlink the socket so that a single name survives gpg-agent restarts
if [ -f "${HOME}/.gpg-agent-info" ]; then
    source "${HOME}/.gpg-agent-info"

    mkdir -p "$XDG_RUNTIME_DIR/gpg-agent"

    # this anonymous function keeps variables inside local
    function () {
        local gpg_suffix="${GPG_AGENT_INFO#*:}"
        local gpg_socket="$XDG_RUNTIME_DIR/gpg-agent/gpg-agent.socket"
        local ssh_socket="$XDG_RUNTIME_DIR/gpg-agent/ssh-agent.socket"

        # create socket symplinks 
        ln -sf "${GPG_AGENT_INFO%%:*}" "$gpg_socket"
        ln -sf "$SSH_AUTH_SOCK" "$ssh_socket"

        # export variables
        export GPG_AGENT_INFO="$gpg_socket:$gpg_suffix"
        export SSH_AUTH_SOCK="$ssh_socket"
    }
fi


# ----------------------------
# Set aliases
# ----------------------------

if [ -f $HOME/.aliases ]; then
    source $HOME/.aliases
fi


# ----------------------------
# Source files in .zsh
# ----------------------------

_ZSH_DIR="$HOME/.zsh"
if [ -d $_ZSH_DIR ]; then
    for src in $_ZSH_DIR/*.zsh; do
	    source $src
    done
fi


# ----------------------------
# Hooks
# ----------------------------

add-zsh-hook precmd save_return_value          # catch the return value

add-zsh-hook chpwd update_current_vcs_vars     # update vcs info
add-zsh-hook precmd refresh_current_vcs_vars

add-zsh-hook precmd set-window-title           # set window title
# add-zsh-hook preexec set-window-title


# ----------------------------
# Startup actions
# ----------------------------

messagehello                  # welcome message for tmux sessions
