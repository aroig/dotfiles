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
fpath=($HOME/.zshrc.d/completions $fpath)



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
# Source files in .zshrc.d
# ----------------------------

if [ -d $HOME/.zshrc.d ]; then
    ZSHRCD="$HOME/.zshrc.d"
    for src in $ZSHRCD/*.sh; do
	source $src
    done
fi


# ----------------------------
# Set aliases
# ----------------------------

# Environment
if [ -f $HOME/.zshenv ]; then
    source $HOME/.zshenv
fi

# Aliases
if [ -f $HOME/.aliases ]; then
    source $HOME/.aliases
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
