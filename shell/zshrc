#!/bin/zsh
#------------------------------------------------------------------#
# File:     .zshrc   ZSH resource file for interactive shells      #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

# Arch prompt, just in case
PROMPT='[%n@%m %1~] $ '

# In case of dumb terminal, like tramp login
if [ "$TERM" = "dumb" ]; then
    unsetopt zle prompt_cr prompt_subst
    PS1='$ '
    return
fi

# Disable ^S ^Q to stop start the output
stty stop undef
stty start undef

# Stop here if unknown terminal
case $TERM in
    rxvt*|xterm*|eterm-color)   # graphical terminal
        ;; 

    screen*|linux*)             # tty or screen / tmux.
        unset DISPLAY
        ;;  

    vt*)                        # serial console.
        unset DISPLAY
        ;;  

    *)                          # unknown terminal, stop right here.
        return
        ;;   
esac

# source environment variables
[ -f "$HOME/.zshenv" ] && source "$HOME/.zshenv"

# source files in ~/.zsh
_ZSH_DIR="$HOME/.zsh"
if [ -d "$_ZSH_DIR" ]; then
    # NOTE: We put the find at the end because otherwise the while is
    # run in a sub-shell...
    while read src; do
        # we admit symlinks, but only source them if thay are not broken
        src_path="$(readlink -f "$src")"
        [ -f "$src_path" ] && source "$src_path"
    done < <(find "$_ZSH_DIR/" -maxdepth 1 -regex '^.*\.zsh$' | sort)
fi



# ----------------------------
# Hooks
# ----------------------------

autoload -U add-zsh-hook

# catch the return value before setting any prompt.
save_return_value() { ANS=$?; };
add-zsh-hook precmd save_return_value        

# TODO: this is part of the old vcs code, but maybe I want to use them
# on the new stuff!
# add-zsh-hook chpwd update_current_vcs_vars     
# add-zsh-hook precmd refresh_current_vcs_vars

# set window title
add-zsh-hook precmd set-window-title           # set window title
# add-zsh-hook preexec set-window-title

# prompt
setopt prompt_subst
PROMPT='$(abdo_prompt_main)'
PROMPT2='$(abdo_prompt_cont)'

# startup message
# abdo_prompt_messagehello

