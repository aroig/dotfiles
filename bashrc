#!/bin/bash
#------------------------------------------------------------------#
# File:     .bashrc   Bash resource file                           #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

# Arch prompt, just in case
PS1='[\u@\h \W]\$ '



#------------------------------
# Set environment
#------------------------------

# source environment variables
[ -f "$HOME/.zshenv" ] && source "$HOME/.zshenv"

# If not running interactively, do nothing
# [[ $- != *i* ]] && return

# source files in ~/.bash
_BASH_DIR=$HOME/.bash
if [ -d "$_BASH_DIR" ]; then
    # NOTE: We put the find at the end because otherwise the while is
    # run in a sub-shell...
    while read src; do
        # we admit symlinks, but only source them if thay are not broken
        src_path="$(readlink -f "$src")"
        [[ -f "$src_path" ]] && source "$src_path"
    done < <(find "$_BASH_DIR/" -maxdepth 1 -regex '^.*\.sh$' | sort)
fi



# ----------------------------
# Hooks
# ----------------------------

# We set the prompt from here. bash is not as flexible as zsh. Can't set colors
# dynamically and escape the color sequences with \[ and \] all from a function
# that runs at "prompt time". So we add a hook that sets the prompt from scratch
# every time!

abdo_set_bash_prompt() {
    # save it for later
    ANS="$?"
    PS1=$(abdo_prompt_main)
};
PROMPT_COMMAND=abdo_set_bash_prompt



#------------------------------
# Set Prompt
#------------------------------

# PS1="\$(abdo_prompt_main)"
PS2="\$(abdo_prompt_cont)"

