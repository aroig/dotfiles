#!/bin/bash
#------------------------------------------------------------------#
# File:     .bashrc   Bash resource file                           #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

# Arch prompt, just in case
PS1='[\u@\h \W]\$ '


#------------------------------
# Variables
#------------------------------

HOST=`hostname`



#------------------------------
# Set environment
#------------------------------

# source environment variables
[[ -f $HOME/.zshenv ]] && . $HOME/.zshenv

# source aliases
[[ -f $HOME/.aliases ]] && . $HOME/.aliases

# If not running interactively, do nothing
# [[ $- != *i* ]] && return

# source files in ~/.bash
_BASH_DIR=$HOME/.bash
if [ -d $_BASH_DIR ]; then
    # NOTE: We put the find at the end because otherwise the while is
    # run in a sub-shell...
    while read src; do
        # we admit symlinks, but only source them if thay are not broken
        src_path=$(readlink -f $src)
        [ -f "$src_path" ] && source "$src_path"
    done < <(find "$_BASH_DIR/" -maxdepth 1 -regex '^.*\.sh$' | sort)
fi



#------------------------------
# Set Prompt
#------------------------------

PS1="$(promptabdo) $(promptdir) $(promptsymbol $ANS) "
PS2="$(promptcont) "

