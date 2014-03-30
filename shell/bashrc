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
[[ -f $HOME/.zshenv ]] && . $HOME/.zshenv

# source aliases
[[ -f $HOME/.aliases ]] && . $HOME/.aliases

# If not running interactively, do nothing
# [[ $- != *i* ]] && return

# source files in ~/.bash
_BASH_DIR=$HOME/.bash
if [ -d $_BASH_DIR ]; then
    for src in $_BASH_DIR/*.sh; do
	    source $src
    done
fi

