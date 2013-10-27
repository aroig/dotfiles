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

if [ -f $HOME/.zshenv ]; then
    source $HOME/.zshenv
fi


# If not running interactively, do nothing
# [[ $- != *i* ]] && return


#------------------------------
# Set aliases
#------------------------------

if [ -f $HOME/.aliases ]; then
    source $HOME/.aliases
fi


#------------------------------
# Source files in .bash
#------------------------------

_BASH_DIR=$HOME/.bash
if [ -d $_BASH_DIR ]; then
    for src in $_BASH_DIR/*.sh; do
	    source $src
    done
fi

