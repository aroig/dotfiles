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


# If not running interactively, don't do anything
[[ $- != *i* ]] && return


#------------------------------
# Source files in .zshrc.d
#------------------------------

if [ -d $HOME/.bashrc.d ]; then
    for src in $HOME/.bashrc.d/*.sh; do
	source $src
    done
fi
