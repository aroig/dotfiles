#!/bin/bash
#------------------------------------------------------------------#
# File:     history.sh         History stuff                       #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

HISTFILE="$XDG_RUNTIME_DIR/shell/bash_history"
HISTSIZE=1000
SAVEHIST=1000

# Avoid duplicates
export HISTCONTROL=ignoredups:erasedups  

mkdir -p "$(dirname "$HISTFILE")"

shopt -s histappend

sync_history() { history -a; history -c; history -r; }
