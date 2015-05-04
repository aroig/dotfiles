#!/bin/zsh
#------------------------------------------------------------------#
# File:     history.zsh        History stuff                       #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

HISTFILE="$XDG_RUNTIME_DIR/shell/zsh_history"
HISTSIZE=1000
SAVEHIST=1000

mkdir -p "$(dirname "$HISTFILE")"
setopt incappendhistory 
setopt sharehistory
setopt extendedhistory
