#!/bin/zsh
#------------------------------------------------------------------#
# File:     history.zsh        History stuff                       #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

HISTFILE="$XDG_RUNTIME_DIR/zsh/zhistory"
HISTSIZE=1000
SAVEHIST=1000

mkdir -p "$HISTFILE"
setopt incappendhistory 
setopt sharehistory
setopt extendedhistory
