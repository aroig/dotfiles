#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     wintitle.sh   Set terminal window title                #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


set-window-title () {
    local cmd

    if [[ "$1" == "" ]]; then
	cmd=""
    else
	cmd=" ($1)"
    fi
    print -Pn "\e]0;%n@%m: %c${cmd}\a"
}


if [ "$ZSH_VERSION" ]; then
    autoload -U add-zsh-hook

    # set window title
    add-zsh-hook precmd set-window-title
fi

