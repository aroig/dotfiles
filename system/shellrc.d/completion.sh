#!/bin/zsh
#------------------------------------------------------------------#
# File:     completion.zsh     Completion adjustments              #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

if [ "$ZSH_VERSION" ]; then
    
    # add completions to fpath
    fpath=($HOME/.zsh/completions $fpath)

    # enable completion
    zmodload zsh/complist 
    autoload -U compinit
    compinit -u

    # settings
    setopt bash_auto_list     # Bring list on second tab.
    setopt glob_complete      # Autocomplete with glob

    # colorize stuff
    zstyle ':completion:*:default'           list-colors ${(s.:.)LS_COLORS}   # colorize file lists
    zstyle ':completion:*:*:systemctl*'      list-colors ${SYSTEMD_COLORS}    # colorize systemd units

    zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'                 # case insensitive
    zstyle ':completion:*:descriptions' format '%U%B%d%b%u'                   # heading format

    zstyle ':completion:*' menu select=4                                      # menu selection when >= 4 options


    #--------------------------#
    # Function completions     #
    #--------------------------#

    # copy a completions from oldcmd to newcmd
    # compdef newcmd=oldcmd
    compdef mk=make
    
    compdef jctl=journalctl

    # zstyle ':completion::*:expand:*' tag-order all-expansions
    
fi
