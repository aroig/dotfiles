#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     prompt.sh          Prompt                              #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


promptwindow () {
    local window
    if [[ "$TMUX" != "" ]]; then
        window=$(tmux display-message -p '#I')
        window="${fg_bold[white]}[$window]${fx[reset]} "
    else
        window=""
    fi
    echo "$window"
}


promptuser () {
    local user_fmt=""
    case $(id -u -n) in
	    root) user_fmt="${fg[red]}$(id -u -n)"   ;;
        *)    user_fmt="${fg[green]}$(id -u -n)" ;;
    esac
    echo "$user_fmt${fx[reset]}"
}


promptat () {
    if [ -n "$ZSH_VERSION" ]; then
        echo "@"
    elif [ -n "$BASH_VERSION" ]; then
        echo "${fg[cyan]}@${fx[reset]}"
    else
        echo "${fg[magenta]}@${fx[reset]}"
    fi
}


prompthost () {
    local host_fmt=""

    case "$HOST" in
	    grothendieck) host_fmt="${fg[yellow]}$HOST"       ;;
        hodge)        host_fmt="${fg_bold[blue]}$HOST"    ;;
        galois)       host_fmt="${fg_bold[red]}$HOST"     ;;
        skynet)       host_fmt="${fg_bold[cyan]}$HOST"    ;;
        ada)          host_fmt="${fg_bold[magenta]}$HOST" ;;        
        *)            host_fmt="${fg_bold[white]}$HOST"   ;;
    esac
    echo "$host_fmt${fx[reset]}"
}


promptsymbol () {
    local psymb
    local promptcol

    case "$1" in
        0) promptcol="${fg[white]}"  ;;
        *) promptcol="${fg[red]}"    ;;
    esac

    if [ -n "$ZSH_VERSION" ]; then
        psymb='$'

    elif [ -n "$BASH_VERSION" ]; then
        psymb='#'
        promptcol="${fg[cyan]}"

    else
        psymb='%'
        promptcol="${fg[magenta]}"
    fi   

    echo "${promptcol}${psymb}${fx[reset]}"
}


# Unicode symbols ↯ ☼ ☠ ☺ ☻ ✓ ⚡ ⚪ ⚬ ⚫ ☀ ⦁ √ ⋆ 
promptvcs () {
    local vcremote
    case $__CURRENT_VCS_REMOTE_STATUS in
        sync)      vcremote="${fg[green]}=${fx[reset]}"   ;;
	    ahead)     vcremote="${fg[blue]}>${fx[reset]}"    ;;
        behind)    vcremote="${fg[magenta]}<${fx[reset]}" ;;
        divergent) vcremote="${fg[red]}Y${fx[reset]}"     ;;
        unknown)   vcremote="${fg[yellow]}?${fx[reset]}"  ;;        
        *)         vcremote=""                            ;;
    esac

    local vcstatus
    case $__CURRENT_VCS_STATUS in
        sync)      vcstatus="${fg_bold[green]}√${fx[reset]}" ;;
 	    staged)    vcstatus="${fg[green]}*${fx[reset]}"      ;;
        changed)   vcstatus="${fg[red]}*${fx[reset]}"        ;;
        untracked) vcstatus="${fg[red]}+${fx[reset]}"        ;;
        deleted)   vcstatus="${fg[red]}-${fx[reset]}"        ;;        
	    conflict)  vcstatus="${fg[red]}X${fx[reset]}"        ;;
        ignored)   vcstatus="${fg[white]}·${fx[reset]}"      ;;        
        *)         vcstatus="${fg[yellow]}?${fx[reset]}"     ;;
    esac

    local vcbranch="$__CURRENT_VCS_BRANCH"
    local vcrev="$__CURRENT_VCS_REV"

    case $__CURRENT_VCS_PROGRAM in
        git) echo "${vcstatus}${fg[blue]}(${vcbranch})${vcremote}${fx[reset]}" ;;
        hg)  echo "${vcstatus}${fg[blue]}(${vcrev})${fx[reset]}"               ;;
        bzr) echo "${vcstatus}${fg[blue]}(${vcrev})${fx[reset]}"               ;;        
        *)   echo ""                                                           ;;
    esac
}


promptabdo () {
    local prompt
    case $__CURRENT_VCS_PROGRAM in
        git|hg|bzr) prompt="$(promptwindow)$(promptuser)$(promptat)$(prompthost) $(promptvcs)"  ;;
	    none|*)    prompt="$(promptwindow)$(promptuser)$(promptat)$(prompthost)"                ;;
    esac
    echo "$prompt"
}


promptdir () {
    local currdir
    case "$PWD" in
        /home/$USER) currdir="~"                ;;
        *)           currdir="$(basename $PWD)" ;;
    esac   

    case "$TERM" in
	    rxvt*|xterm*) echo "${fx[italic]}${fg_bold[yellow]}$currdir${fx[reset]}" ;;
        *)            echo "${fg_bold[yellow]}$currdir${fx[reset]}"              ;;
    esac
}


promptcont () {
    local symb
    if [ -n "$ZSH_VERSION" ]; then
        symb=' %_'
    else
        symb=' ·· '
    fi
    echo "${fg_bold[yellow]}$symb${fx[reset]}>"
}


messagehello() {
#    echo "${fg_bold[blue]}Arch Linux.${fx[reset]}"

    local creationdate
    if [[ "$TMUX" != "" ]]; then
	    creationdate=$(tmux list-sessions | grep ssh | sed 's/^[0-9a-zA-Z :]*(created\s*\([^\[]*\)).*$/\1/g')
	    if [[ "$creationdate" != "" ]]; then
	        echo "${fg_bold[yellow]}Tmux${fx[reset]} session created on $creationdate."
	        echo ""
	    fi
    fi
}



