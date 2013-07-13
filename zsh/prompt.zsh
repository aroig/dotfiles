#!/bin/zsh
#------------------------------------------------------------------#
# File:     prompt.zsh         Prompt                              #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


promptwindow () {
    local window
    if [[ "$TMUX" != "" ]]; then
        window=$(tmux display-message -p '#I')
        window="%{$fg_bold[white]%}[$window]%{$reset_color%} "
    else
        window=""
    fi
    echo "$window"
}

promptuser () {
    local user_fmt=""
    case $(id -u -n) in
	    root) user_fmt="%{$fg[red]%}$(id -u -n)"   ;;
        *)    user_fmt="%{$fg[green]%}$(id -u -n)" ;;
    esac
    echo "$user_fmt%{$reset_color%}"
}


prompthost () {
    local host_fmt=""

    case "$HOST" in
	    grothendieck) host_fmt="%{$fg[yellow]%}$HOST"       ;;
        hodge)        host_fmt="%{$fg_bold[blue]%}$HOST"    ;;
        galois)       host_fmt="%{$fg_bold[red]%}$HOST"     ;;
        skynet)       host_fmt="%{$fg_bold[cyan]%}$HOST"    ;;
        ada)          host_fmt="%{$fg_bold[magenta]%}$HOST" ;;        
        *)            host_fmt="%{$fg_bold[white]%}$HOST"   ;;
    esac
    echo "$host_fmt%{$reset_color%}"
}

promptsymbol () {
    local psymb
    local promptcol
   
    case "$USER" in
        root) psymb='#' ;;
        *)    psymb='$' ;;
    esac
    
    case "$1" in
        0) promptcol="$fg[white]"  ;;
        *) promptcol="$fg[red]"    ;;
    esac

    echo "%{$promptcol%}${psymb}%{$reset_color%}"
}

# Unicode symbols ↯ ☼ ☠ ☺ ☻ ✓ ⚡ ⚪ ⚬ ⚫ ☀ ⦁ √ ⋆ 

promptvcs () {
    local vcremote
    case $__CURRENT_VCS_REMOTE_STATUS in
        sync)      vcremote="%{$fg[green]%}=%{$reset_color%}"   ;;
	    ahead)     vcremote="%{$fg[blue]%}>%{$reset_color%}"    ;;
        behind)    vcremote="%{$fg[magenta]%}<%{$reset_color%}" ;;
        divergent) vcremote="%{$fg[red]%}Y%{$reset_color%}"     ;;
        unknown)   vcremote="%{$fg[yellow]%}?%{$reset_color%}"  ;;        
        *)         vcremote=""                                  ;;
    esac

    local vcstatus
    case $__CURRENT_VCS_STATUS in
        sync)      vcstatus="%{$fg_bold[green]%}√%{$reset_color%}" ;;
 	    staged)    vcstatus="%{$fg[green]%}*%{$reset_color%}"      ;;
        changed)   vcstatus="%{$fg[red]%}*%{$reset_color%}"        ;;
        untracked) vcstatus="%{$fg[red]%}+%{$reset_color%}"        ;;
        deleted)   vcstatus="%{$fg[red]%}-%{$reset_color%}"        ;;        
	    conflict)  vcstatus="%{$fg[red]%}X%{$reset_color%}"        ;;
        ignored)   vcstatus="%{$fg[white]%}·%{$reset_color%}"      ;;        
        *)         vcstatus="%{$fg[yellow]%}?%{$reset_color%}"     ;;
    esac

    local vcbranch="$__CURRENT_VCS_BRANCH"
    local vcrev="$__CURRENT_VCS_REV"

    case $__CURRENT_VCS_PROGRAM in
        git) echo "${vcstatus}%{$fg[blue]%}(${vcbranch})${vcremote}%{$reset_color%}" ;;
        hg)  echo "${vcstatus}%{$fg[blue]%}(${vcrev})%{$reset_color%}"               ;;
        bzr) echo "${vcstatus}%{$fg[blue]%}(${vcrev})%{$reset_color%}"               ;;        
        *)   echo ""                                                                 ;;
    esac
}

promptabdo () {

    local prompt
    case $__CURRENT_VCS_PROGRAM in
        git|hg|bzr) prompt="$(promptwindow)$(promptuser)@$(prompthost) $(promptvcs)"  ;;
	    none|*)    prompt="$(promptwindow)$(promptuser)@$(prompthost)"                ;;
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
	    rxvt-unicode*) echo "%{$FX[italic]$fg_bold[yellow]%}$currdir%{$reset_color%}" ;;
        *)             echo "%{$fg_bold[yellow]%}$currdir%{$reset_color%}"            ;;
    esac
}


messagehello() {
#    echo "$fg_bold[blue]Arch Linux.$reset_color"

    local creationdate
    if [[ "$TMUX" != "" ]]; then
	    creationdate=$(tmux list-sessions | grep ssh | sed 's/^[0-9a-zA-Z :]*(created\s*\([^\[]*\)).*$/\1/g')
	    if [[ "$creationdate" != "" ]]; then
	        echo "$fg_bold[yellow]Tmux$reset_color session created on $creationdate."
	        echo ""
	    fi
    fi
}


save_return_value() {
    ANS=$?
}

# Set the prompt
setopt prompt_subst
PROMPT='$(promptabdo) $(promptdir) $(promptsymbol $ANS) '
PROMPT2='%{$fg_bold[yellow]%} %_%{$reset_color%}> '

