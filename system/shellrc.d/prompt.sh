#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     prompt.sh          Prompt                              #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


# ----------------------------
# Prepare colors for the prompt
# ----------------------------

# Colors in the prompt need to be escaped between %{ %} (zsh) or \[ \] (bash). 
# This lets the shell know about characters that do not occupy space. This prevents
#  artifacts when browsing command history, etc.
if [[ -n "$ZSH_VERSION" ]]; then
    _cb="%{"
    _ce="%}"

else        
    _cb="\["
    _ce="\]"  
fi


# ----------------------------
# Git Prompt config
# ----------------------------

GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
GIT_PS1_DESCRIBE_STYLE="branch"
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_STATESEPARATOR=""



# ----------------------------
# Prompt Stuff
# ----------------------------

abdo_prompt_tmux () {
    local win wcol

    wcol="${fg_bold[white]}"

    if [[ -n "$TMUX" ]]; then  win=$(tmux display-message -p '[#I] ')
    else                       win=""
    fi

    echo "${_cb}${wcol}${_ce}$win${_cb}${fx[reset]}${_ce}"
}


abdo_prompt_nix() {
    if type "nix-env" >/dev/null 2>&1; then
        local profile_full="$(readlink ~/.nix-profile)"
        local profile="$(basename "$profile_full")"
        echo "[${_cb}${fg_bold[blue]}${_ce}$profile${_cb}${fx[reset]}${_ce}] "
    fi    
}


abdo_prompt_shell_color() {
    if [[ -n "$ZSH_VERSION" ]]; then    echo "${fg[white]}"
    elif [[ -n "$BASH_VERSION" ]]; then echo "${fg[cyan]}"
    else                                echo "${fg[magenta]}"
    fi
}


abdo_prompt_userhost() {
    local user host ucol hcol acol

    if [[ -n "$ZSH_VERSION" ]]; then
        user=$USER
        host=$HOST
    else
        user=$(id -u -n)
        host=$(hostname -s)
    fi

    acol=$(abdo_prompt_shell_color)

    case "$user" in
	    root)    ucol="${fg[red]}"     ;;
        abdo)    ucol="${fg[green]}"   ;;
        bibrain) ucol="${fg[cyan]}"    ;;
        roberta) ucol="${fg[magenta]}" ;;
        *)       ucol="${fg[white]}"   ;;
    esac

    case "$host" in
	    grothendieck) hcol="${fg[yellow]}"       ;;
        galois)       hcol="${fg_bold[red]}"     ;;
        skynet)       hcol="${fg_bold[cyan]}"    ;;
        quark)        hcol="${fg_bold[blue]}"    ;;
        ada)          hcol="${fg_bold[magenta]}" ;;        
        *)            hcol="${fg_bold[white]}"   ;;
    esac

    echo "${_cb}${ucol}${_ce}$user${_cb}${fx[reset]}${acol}${_ce}@${_cb}${fx[reset]}${hcol}${_ce}$host${_cb}${fx[reset]}${_ce}"
}


abdo_prompt_symbol () {
    local psym pcol

    pcol=$(abdo_prompt_shell_color)
    [[ ! "$ANS" = '0' ]] && pcol="${fg[red]}"

    if [[ -n "$ZSH_VERSION" ]]; then    psym='$'
    elif [[ -n "$BASH_VERSION" ]]; then psym='$'
    else                                psym='%'
    fi   

    echo " ${_cb}${pcol}${_ce}$psym${_cb}${fx[reset]}${_ce}"
}


# Unicode symbols ↯ ☼ ☠ ☺ ☻ ✓ ⚡ ⚪ ⚬ ⚫ ☀ ⦁ √ ⋆ 
# TODO: eclose clors with _cb and _ce
abdo_prompt_vcs_old () {
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
        git) echo " ${vcstatus}${fg[blue]}(${vcbranch})${vcremote}${fx[reset]}" ;;
        hg)  echo " ${vcstatus}${fg[blue]}(${vcrev})${fx[reset]}"               ;;
        bzr) echo " ${vcstatus}${fg[blue]}(${vcrev})${fx[reset]}"               ;;        
        *)   echo ""                                                            ;;
    esac
}


abdo_prompt_annex() {
    local conflict=""
    local missing="${_cb}${fg[yellow]}${_ce}$(abdo_annex_missing "$PWD")${_cb}${fx[reset]}${_ce}"
    if [ "$(abdo_annex_conflicts "$PWD")" ]; then
        conflict=" ${_cb}${fg[red]}${_ce}X${_cb}${fx[reset]}${_ce}"
    fi
    echo -en " ($missing$conflict)"
}


# TODO: complete for ther vcs
abdo_prompt_vcs() {
    local vcs
    vcs=$(abdo_get_vcs "$PWD")
    case $vcs in
        git)
            local gitprompt="$(__git_ps1 "%s")"
            echo -en " [$gitprompt]"
            ;;

        annex)
            local gitprompt="$(__git_ps1 "%s")"
            echo -en " [$gitprompt$missing$conflict]"
            ;;
        
        hg)
            echo -en " [${_cb}${fg[red]}${_ce}hg${_cb}${fx[reset]}${_ce}]"
            ;;

        bzr)
            echo -en " [${_cb}${fg[yellow]}${_ce}bzr${_cb}${fx[reset]}${_ce}]"
            ;;

        darcs)
            echo -en " [${_cb}${fg[green]}${_ce}darcs${_cb}${fx[reset]}${_ce}]"
            ;;
    esac
}


abdo_prompt_directory() {
    local currdir user dcol

    if [[ -n "$ZSH_VERSION" ]]; then
        user=$USER
    else
        user=$(id -u -n)
    fi

    case "$PWD" in
        /home/$user) currdir="~"                ;;
        *)           currdir="$(basename $PWD)" ;;
    esac   

    case "$TERM" in
	    rxvt*|xterm*) dcol="${fx[italic]}${fg_bold[yellow]}" ;;
        *)            dcol="${fg_bold[yellow]}"              ;;
    esac

    echo " ${_cb}${dcol}${_ce}$currdir${_cb}${fx[reset]}${_ce}"

}



# ----------------------------
# Main Prompt functions
# ----------------------------

abdo_prompt_main () {
    local prompt_pre prompt_post
    prompt_pre="$(abdo_prompt_tmux)$(abdo_prompt_nix)$(abdo_prompt_userhost)"
    prompt_post="$(abdo_prompt_vcs)$(abdo_prompt_directory)$(abdo_prompt_symbol)"
    echo -n "${prompt_pre}${prompt_post} "
}


abdo_prompt_cont () {
    local symb
    symb=' ·· '
    [[ -n "$ZSH_VERSION" ]] && symb=' %_'    
    echo -en "${_cb}${fg_bold[yellow]}${_ce}$symb${_cb}${fx[reset]}${_ce}> "
}


abdo_prompt_messagehello() {
#    echo -e "${fg_bold[blue]}Arch Linux.${fx[reset]}"

    local creationdate
    if [[ -n "$TMUX" ]]; then
	    creationdate=$(tmux list-sessions | grep ssh | sed 's/^[0-9a-zA-Z :]*(created\s*\([^\[]*\)).*$/\1/g')
	    if [[ -n "$creationdate" ]]; then
	        echo -e "${fg_bold[yellow]}Tmux${fx[reset]} session created on $creationdate."
	        echo ""
	    fi
    else
        echo -e "${fg_bold[white]}Arch${fx[reset]} ${fg_light[blue]}Linux${fx[reset]}"
        echo -e ""
    fi
    
}


abdo_save_return_value() {
    ANS=$?;
}


# In case of dumb terminal, like tramp login
if [ "$TERM" = "dumb" ]; then
    unsetopt zle prompt_cr prompt_subst
    PS1='$ '
    return
fi


if [ "$ZSH_VERSION" ]; then
    # arch prompt, just in case
    PROMPT='[%n@%m %1~] $ '

    if [ ! "$TERM" = "dumb" ]; then
        autoload -U add-zsh-hook
        
        # catch the return value before setting any prompt.
        add-zsh-hook precmd abdo_save_return_value

        # set prompt
        setopt prompt_subst
        PROMPT='$(abdo_prompt_main)'
        PROMPT2='$(abdo_prompt_cont)'
    fi
fi


if [ "$BASH_VERSION" ]; then
    # arch prompt, just in case
    PS1='[\u@\h \W]\$ '

    if [ ! "$TERM" = "dumb" ]; then
        # prompt hook
        PROMPT_COMMAND=abdo_save_return_value
    
        # set prompt
        PS1="\$(abdo_prompt_main)"
        PS2="\$(abdo_prompt_cont)"
    fi
fi

