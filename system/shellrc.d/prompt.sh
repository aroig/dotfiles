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
        host=$(hostname)
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
        riemann)      hcol="${fg_bold[yellow]}"  ;;
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
    if [ ! "$TERM" = "dumb" ]; then
        # prompt hook. need to do it this way in order to call functions
        PROMPT_COMMAND='abdo_save_return_value; PS1="$(abdo_prompt_main)"; PS2="\$(abdo_prompt_cont)"'
    fi
fi
