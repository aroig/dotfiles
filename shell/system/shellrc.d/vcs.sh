#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     vcs.sh             Vcs information                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

abdo_get_vcs() {
    local path pathold
    path="$(/usr/bin/realpath "$1")"
    while [[ -n "$path" ]]; do
        [[ -e "$path/.git/annex" ]] && echo 'annex' && return
        [[ -e "$path/.git"       ]] && echo 'git'   && return
        [[ -e "$path/.hg"        ]] && echo 'hg'    && return
        [[ -e "$path/.bzr"       ]] && echo 'bzr'   && return
        [[ -e "$path/.darcs"     ]] && echo 'darcs' && return

        pathold="$path"
        path="${path%/*}"
        [[ "$path" == "$pathold" ]] && return
    done
}


abdo_annex_missing() {
    find "$1" -type l -xtype l | wc -l | numfmt --to=si
}


abdo_annex_conflicts() {
    find "$1" -path './.git' -prune -or -regex '^.*\.variant-[a-zA-Z0-9]+$' -print
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

