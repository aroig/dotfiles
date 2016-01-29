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


abdo_git_dirinfo() {
    local dir="$(realpath $1)"
    (
        cd $dir;
        local vcs=$(abdo_get_vcs "$dir")
        if [ "$vcs" ]; then
            # The -P tells print to format it as a prompt
            print -P " $(abdo_prompt_vcs "%s")" | tr -d '\n'

            # too slow
            # if [ "$vcs" = "annex" ]; then
            #    print -P " $(abdo_prompt_annex)" | tr -d '\n'
            # fi

            if [[ "$vcs" =~ "git|annex" ]]; then
                printf "  "
                git --no-pager log -n 1 --pretty="format:%C(blue)%ad%C(reset) %C(green)%an%C(reset). %s" --date="format:%Y-%m-%d %H:%M"
            fi
        fi
    ) 
}

# do a ls with additional git status details.
abdo_git_ls() {
    local list
    if [ -n "$1" ]; then
        list=$@
    else
        list="."
    fi

    local dirname
    for arg in $list; do
        if [ -d "$arg" ]; then
            find "$arg" -mindepth 1 -maxdepth 1 -type d -not -path '*/\.*' | \
            while read dir; do
                dirname=${dir#$arg/}
                printf "\e[1;34m%15s\e[0m" "$dirname"
                abdo_git_dirinfo "$dir"
                echo ""
            done
        fi
    done
}
