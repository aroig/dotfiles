#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     vcs.sh             Vcs information                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

abdo_get_vcs() {
    local path pathold
    path="$1"
    while [[ -n "$path" ]]; do
        [[ -e "$path/.git"   ]] && echo 'git'   && return
        [[ -e "$path/.hg"    ]] && echo 'hg'    && return
        [[ -e "$path/.bzr"   ]] && echo 'bzr'   && return
        [[ -e "$path/.darcs" ]] && echo 'darcs' && return

        pathold="$path"
        path="${path%/*}"
        [[ "$path" == "$pathold" ]] && return
    done
}
