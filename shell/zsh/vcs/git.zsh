#!/bin/zsh

function get_git_tracking_branch () {
    br=$1

    if [[ ! "$br" == "detached" ]]; then
	local remote=$(git config branch.$br.remote)
	local remote_ref=$(git config branch.$br.merge)
	if [[ -n "$remote" ]]; then
   	    local rbr=$(expr $remote_ref : 'refs/heads/\(.*\)')
	    local tbr=refs/remotes/$remote/$rbr
	    echo "$tbr"
	    return
	fi
    fi
    echo ""
}


local st="$(git status --porcelain 2> /dev/null)"

local br
br="${$(git symbolic-ref HEAD 2>/dev/null)##refs/heads/}" ||
br="detached"

local rev
rev="$(git rev-parse --short HEAD 2> /dev/null)" ||
rev="none"

__CURRENT_VCS_BRANCH="$br"
__CURRENT_VCS_REV="$rev"
# __CURRENT_VCS_TAGS="$(git describe --tags HEAD 2> /dev/null)"



if   [[ ! "$(echo -n "$st\n" | egrep '^\s*U' | wc -l)" == "0" ]]; then       __CURRENT_VCS_STATUS='conflict'
elif [[ ! "$(echo -n "$st\n" | egrep '^\s*.U' | wc -l)" == "0" ]]; then      __CURRENT_VCS_STATUS='conflict'
elif [[ ! "$(echo -n "$st\n" | egrep '^\s*AA' | wc -l)" == "0" ]]; then      __CURRENT_VCS_STATUS='conflict'
elif [[ ! "$(echo -n "$st\n" | egrep '^\s*DD' | wc -l)" == "0" ]]; then      __CURRENT_VCS_STATUS='conflict'      
elif [[ ! "$(echo -n "$st\n" | egrep '^\?' | wc -l)" == "0" ]]; then         __CURRENT_VCS_STATUS='untracked'
elif [[ ! "$(echo -n "$st\n" | egrep '^[MARC ]D' | wc -l)" == "0" ]]; then   __CURRENT_VCS_STATUS='deleted'
elif [[ ! "$(echo -n "$st\n" | egrep '^[MADRC ]M' | wc -l)" == "0" ]]; then  __CURRENT_VCS_STATUS='changed'    
elif [[ ! "$(echo -n "$st\n" | egrep '^[MADRC ]T' | wc -l)" == "0" ]]; then  __CURRENT_VCS_STATUS='changed'    
elif [[ ! "$(echo -n "$st\n" | egrep '^[MADRC].' | wc -l)" == "0" ]]; then   __CURRENT_VCS_STATUS='staged'
elif [[ ! "$(echo -n "$st\n" | egrep '^!' | wc -l)" == "0" ]]; then          __CURRENT_VCS_STATUS='ignored'    
elif [[ "$st" == "" ]]; then                                                 __CURRENT_VCS_STATUS='sync'
else                                                                         __CURRENT_VCS_STATUS='unknown'
fi


local tbr="$(get_git_tracking_branch $br)"
__CURRENT_VCS_REMOTE_STATUS="none"

if [ "$tbr" ]; then
    local commits="$(git rev-list --left-right $tbr...HEAD)"
    local headahead="$(echo $commits | grep "^>" | wc -l)"
    local headbehind="$(echo $commits | grep "^<" | wc -l)"

    if [[ "$headbehind" == "0" ]]; then
	if [[ "$headahead" == "0" ]]; then    __CURRENT_VCS_REMOTE_STATUS="sync"
	else                                  __CURRENT_VCS_REMOTE_STATUS="ahead"
	fi
    else
	if [[ "$headahead" == "0" ]]; then    __CURRENT_VCS_REMOTE_STATUS="behind"
	else                                  __CURRENT_VCS_REMOTE_STATUS="divergent"
	fi
    fi
fi
