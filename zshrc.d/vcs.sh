

function get_git_tracking_branch () {
    br=$1

    if [[ ! "$br" == "detached" ]]; then
	local remote=$(git config branch.$br.remote)
	local remote_ref=$(git config branch.$br.merge)
	if [[ -n "$remote" ]]; then
   	    local remote_branch=$(expr $remote_ref : 'refs/heads/\(.*\)')
	    local tracking_branch=refs/remotes/$remote/$remote_branch
	    echo "$tracking_branch"
	    return
	fi
    fi
    echo ""
}

# git rev-list --left-right $(get_git_tracking_branch $br)...HEAD


function update_current_git_vars() {
    local st="$(git status --porcelain 2> /dev/null)"

    local br
    br="${$(git symbolic-ref HEAD 2>/dev/null)##refs/heads/}" ||
    br="detached"      # detached HEAD

    # Branch
    __CURRENT_VCS_BRANCH="$br"
   
    # Revision
    __CURRENT_VCS_REV="$(git rev-parse --short HEAD)"

    # Revision
    __CURRENT_VCS_TAGS="$(git describe --tags HEAD 2> /dev/null)"

    # Repo status
    if [[ ! "$(echo -n "$st" | grep '^\s*U[UAD]' | wc -l)" == "0" ]]; then
        __CURRENT_VCS_STATUS='conflict'

    elif [[ ! "$(echo -n "$st" | grep '^\s*\?\?' | wc -l)" == "0" ]]; then
        __CURRENT_VCS_STATUS='untracked'

    elif [[ ! "$(echo -n "$st" | grep '^\s*M' | wc -l)" == "0" ]]; then
        __CURRENT_VCS_STATUS='changed'
  
    elif [[ "$(echo -n "$st" | wc -l)" == "0" ]]; then
        __CURRENT_VCS_STATUS='clean'
    else
	__CURRENT_VCS_STATUS='unknown'
    fi	   

    # Status with respect to remote origin
    __CURRENT_VCS_TIMELINE=""
    local tracking_branch="$(get_git_tracking_branch $br)"
    if [[ ! x"$tracking_branch" == x ]]; then
	local commits="$(git rev-list --left-right $tracking_branch...HEAD)"
	local headahead="$(echo $commits | grep "^>" | wc -l)"
	local headbehind="$(echo $commits | grep "^<" | wc -l)"

	if [[ "$headbehind" == "0" ]]; then
	    if [[ "$headahead" == "0" ]]; then
		__CURRENT_VCS_TIMELINE="sync"
	    else
		__CURRENT_VCS_TIMELINE="ahead"
	    fi
	else
	    if [[ "$headahead" == "0" ]]; then
		__CURRENT_VCS_TIMELINE="behind"
	    else
		__CURRENT_VCS_TIMELINE="divergent"
	    fi
	fi
    fi
}



function update_current_hg_vars() {
    local st="$(hg status)"

    __CURRENT_VCS_REV="$(hg id -n)"
    __CURRENT_VCS_BRANCH="$(hg branch)"

    if [[ "$st" == "" ]]; then
	__CURRENT_VCS_STATUS="clean"
    else
	__CURRENT_VCS_STATUS="changed"
    fi
}


function detect_current_vcs() {
    local isgit="$(git rev-parse --is-inside-work-tree 2> /dev/null)"
    if [[ -n "$isgit" ]]; then
	__CURRENT_VCS_PROGRAM="git"
	return
    fi

    local ishg="$(hg status &> /dev/null; echo "$?")"
    if [[ "$ishg" == "0" ]]; then  
	__CURRENT_VCS_PROGRAM="hg"
	return
    fi
    
    __CURRENT_VCS_PROGRAM="none"
}


function refresh_current_vcs_vars() {
    unset __CURRENT_VCS_REV
    unset __CURRENT_VCS_BRANCH
    unset __CURRENT_VCS_STATUS
    unset __CURRENT_VCS_TIMELINE

    # If empty means have never detected
    if [[ "$__CURRENT_VCS_PROGRAM" == "" ]]; then
	detect_current_vcs
    fi

    case $__CURRENT_VCS_PROGRAM in
	git)
	    update_current_git_vars
            ;;

        hg)
	    update_current_hg_vars
	    ;;
    esac
}

# Detects vcs and sets all variables
function update_current_vcs_vars() {
    unset __CURRENT_VCS_PROGRAM

    detect_current_vcs
    refresh_current_vcs_vars
}


add-zsh-hook chpwd update_current_vcs_vars
add-zsh-hook precmd refresh_current_vcs_vars
