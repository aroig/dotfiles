#!/usr/bin/sh

# ------------------------------------------------------------------ #
# git repo paths
# ------------------------------------------------------------------ #

##
# usage: git_annex_is_root <path>
#
# Check whether <path> is the root of a git annex repo
##
git_annex_is_root() {
    local path="$1"
    test -d "$path/.git/annex"
}


##
# usage: git_annex_is_repo <path>
#
# Check whether <path> belongs to a git-annex repo
##
git_annex_is_repo() {
    local path="$1"
    local gitdir="$(git_gitdir_path "$path")"
    test -n "$gitdir" && test -d "$gitdir/annex"
}


##
# usage: git_annex_skip <path> <action> [<remote>]
#
# Check whether git repo has this remote
##
git_annex_skip() {
    local path="$1"
    local action="$2"
    local remote="$3"
    
    # skip if repo is not a git annex repo
    git_annex_is_root "$path" || return 0

    # skip if remote is not configured
    if [[ "$action" =~ list ]] && [ "$remote" ]; then
        git_has_remote "$path" "$remote" || return 0        
            
    elif [[ "$action" =~ sync|push|pull|fetch|update ]] && [ "$remote" ]; then
        # test if local repo has remote
        git_has_remote "$path" "$remote" || return 0

        # test if remote repo exists
        local host="$(git_remote_host "$path" "$remote")"
        local path="$(git_remote_path "$path" "$remote")"        
        if [ "$host" ] && [ "$path" ]; then
            if [ "$host" = "localhost" ]; then
                test -d "$path/.git/annex" || return 0
                
            elif [ "$host" ]; then
                systemctl --user start "sshmux@$host.service"
                ssh "$host" "test -d '$path/.git/annex'" || return 0
            fi
        fi
    fi
    return 1
}



# ------------------------------------------------------------------ #
# configuration
# ------------------------------------------------------------------ #

##
# usage: git_add_S3_remote <path> <name> <bucket> <subdir>
#
# Add a git annex S3 remote. Noop if the remote is named as location
##
git_annex_add_S3_remote() {
    local path="$1"
    local name="$2"
    local bucket="$3"
    local subdir="$4"
    if [ ! "$(git_get_config "$path" "remote.$name.annex-s3")" = "true" ]; then
        if git grep --quiet " name=$name " git-annex:remote.log 2> /dev/null; then
            info "adding git annex S3 remote '$name': $bucket/$subdir"
            git annex enableremote "$name"
        else
            warning "Unknown S3 special remote '$name'. Try running 'mr init' after 'mr sync'."
        fi
    fi
}


##
# usage: git_annex_init <path>
#
# Initialize an empty git-annex repo
##
git_annex_init() {
    local path="$1"
    (
        cd "$path"
        if ! git_is_repo; then
            error "git_annex_init expects a git repo: '$path'"
        fi

        if ! git_annex_is_repo; then
            git annex init
        fi
    )
}


##                     
# git_annex_commit_if_changed <path> <message>
# Commits changes to a repo if there are any, otherwise do nothing.
##
git_annex_commit_if_changed() {
    local srcpath="$(readlink -f "$1")"
    local message="$2"

    if git_annex_is_repo "$srcpath"; then
        (
            cd "$srcpath"
            git annex add || return 1
        )
    fi
    
    git_commit_if_changed "$srcpath" "$message"
}
