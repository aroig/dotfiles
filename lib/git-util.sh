#!/usr/bin/sh

# ------------------------------------------------------------------ #
# git repo paths
# ------------------------------------------------------------------ #

##
# usage: git_root_path <path>
#
# Return root of the repository
##
git_root_path() {
    local path="$1"
    (
        cd "$path"
        local gitroot="$(git rev-parse --show-toplevel 2> /dev/null)"
        realpath "$gitroot" 2> /dev/null
    )
}


##
# usage: git_gitdir_path <path>
#
# Return root of the repository
##
git_gitdir_path() {
    local path="$1"
    (
        cd "$path"
        local gitdir="$(git rev-parse --git-dir 2> /dev/null)"
        realpath "$gitdir" 2> /dev/null
    )
}


##
# usage: git_is_root <path>
#
# Check whether <path> is the root of a git repo
##
git_is_root() {
    local path="$1"
    test -e "$path/.git"
}


##
# usage: git_is_repo <path>
#
# Check whether <path> belongs to a git repo
##
git_is_repo() {
    local path="$1"
    local gitdir="$(git_gitdir_path "$path")"
    test -n "$gitdir" && test -e "$gitdir"
}


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



# ------------------------------------------------------------------ #
# state querying
# ------------------------------------------------------------------ #

##
# usage: git_remotes <path>
#
# List remotes configured in repo
##
git_remotes() {
    local path="$1"
    (
        cd "$path"
        git remote show
    )
}


##
# usage: git_has_remote <path> <name>
#
# Check whether git repo has this remote
##
git_has_remote() {
    local path="$1"
    local rmt="$2"
    (
        cd "$path"
        git remote show | grep -q "$rmt" 2> /dev/null
    )
}


##
# usage: git_tracking_branch <path> [<branch>]
#
# Get the remote tracking branch  of <branch> or current branch
##
git_tracking_branch() {
    local path="$1"
    local branch="$2"
    (
        cd "$path"
        if [ "$branch" ]; then
            git rev-parse --abbrev-ref --symbolic-full-name "$1{u}" 2> /dev/null
        else
            git rev-parse --abbrev-ref --symbolic-full-name "@{u}" 2> /dev/null
        fi
    )
}


##
# usage: git_get_config <path> <key>
#
# Check whether git repo has this remote
##
git_get_config() {
    local path="$1"
    local key="$2"
    (
        cd "$path"
        git config "$key" 2> /dev/null        
    )
}


##
# git_skip_missing_remote <repo> <remote>
# skips execution if remote is missing
##
git_skip_missing_remote() {
    local remote="$1"
    if [ "$remote" ] && ! git_has_remote "$MR_REPO" "$remote"; then
        skip "Remote '$remote' not configured."
    fi
}



# ------------------------------------------------------------------ #
# configuration
# ------------------------------------------------------------------ #

##
# usage: git_config <path> <key> <value>
#
# Set a git config parameter
##
git_config() {
    local path="$1"
    local key="$2"
    local val="$3"
    (
        cd "$path"
        local curval="$(git config "$key" 2> /dev/null)"

        # warn if changing a current value
        if [ "$curval" ] && [ "$curval" != "$val" ]; then
            warning "changing git config for '$key': '$curval' -> '$val'"
        fi

        #  change value if needs to be changed
        if [ "$curval" != "$val" ]; then
            info "setting git config: $key = $val"
            git config "$key" "$val"
        fi
    )
}


##
# usage: git_config_safe <path> <key> <value>
#
# Set a git config parameter. Never change an existing value
##
git_config_safe() {
    local path="$1"
    local key="$2"
    local val="$3"
    (
        cd "$path"
        local curval="$(git config "$key" 2> /dev/null)"

        # warn if changing a current value
        if [ "$curval" ] && [ "$curval" != "$val" ]; then
            error "attempting to change git config for '$key': '$curval' -> '$val'"

        elif [ "$curval" != "$val" ]; then
            info "setting git config: $key = $val"
            git config "$key" "$val"
        fi
    )
}


##
# usage: git_add_remote <path> <name> <url>
#
# Add a git remote. Noop if the remote is named as location
##
git_add_remote() {
    local path="$1"
    local name="$2"
    local url="$3"
            
    if [ ! "$(git_get_config "$path" "remote.$name.url")" ]; then
        info "adding git remote '$name': $url"
        git remote add "$name" "$url"
    else
        git_config "$path" "remote.$name.url" "$url"
    fi    
}


##
# usage: git_add_S3_remote <path> <name> <bucket> <subdir>
#
# Add a git annex S3 remote. Noop if the remote is named as location
##
git_add_S3_remote() {
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
# usage: git_init <path>
#
# Initialize an empty git repo
##
git_init() {
    local path="$1"
    (
        cd "$path"
        if ! git_is_repo; then
            git init
        fi
    )
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


# ------------------------------------------------------------------ #
# state changes
# ------------------------------------------------------------------ #

##                     
# commit_if_changes <path> <message>
# Commits changes to a repo if there are any, otherwise do nothing.
##
git_commit_if_changes() {
    local srcpath="$(readlink -f "$1")"
    local message="$2"
    (
        cd "$srcpath"
        local work_tree="$(git rev-parse --show-toplevel 2>/dev/null)"
        local git_dir="$(git rev-parse --git-dir 2>/dev/null)"
        
        local num="$(git status --porcelain . | wc -l)"
        if [[ $num -ge 1 ]]; then
            if [ -d "$git_dir/annex" ]; then
                git annex add
            fi
            git add -A .
        fi
        local num="$(git status --porcelain . | wc -l)"
        if [[ $num -ge 1 ]]; then
            git commit -m "$message ($num files)"
        fi
    )
}
