
##
# usage: unison_is_repo <path>
#
# Check whether <path> is the root of an unison replica
##
unison_is_root() {
    local path="$1"
    test -e "$path/.unison"
}


##
# usage: unison_has_remote <path> <name>
#
# Check whether unison has this remote
##
unison_has_remote() {
    local path="$1"
    local rmt="$2"
    [ "$(unison_get_config "$path" "remote_$rmt")" ]
}


##
# unison_get_config <path> <key>
##
unison_get_config() {
    local path="$1"
    local cfgpath="$path/.unison/config"    
    local key="$2"

    if [ ! -f "$cfgpath" ]; then
        return
    fi
    
    (
        source "$cfgpath"
        eval echo "\$$key"
    )    
}


##
# unison_config <path> <key> <value>
##
unison_config() {
    local path="$1"
    local cfgpath="$path/.unison/config"
    local key="$2"
    local value="$3"
    local cur="$(unison_get_config "$path" "$key")"

    # if key already exists, and is different
    if [ "$cur" ] && [ ! "$value" = "$cur" ]; then
        sed -i "/^$key=.*$/d" "$cfgpath"
    fi
    
    if [ ! "$value" = "$cur" ]; then
        info "setting config: $key = $value"
        mkdir -p "$path/.unison"
        echo "$key=\"$value\"" >> "$cfgpath"   
    fi
}


##
# usage: unison_skip <path> <action> [<remote>]
#
# Check whether git repo has this remote
##
unison_skip() {
    local path="$1"
    local action="$2"
    local remote="$3"
    
    # skip if repo is not a directory
    [ ! -d "$MR_REPO" ] && return 0

    # skip if remote is not configured
    if [[ "$action" =~ sync|push|pull|fetch|update|list ]]; then
        [ "$remote" ] && ! unison_has_remote "$MR_REPO" "$remote" && return 0
    fi
    return 1
}


##
# unison_sync <path> <remote> <direction>
##
unison_sync() {
    local cmd="$1"
    local path="$2"
    local remote="$3"
    
    local rootA="$(realpath "$MR_REPO")"
    local rootB="$(unison_get_config "$path" "remote_$remote")"
    local mountpoint="$(unison_get_config "$path" "mountpoint")"
    local ignore="$(unison_get_config "$path" "ignore")"

    # check remote argument is passed
    if [ -z "$remote" ]; then
        error "Need a remote"
    fi

    # check that path is a directory
    if [ ! -d "$path" ]; then
        error "Path is not a directory: '$path'"
    fi
    
    # check remote is known
    if [ -z "$rootB" ]; then
        error "Unknown remote '$remote'"
    fi

    local args=('-ignore' "Path .unison" '-ignore' "Path .async.last" '-logfile' "/dev/null")
    
    # check for mountpoint argument
    if [ "$mountpoint" ]; then
        args+=('-mountpoint' "$mountpoint")
    fi

    # check ignores
    if [ "$ignore" ]; then
        args+=('-ignore' "Regex $ignore")
    fi
    
    # check for force argument
    local force
    if [ "$cmd" = "push" ]; then
        args+=('-force' "$rootA")

    elif [ "$cmd" = "pull" ]; then
        args+=('-force' "$rootB")

    elif [ "$cmd" = "sync" ]; then
        :
        
    else
        error "Unknown sync cmd '$cmd'"
    fi
    
    # local sshargs=-sshargs -o ServerAliveInterval=60 -o LogLevel=quiet -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no

    # echo "rootA: $rootA"
    # echo "rootB: $rootB"
    unison -root "$rootA" -root "$rootB" "${args[@]}" -batch default.prf
}


