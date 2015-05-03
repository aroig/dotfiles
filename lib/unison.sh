
# ------------------------------------------------------------------ #
# querying data
# ------------------------------------------------------------------ #

##
# usage: unison_is_repo <path>
#
# Check whether <path> is the root of an unison replica
##
unison_is_root() {
    local path="$1"
    test -d "$path/.unison"
}


##
# usage: unison_has_remote <path> <name>
#
# Check whether unison has this remote
##
unison_has_remote() {
    local path="$1"
    local rmt="$2"
    test -n "$(unison_get_config "$path" "remote_$rmt")"
}


##
# usage: unison_skip <path> <action> [<remote>]
#
# Check whether this repo should be skipped 
##
unison_skip() {
    local path="$1"
    local action="$2"
    local remote="$3"
    
    # skip if repo is not a unison replica
    ! unison_is_root "$path" && return 0

    # skip if remote is not configured
    if [[ "$action" =~ sync|push|pull|fetch|update|list ]]; then
        [ "$remote" ] && ! unison_has_remote "$path" "$remote" && return 0
    fi
    return 1
}


##
# usage: unison_get_config <path> <key>
#
# Get configuration key for repo at <path>
##
unison_get_config() {
    local path="$1"
    local cfgpath="$path/.unison/config"    
    local key="$2"

    if [ ! -f "$cfgpath" ]; then
        return
    fi

    env -i sh -c "source '$cfgpath'; eval echo '\$$key'" 
}



# ------------------------------------------------------------------ #
# configuration
# ------------------------------------------------------------------ #

##
# usage: unison_config <path> <key> <value>
#
# Set configuration of repo at <path>
##
unison_config() {
    local path="$1"
    local cfgpath="$path/.unison/config"
    local key="$2"
    local val="$3"
    local curval="$(unison_get_config "$path" "$key")"
    
    # warn if changing current value
    if [ "$curval" ] && [ ! "$val" = "$curval" ]; then
        warning "changing config for '$key': '$curval' -> '$val'"
        sed -i "/^$key=.*$/d" "$cfgpath"
    fi
    
    if [ ! "$val" = "$curval" ]; then
        info "setting config: $key = $val"
        mkdir -p "$path/.unison"
        echo "$key=\"$val\"" >> "$cfgpath"   
    fi
}


##
# usage: unison_config <path> <key> <value>
#
# Set configuration of repo at <path>. Refuse to change an already existing value
##
unison_config_safe() {
    local path="$1"
    local cfgpath="$path/.unison/config"
    local key="$2"
    local val="$3"
    local curval="$(unison_get_config "$path" "$key")"
    
    # error out if attempting to change current value
    if [ "$curval" ] && [ ! "$val" = "$curval" ]; then
        error "attempting to change config for '$key': '$curval' -> '$val'"

    elif [ ! "$val" = "$curval" ]; then
        info "setting config: $key = $val"
        mkdir -p "$path/.unison"
        echo "$key=\"$val\"" >> "$cfgpath"   
    fi
}



# ------------------------------------------------------------------ #
# state changes
# ------------------------------------------------------------------ #

##
# usage: unison_sync [ sync | push | pull ] <path> <remote>
#
# Perform a sync to a remote
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

    local args=('-ignore' "Path .unison" '-logfile' "/dev/null")
    
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


