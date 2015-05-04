
# ------------------------------------------------------------------ #
# repo state querying
# ------------------------------------------------------------------ #

##
# usage: unison_root_path <path>
#
# Return root of the repository
##
unison_root_path() {
    local path="$1"

    local p="$path"
    local pold=""
    while [ ! "$p = $pold" ]; do
        if [ -d "$p/.unison" ]; then
            echo "$p/.unison"
            return
        fi
        pold="$p"
        p="${p%/*}"
    done
}


##
# usage: unison_is_root <path>
#
# Check whether <path> is the root of an unison replica
##
unison_is_root() {
    local path="$1"
    test -d "$path/.unison"
}


##
# usage: unison_is_repo <path>
#
# Check whether <path> belongs to a unison replica
##
unison_is_repo() {
    local path="$1"
    local root="$(unison_root_path "$path")"
    test -n "$root" && unison_is_root "$root"
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
# remote querying
# ------------------------------------------------------------------ #

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
# usage: unison_remote_url <path> <remote>
#
# Return a remote url
##
unison_remote_url() {
    local path="$1"
    local remote="$2"
    unison_get_config "$path" "remote_$remote"
}


##
# usage: unison_remote_host <path> <remote>
#
# Return the host for the given remote
##
unison_remote_host() {
    local path="$1"
    local remote="$2"
    local url="$(unison_remote_url "$path" "$remote")"

    if [[ "$url" =~ ^ssh:// ]]; then
        url="${url#ssh://}"
        echo "${url%%/*}"
    fi    
}


##
# usage: unison_remote_path <path> <remote>
#
# Return the path for the given remote
##
unison_remote_path() {
    local path="$1"
    local remote="$2"
    local url="$(unison_remote_url "$path" "$remote")"

    if [[ "$url" =~ ^/ ]]; then
        echo "$url"
    elif [[ "$url" =~ ^ssh:// ]]; then
        url="${url#ssh://}"
        echo "${url#*/}"
    fi
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
    if ! unison_is_root "$path"; then
        warning "Directiory is not an unison replica. Skipping: $path"
        return 0
    fi

    # skip silently if local replica is not configured
    [ "$(unison_get_config "$path" "uuid")" ] || return 0

    if [[ "$action" =~ list ]] && [ "$remote" ]; then         
        # skip silently if remote is not configured
        unison_has_remote "$path" "$remote" || return 0
       
    elif [[ "$action" =~ sync|push|pull|fetch|update ]] && [ "$remote" ]; then
        # skip silently if remote is not configured
        unison_has_remote "$path" "$remote" || return 0

        # test if remote repo exists
        local host="$(unison_remote_host "$path" "$remote")"
        local path="$(unison_remote_path "$path" "$remote")"
        if [ "$host" ] && [ "$path" ]; then
            # open a ssh connection
            if [ ! "$host" = 'localhost' ]; then
                systemctl --user start "sshmux@$host.service"
            fi

            # test if remote repo is unison replica
            if ! remote_run "$host" "/" "test -d '$path/.unison'"; then
                warning "There is no unison replica on remote '$remote'. Skipping: $path"
                return 0
            fi
        fi
    fi
    return 1
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
        echo "$key=\"$val\"" >> "$cfgpath"   
    fi
}



# ------------------------------------------------------------------ #
# state changes
# ------------------------------------------------------------------ #

##
# usage: unison_init <path>
#
# Initialize an empty unison replica
##
unison_init() {
    local path="$1"
    (
        cd "$path"
        if ! unison_is_repo; then
            mkdir -p "$path/.unison"
        fi
    )
}


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

    # on non-tty make terse output
    if [ ! -t 1 ]; then
        args+=('-terse')
    fi
    
    # local sshargs=-sshargs -o ServerAliveInterval=60 -o LogLevel=quiet -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no

    # echo "rootA: $rootA"
    # echo "rootB: $rootB"
    unison -root "$rootA" -root "$rootB" "${args[@]}" -batch default.prf
}


