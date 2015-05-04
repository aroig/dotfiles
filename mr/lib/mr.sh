#!/usr/bin/sh


# UI utils
# ------------------------------------------------------------------ #

error() {
    if [ "$ANSI_COLORS_DISABLED" = "1" ]; then
        echo "mr error: $@" >&2
    else
        echo -e "\e[01;37mmr \e[01;31merror\e[01;37m:\e[0m $@" >&2
    fi
	exit 1
}

warning() {
    if [ "$ANSI_COLORS_DISABLED" = "1" ]; then
        echo "mr warning: $@" >&2
    else
        echo -e "\e[01;37mmr \e[01;33mwarning\e[01;37m:\e[0m $@" >&2
    fi
}

info() {
    echo -e "mr: $@" >&1
}



# Miscelania
# ------------------------------------------------------------------ #

##
# is_subdir <path1> <path2>
# Check whether first argument is a subdir of the second
##
is_subdir() {
    test "${1##$2}" != "${1}"
}


is_same_path() {
    local pathA="$1"
    if [ -e "$pathA" ]; then
        pathA="$(realpath "$pathA" 2> /dev/null)"
    fi

    local pathB="$2"
    if [ -e "$pathB" ]; then
        pathA="$(realpath "$pathB" 2> /dev/null)"
    fi
    
    test "$pathA" = "$pathB"
}

load_config() {
    local conf="$1"
    cat "$HOME/.mr/etc/$conf.conf"    
}

load_lib() {
    local lib="$1"
    source "$HOME/.mr/lib/$lib.sh"
}


##
# usage: remote_run <host> <path> <cmd>
#
# Run on a command on the given path on the given host.
##
remote_run() {
    local host="$1"
    local path="$2"
    local cmd="$3"

    case "$host" in
        localhost)
            sh -c "cd '$path'; $cmd"
            ;;
        
        *)
            ssh "$host" "cd '$path'; $cmd"
            ;;
    esac
}



# Filesystem
# ------------------------------------------------------------------ #

symlink_relative() {
    local src="$1"
    local lnk="$2"
    
    if [ -L "$lnk" ]; then
        rm -Rf "$lnk"
        
    elif [ -e "$lnk" ]; then
        error "Can't remove '$lnk' to place a symlink"
    fi

    local parent="$(dirname "$lnk")"
    local rel="$(realpath --relative-to="$parent" "$src" 2> /dev/null)"    
    ln -s "$rel" "$lnk"
}

expire_old() {
    local path="$1"
    local time="$2"

    if [ -z "$path" ]; then
        echo "Path is empty"
        exit 1
    fi

    find "$path" -mindepth 1 -maxdepth 1 -ctime "+$time" -print -exec rm -Rf {} +   
}
