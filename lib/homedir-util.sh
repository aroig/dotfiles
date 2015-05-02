
# ------------------------------------------------------------------ #
# homedir remote querying
# ------------------------------------------------------------------ #

##
# usage: homedir_host <remote>
#
# Get host for a homedir remote
##
homedir_host() {
    local rmt="$1"
    local host
    case "$rmt" in
        localhost)    host="localhost"                     ;;
        ada)          host="localhost"                     ;;
        babel)        host="babel.abdoroig.net"            ;;
        galois)       host="galois"                        ;;
        grothendieck) host="grothendieck"                  ;;
        quasar)       host="localhost"                     ;;
        skynet)       host="skynet"                        ;;
        quark)        host="quark"                         ;;
        *)
            error "Unrecognized remote '$rmt'"             ;;
    esac
    echo "$host"
}


##
# usage: homedir_remote_from_mrconfig <path>
#
# Get a remote name that containes <path> on the current machine
##
homedir_remote_from_mrconfig() {
    local path="$(dirname "$1")"
    local host="$(hostname)"

    case "$path" in
        /home/abdo*)           echo "$host"                   ;;
        /media/ada/home/abdo*) echo "ada"                     ;;
        /media/quasar/abdo*)   echo "quasar"                  ;;
        *)
            error "Can't find a remote associated to '$path'" ;;
    esac
}


##
# usage: homedir_remote_url <type> <host> <repodir>
#
# Get url for a given remote.
##
homedir_remote_url() {
    local type="$1"
    local host="$2"
    local repodir="$3"

    if [ "$host" = 'localhost' ]; then
        printf "$repodir"
    elif [ "$type" = 'uni' ]; then
        printf "ssh://$host/$repodir"
    else
        printf "$host:$repodir"
    fi
}



# ------------------------------------------------------------------ #
# command execution
# ------------------------------------------------------------------ #

##
# usage: homedir_remote_run <path> <remote> <cmd>
#
# Run on a command on the given directory in a remote.
##
homedir_remote_run() {
    local repo="$1"
    local remote="$2"
    local cmd="$3"

    # TODO: get host and path from the repo
    local host="$(homedir_host "$remote")"
    local path="$HOME"

    case "$host" in
        localhost)
            bash -c "cd '$path'; $cmd"
            ;;
        
        *)
            ssh "$host" "cd '$path'; $cmd"
            ;;
    esac
}



# ------------------------------------------------------------------ #
# homedir state changes
# ------------------------------------------------------------------ #

##
# usage: homedir_perms <path>
#
# Change perms for <path>
##
homedir_perms() {
    local path="$(realpath "$MR_REPO")"
    local mod="$1"
    local cur="$(stat -c '%a' "$path")"
    if [ ! "$cur" = "$mod" ]; then
        info "setting perms to $mod for '$path'"
        chmod "$mod" "$path"
    fi
}


##
# usage: homedir_git_init
#
# Initialize homedir directory in $MR_REPO
##
homedir_git_init() {
    local path="$MR_REPO"      
    git_init "$path"
    git_config "$path" "gc.auto" "0"
}


##
# usage: homedir_annex_init
#
# Initialize homedir directory in $MR_REPO
##
homedir_annex_init() {
    local path="$MR_REPO"
    local gitdir="$(git_gitdir_path "$path")"
    local githooks='.githooks'
    
    git_annex_init "$path"

    # create a hooks dir versioned in the repo
    if [ ! -d "$path/$githooks" ]; then
        mkdir -p "$path/$githooks"
    fi

    # symlink to '.git/hooks'
    if [ -e "$gitdir" ] && [ ! -L "$gitdir/hooks" ]; then
        info "symkinking '.git/hooks' -> '$githooks'"
        rm -Rf "$gitdir/hooks"
        symlink_relative "$path/$githooks" "$gitdir/hooks"
    fi
}


##
# usage: homedir_relocate_directory <src> <tgt>
#
# Attempts to move directory at <src> to <tgt> and symlink
##
homedir_relocate_directory() {
    local src="$1"
    local tgt="$2"

    # if not already symlinked
    if [ -d "$src" ] && [ ! -L "$src" ]; then
        # move it to the right place
        if [ ! -e "$tgt" ]; then
            info "moving directory to '$tgt'"
            mv "$src" "$tgt"
            # change dir to new location
            cd "$tgt"
        else
            error "Can't move directory to '$src', file already exists on target"
        fi
        # add symlink at home
        if [ ! -L "$src" ]; then
            info "adding a symlink '$src' -> '$tgt'"
            symlink_relative "$tgt" "$src"
        fi
    fi
}


##
# usage: homedir_remote
#
# Configure remote for homedir directory in $MR_REPO
##
homedir_remote() {
    local path="$MR_REPO"
    local type="$1"
    local name="$2"
    local uuid="$3"
    local repodir="$4"

    # get the host for the remote
    local host="$(homedir_host "$name")"

    # get remote for which we are running
    local lrmt="$(homedir_remote_from_mrconfig "$MR_CONFIG")"

    # compute the url for git remotes
    local url="$(homedir_remote_url "$type" "$host" "$repodir")"

    # detect detect whether the remote refers to the current repo.
    if [ "$name" = "$lrmt" ]; then type='self'; fi
    
    # configure the remote        
    case "$type" in
        self)
            if git_annex_is_repo "$path"; then
                git_config_safe "$path" "annex.uuid" "$uuid"
            fi
                
            # if path and repodir do not match, move directory repodir, and symlink from /home
            if [ ! "$path" = "$repodir" ]; then
                homedir_relocate_directory "$path" "$repodir"
            fi
            ;;
        
        dir)
            ;;

        uni)
            unison_config "$path" "remote_$name" "$url"
            unison_config_safe "$path" "remote_$name_uuid" "$uuid"            
            if [ ! "$host" = "localhost" ]; then
                unison_config "$path" "remote_$name_command" "systemctl --user start sshmux@$host.service"
            fi
            ;;
        
        git)
            git_add_remote "$path" "$name" "$url"
            git_config_safe "$path" "remote.$name.annex-uuid" "$uuid"
            if [ ! "$host" = "localhost" ]; then
                git_config "$path" "remote.$name.annex-start-command" "systemctl --user start sshmux@$host.service"
            fi
            ;;

        s3)
            git_add_S3_remote "$path" "$name" "$host" "$repodir"
            git_config_safe "$path" "remote.$name.annex-uuid" "$uuid"
            ;;

        *)
            error "Unknown remote typ: '$type'"
            ;;        
    esac
}


##
# usage: homedir_checkout <remote> [,<remote>...]
#
# Checkout on the given remotes only
##
homedir_checkout() {
    local local_remote="$(homedir_remote_from_mrconfig "$MR_CONFIG")"
    local repo="$MR_REPO"

    for rmt in "$@"; do
        if [ "$rmt" = "$local_remote" ]; then
            mkdir -p "$repo"
            (
                cd "$repo"
                mr init
            )
            return
        fi
    done
}
