
# ------------------------------------------------------------------ #
# homedir remote querying
# ------------------------------------------------------------------ #

##
# usage: homedir_is_root <path>
#
# Check whether path is at the root of a home directory tree
##
homedir_is_root() {
    local path="$1"
    local mrconfig="$(dirname "$path")/.mrconfig"
    test -f "$mrconfig" || return 1
    
    local lrmt="$(homedir_remote_from_path "$path")"
    test -n "$lrmt" || return 1

    return 0
}


##
# usage: homedir_host <remote>
#
# Get host for a homedir remote
##
homedir_host() {
    local rmt="$1"
    
    case "$rmt" in
        localhost)    echo "localhost"                     ;;
        ada)          echo "localhost"                     ;;
        babel)        echo "babel.abdoroig.net"            ;;
        galois)       echo "galois"                        ;;
        grothendieck) echo "grothendieck"                  ;;
        quasar)       echo "localhost"                     ;;
        skynet)       echo "skynet"                        ;;
        quark)        echo "quark"                         ;;
    esac
}


##
# usage: homedir_remote_from_path <path>
#
# Get a remote name for which <path> is a directory.
##
homedir_remote_from_path() {
    local path="$(dirname "$1")"
    local host="$(hostname)"

    case "$path" in
        /home/abdo)           echo "$host"                   ;;
        /data/abdo)           echo "$host"                   ;;
        /media/ada/home/abdo) echo "ada"                     ;;
        /media/quasar/abdo)   echo "quasar"                  ;;
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
# usage: homedir_init <type>
#
# Initialize homedir directory in $MR_REPO
##
homedir_init() {
    local type="$1"
    local path="$MR_REPO"
    case "$type" in
        dir)
            ;;
        
        uni)
            unison_init "$path"
            ;;
        
        git)            
            git_init "$path"
            git_config "$path" "gc.auto" "0"
            ;;

        annex)
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
            ;;

        *)
            error "Unknown repo type '$type'"
            ;;
    esac
}


##
# usage: homedir_ecryptfs_init
#
# Initialize encrypted private directory in $MR_REPO
#
# NOTE: useful stuff
#     keyctl list @u
#     keyctl clear @u
##
homedir_ecryptfs_init() {
    local keylength="64"
    local path="$MR_REPO"
    local privdir="$(basename "$MR_REPO")"
    local backend="$(dirname "$MR_REPO")/.$privdir"

    local ecryptfs_base="$(dirname "$MR_REPO")/.ecryptfs"
    local ecryptfs_pass="$ecryptfs_base/$privdir-passphrase"
    local ecryptfs_sig="$ecryptfs_base/$privdir.sig"    
    local ecryptfs_conf="$ecryptfs_base/$privdir.conf"


    # skip ecryptfs setup if $path is not at homedir
    if [ ! "$path" = "$HOME/$privdir" ]; then
        warning 'Not in $HOME. Skipping ecryptfs setup.'
        return
    fi
    
    # create ecryptfs directory
    if [ ! -e "$ecryptfs_base" ]; then
        mkdir -p -m 700 "$ecryptfs_base"
    fi
    
    # generate random passphrase and encrypt it with my key
    if [ ! -f "$ecryptfs_pass" ]; then
        echo "ecryptfs: generating a random passphrase"
        cat /dev/urandom | tr -dc '0-9a-f' | head -c "$keylength" | xargs printf "%s\n" | \
            gpg --batch -e > "$ecryptfs_pass"
        if [ ! "$?" == "0" ]; then
            error "gpg could not encrypt the passphrase"         
        fi
    fi 

    # store passphrase signature
    if [ ! -f "$ecryptfs_sig" ]; then
        echo "ecryptfs: saving passphrase signature"
        gpg --batch -d "$ecryptfs_pass" |                    \
            ecryptfs-add-passphrase --fnek | grep "\[.*\]" | \
            sed "s/^.*\[\(.*\)\].*$/\1/" > "$ecryptfs_sig"       
    fi

    # prepare ecryptfs config
    if [ ! -f "$ecryptfs_conf" ]; then
        echo "ecryptfs: preparing ecryptfs config"
        echo "$backend $path ecryptfs" > "$ecryptfs_conf"
    fi

    if ! mountpoint -q "$path"; then
        error "Please, mount '$privdir' now and retry."
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
# usage: homedir_remote <path> <type> <name> <uuid> <repodir>
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
    if [ -z "$host" ]; then
        error "Can't determine host"
    fi
    
    # get remote for which we are running
    local lrmt="$(homedir_remote_from_path "$path")"
    if [ -z "$lrmt" ]; then
        error "Can't determine remote corresponding to current path"
    fi

    # compute the url for git remotes
    local url="$(homedir_remote_url "$type" "$host" "$repodir")"

    # detect detect whether the remote refers to the current repo.
    if [ "$name" = "$lrmt" ]; then
        homedir_local "$type" "$name" "$uuid" "$repodir"
        return
    fi
    
    # configure the remote        
    case "$type" in        
        dir)
            ;;

        uni)
            unison_config "$path" "remote_$name" "$url"
            # unison_config_safe "$path" "remote_${name}_uuid" "$uuid"            
            # if [ ! "$host" = "localhost" ]; then
            #     unison_config "$path" "remote_${name}_command" "systemctl --user start sshmux@$host.service"
            # fi
            ;;
        
        git)
            git_add_remote "$path" "$name" "$url"
            git_config_safe "$path" "remote.${name}.annex-uuid" "$uuid"
            if [ ! "$host" = "localhost" ]; then
                git_config "$path" "remote.${name}.annex-start-command" "systemctl --user start sshmux@$host.service"
            fi
            ;;

        s3)
            git_annex_add_S3_remote "$path" "$name" "$host" "$repodir"
            git_config_safe "$path" "remote.$name.annex-uuid" "$uuid"
            ;;

        *)
            error "Unknown remote type: '$type'"
            ;;        
    esac
}


##
# usage: homedir_local <path> <type> <name> <uuid> <repodir>
#
# Configure local homedir repository
##
homedir_local() {
    local path="$MR_REPO"
    local type="$1"
    local name="$2"
    local uuid="$3"
    local repodir="$4"
   
    case "$type" in
        dir)
            ;;
        
        git)
            git_config_safe "$path" "annex.uuid" "$uuid"
            ;;

        uni)
            unison_config_safe "$path" "uuid" "$uuid"
            ;;

        *)
            error "Unknown local remote type: '$type'"
            ;;
    esac

    # if path and repodir do not match, move directory repodir, and symlink from /home
    if [ ! "$path" = "$repodir" ]; then
        homedir_relocate_directory "$path" "$repodir"
    fi    
}
  

##
# usage: homedir_checkout <remote> [,<remote>...]
#
# Checkout on the given remotes only
##
homedir_checkout() {
    # NOTE: during checkout $MR_REPO is absolute path
    local repo="$MR_REPO"
    local lrmt="$(homedir_remote_from_path "$repo")"
    if [ -z "$lrmt" ]; then
        error "Can't determine remote corresponding to current path: '$repo'"
    fi

    for rmt in "$@"; do
        if [ "$rmt" = "$lrmt" ]; then
            mkdir -p "$repo"
            (
                cd "$repo"
                mr init
            )
            return
        fi
    done
}
