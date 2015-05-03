#!/usr/bin/sh

# ------------------------------------------------------------------ #
# Globals
# ------------------------------------------------------------------ #

arch_is_package() {
    local path="$1"
    test -f "$path/PKGBUILD" && is_subdir "$path" "$AB2_ARCH_DIR"
}



# ------------------------------------------------------------------ #
# makepkg
# ------------------------------------------------------------------ #

get_makepkg_config() {
    key="$1"
    (
        for f in "/etc/makepkg.conf" "$HOME/.makepkg.conf"; do
            if [ -f "$f" ]; then source "$f"; fi
        done
        eval "echo \"\$$key\""
    )
}



# ------------------------------------------------------------------ #
# PKGBUILD stuff
# ------------------------------------------------------------------ #

##
# pkgbuild_parse <file> <action>
# Extract information from PKGBUILD files
##
pkgbuild_parse() {
    local pkgbuild="$1"
    local action="$2"
    local parser="$HOME/.mr/bin/pkgbuild-parse.sh"
    if [ ! -f "$parser" ]; then
        error "Can't find script '$parser'"
    fi
    bash "$parser" "$pkgbuild" "$action"
}

##
# pkgbuild_packages <path>
# Return list of packages a PKGBUILD will produce. May need
# access to sources in order to compute the version
##
pkgbuild_packages() {
    local srcpath="$(readlink -f "$1")"
    pkgbuild_parse "$srcpath/PKGBUILD" pkgfiles
}

##
# pkgbuild_sources <path>
# List sources from the PKGBUILD
##
pkgbuild_sources() {
    local srcpath="$(readlink -f "$1")"
    pkgbuild_parse "$srcpath/PKGBUILD" source
}

##
# pkgbuild_dynamic_version <path>
# Check whether <path>/PKGBUILD generates version dynamically
##
pkgbuild_has_dynamic_version() {
    local srcpath="$1"
    [ "$(pkgbuild_parse "$srcpath/PKGBUILD" "dynamicver")" = "yes" ]
}

##
# pkgbuild_bump <path>
# Bump the pkgrel
##
pkgbuild_bump_pkgrel() {
    local srcpath="$(readlink -f "$1")"
    local pkgbuild="$srcpath/PKGBUILD"            
    if [ -f "$pkgbuild" ]; then
        local pkgrel="$(pkgbuild_parse "$pkgbuild" pkgrel)"
        local pkgrel=`expr $pkgrel + 1`
        echo "Bumping pkgrel: $pkgrel"
        sed -i "s/pkgrel=.*$/pkgrel=$pkgrel/" "$pkgbuild"
        git_commit_if_changed "$srcpath" "Bump pkgrel to '$pkgrel'"            
    fi
}

##
# pkgbuild_bump_version <path> <ver>
# Update the version in a PKGBUILD and reset pkgrel if version increases.
##
pkgbuild_bump_version() {
    local srcpath="$(readlink -f "$1")"
    local newver="$2"
    local pkgbuild="$srcpath/PKGBUILD"            
    if [ -f "$pkgbuild" ]; then
        local pkgver="$(pkgbuild_parse "$pkgbuild" pkgver)"
        if [ ! "$pkgver" = "$newver" ]; then
            echo "Bumping pkgver: $newver"
            sed -i "s/pkgver=.*$/pkgver=$newver/" "$pkgbuild" 
            sed -i "s/pkgrel=.*$/pkgrel=1/" "$pkgbuild"
            git_commit_if_changed "$srcpath" "Bump pkgver to '$newver'"            
        fi      
    fi
}



# ------------------------------------------------------------------ #
# AUR stuff
# ------------------------------------------------------------------ #

##
# aur_checkout <aurname> <path>
# Checkout a PKGBUILD from AUR
##
aur_checkout() {
    local aurname="$1"
    local srcpath="$2"
    local pkgname="$(basename "$srcpath")"
    cower -df --ignorerepo "$aurname" -t .
    (
        cd "$pkgname"
        git init
        git_commit_if_changed "$PWD" 'Initial commit'
    )
}

##
# aur_update <aurname> <path>
# Update git repo with last PKGBUILD from AUR.                                   
##
aur_update() {
    local aurname="$1"
    local srcpath="$(readlink -f "$2")"
    local pkgname="$(basename "$srcpath")"
    # NOTE: only update on branch master, to preserve possible local changes.
    if [ ! "$(git symbolic-ref HEAD 2>/dev/null)" == 'refs/heads/master' ]; then
        echo "Not on branch master"
        return 1
    fi              
    cower -df --ignorerepo "$aurname" -t ..
    git_commit_if_changed "$PWD" "Updated from AUR"
}



# ------------------------------------------------------------------ #
# package stuff
# ------------------------------------------------------------------ #

##
# package_missing <path>
# print a list of missing package files
##
package_missing() {
    local srcpath="$(readlink -f "$1")"
    pkgbuild_packages "$srcpath" |
        while read pkg; do                             
            if [ ! -f  "$srcpath/$pkg" ]; then
                echo "$srcpath/$pkg"
            fi
        done
}


##
# package_check_directory <path>
# Fail if path does not contain a PKGBUILD
##
package_check_directory () {
    local srcpath="$(readlink -f "$1")" 
    if [ ! -f "$srcpath/PKGBUILD" ]; then
        echo "Can't find a PKGBUILD in $1"
        exit 1
    fi   
}

##
# package_cleanold <path> <pkgname> <n>
# Remove old packages and signatures, and keep the last n.
##
package_cleanold() {
    local pkgpath="$1"
    local pkgname="$2"
    local num="$3"
    local EXTS=('pkg\.tar\.xz' 'pkg\.tar\.xz\.sig')
    echo "Cleaning old packages in '$pkgpath'"
    for ext in ${EXTS[@]}; do
        find "$pkgpath" -regex ".*/$pkgname-\(.*\)\.$ext" -print | \
            sort | head -n -$num | xargs -r rm
    done
}

##
# package_build <path>
# Build a package at the given directory.
##
package_build() {
    local srcpath="$(readlink -f "$1")"
    shift
    
    # clean older packages
    pkgbuild_packages "$srcpath" |
        while read pkg; do
            pkgname="${pkg%-*-*-*}"
            package_cleanold "$srcpath" "$pkgname" 0
        done
    (
        cd "$srcpath"
        makepkg -L -f --sign "$@"
    )
}

##
# package_repackage <path>
# Repackage a package at the given directory.
##
package_repackage() {
    local srcpath="$(readlink -f "$1")"
    (
        cd "$srcpath"
        makepkg -f --repackage --sign
    )
}

##
# package_download <path>
# Download package sources.
##
package_download() {
    local srcpath="$(readlink -f "$1")"
    (
        cd "$srcpath"
        makepkg --nobuild
    )            
}

##
# package_pull_sources <path>
# Pulls all sources pointing to a local git repo.
##
package_pull_sources() {
    local srcpath="$1"
    local pkgname="$(basename "$srcpath")"
    package_check_directory "$srcpath"
    pkgbuild_sources "$srcpath" |
        while read src; do
            local reponame=$(echo "$src" | sed 's|^\(.*\)::.*$|\1|')
            local repopath=$(echo "$src" | sed 's|^.*file://\(.*\)$|\1|')
            if [ -d "$repopath" ]; then
                (
                    cd "$repopath"
                    mr update
                )                    
                # NOTE: If I symlink to local repos instead of listing in the sources 
                # variable of the PKGBUILD, I could save disk on the build dir, but 
                # unfortunately makepkg does not understand it.
                # rm -Rf "$reponame"
                # ln -sfv "$repopath" "$reponame"
            fi   
        done
}

##
# package_print_changelog <path>
# Prints a changelog of the last pull for all local git sources
# More concreately, prints a concise log between between last reflog jump @{1}..@{0}.
##
package_print_changelog() {
    local srcpath="$1"
    local pkgname="$(basename "$srcpath")"
    package_check_directory "$srcpath"
    pkgbuild_sources "$srcpath" |
        while read src; do
            local reponame=$(echo "$src" | sed 's|^\(.*\)::.*$|\1|')
            local repopath=$(echo "$src" | sed 's|^.*git+file://\(.*\)$|\1|')
            if [ -d "$repopath" ]; then
                (
                    cd "$repopath"
                    git --no-pager log --pretty=format:"%C(cyan)%h %C(green)[%ad] %C(red)%an%C(reset). %s%d" --graph --date=relative @{1}..@{0}
                    echo ""                    
                )                    
            fi   
        done
}

##
# package_install <path>
# Install a package.
##
package_install() {
    local srcpath="$(readlink -f "$1")"
    pkgbuild_packages "$srcpath" |
        while read pkg; do
            if [ -f "$srcpath/$pkg" ]; then
                sudo pacman --noconfirm -U "file://$srcpath/$pkg"               
            fi             
        done
}

##
# package_install_deps <path>
# Install package deps.
##
package_install_deps() {
    local srcpath="$(readlink -f "$1")"
    (
        cd "$srcpath"
        makepkg -s --nobuild --noextract --skipinteg
    )
}



# ------------------------------------------------------------------ #
# repo stuff
# ------------------------------------------------------------------ #


##
# repo_get_dbfile <repo> <arch>
# Return absolute path of a repo database for an architecture
##
repo_get_dbfile() {
    local tgtroot="$1"
    local arch="$2"
    echo "$tgtroot/os/$arch/$(basename "$tgtroot").db.tar.gz"
}

##
# repo_get_arches <repo>
# Return list of architectures present in repo
##
repo_get_arches() {
    local tgtroot="$1"
    find "$tgtroot/os" -maxdepth 1 -mindepth 1 -type d -printf '%P\n'
}

##
# repo_outdated_packages <src> <repo>
# List the packages produced by <src>/PKGBUILD that will be newer than the ones in repo.
##
repo_outdated_packages() {
    local srcpath="$(readlink -f "$1")"
    local tgtroot="$2"
    local repodb tgtrepo

    pkgbuild_packages "$srcpath" |
        while read pkg; do                             
    
            for arch in "$(repo_get_arches "$tgtroot")"; do
                repodb="$(repo_get_dbfile "$tgtroot" "$arch")"
                tgtrepo="$(dirname "$repodb")"
                if [ ! -f  "$tgtrepo/$pkg" ]; then
                    echo "$pkg"
                fi
            done
        done
}

##
# repo_add_package <src> <repo> <arch>
# Add all packages in <src> to the pacman repo and clean older versions of them.
##
repo_add_packages() {
    local srcpath="$(readlink -f "$1")"
    local tgtroot="$2"
    local arch="$3"            
    local repodb="$(repo_get_dbfile "$tgtroot" "$arch")"
    local tgtrepo="$(dirname "$repodb")"
    local pkgext="$(get_makepkg_config PKGEXT)"
    local pkgname
    # make sure repo database exists with right perms.
    install -d -m755 "$tgtrepo"
    touch "$repodb"
    # move the packages to the repo
    pkgbuild_packages "$srcpath" |
        while read pkg; do
            if [[ "$pkg" == *-$arch$pkgext ]] || [[ "$pkg" == *-any$pkgext ]] ; then
                pkgname="${pkg%-*-*-*}"
                # clean older packages
                # TODO: This is crappy, since we can't unambiguously separate
                # package name from package version. When pacman gets updated
                # I will be able to use repo-add --remove, that removes the old file!!
                package_cleanold "$tgtrepo" "$pkgname" 0
                if [ -f "$srcpath/$pkg" ]; then
                    # copy package and signature
                    install -m644 "$srcpath/$pkg" "$tgtrepo/$pkg"
                    install -m644 "$srcpath/$pkg.sig" "$tgtrepo/$pkg.sig"
                    repo-add "$repodb" "$tgtrepo/$pkg"
                else
                    echo "package file not found: $pkg"
                    return 1
                fi
            fi
        done
    git_annex_commit_if_changed "$tgtroot" "auto-commit on $(hostname)"
}

##
# repo_remove_package <src> <repo> <arch>
# Remove all packages produced by PKGBUILD in <src> from repo.
##
repo_remove_packages() {
    local srcpath="$(readlink -f "$1")"
    local tgtroot="$2"
    local arch="$3"
    local repodb="$(repo_get_dbfile "$tgtroot" "$arch")"
    local tgtrepo="$(dirname "$repodb")"
    local pkgext="$(get_makepkg_config PKGEXT)"
    local pkgname
    pkgbuild_packages "$srcpath" |
        while read pkg; do
            if [[ "$pkg" == *-$arch$pkgext ]] || [[ "$pkg" == *-any$pkgext ]] ; then
                pkgname="${pkg%-*-*-*}"
                package_cleanold "$tgtrepo" "$pkgname" 0
                repo-remove "$repodb" "$pkgname"
            fi
        done
    git_annex_commit_if_changed "$tgtroot" "auto-commit on $(hostname)"
}


# ------------------------------------------------------------------ #
# ABS
# ------------------------------------------------------------------ #

##
# abs_update <repo> <path>
# Synchronize abs repo tree and commit changes
##
abs_update() {
    server="rsync.archlinux.org"
    repo="$1"
    arch="$(get_makepkg_config CARCH)"
    rsargs='-mrtv --no-motd --delete-after --exclude .git --no-p --no-o --no-g'
    if [ -z "$repo" ]; then
        error "Missing repo"
    fi   
    if [ -z "$path" ]; then
        error "Missing target path"
    fi   
    rsync $rsargs "$server"::abs/{$arch,any}/$repo/ "$path/"
    git_commit_if_changed "$path" "auto-commit on $(hostname)"    
}

##
# abs_checkout <repo>
# Synchronize abs repo tree and commit changes
##
abs_checkout() {
    repo="$1"
    path="$2"
    if [ -z "$repo" ]; then
        error "Missing repo"
    fi   
    if [ -z "$path" ]; then
        error "Missing target path"
    fi   
    mkdir -p "$path"
    (
        cd "$path"
        git init
    )
    abs_update "$repo" "$path"
}
