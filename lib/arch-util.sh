
## 
# package_repo <path>
# Get the repo a package belongs
##
package_repo() {
    local srcpath="$(readlink -f "$1")"            
    local srcdir="$(dirname "$srcpath")"
    echo "$(basename "$srcdir")"
}


##
# repodir_path <repo>
# Get the path of the repository.
##
repodir_path() {
    local reponame="$1"
    echo "$MR_ARCH_REPOS/$reponame"
}


##
# builddir_path <path>
# Get the path of the directory where the package will be built.
##
builddir_path() {
    # need to dereference symlinks! Otherwise subtle crap may happen when
    #  building stuff. For instance, emacs gets the autoloads paths wrong       
    local builddir="$(readlink -f "$MR_BUILD_PATH")"
    local srcpath="$(readlink -f "$1")"
    local pkgname="$(basename "$srcpath")"
    # Error out if builddir does not exist
    if [ ! -d "$builddir" ]; then
        error "Build directory does not exist. Please create it: $builddir"
        exit 1
    fi   
    echo "$builddir/$pkgname"           
}


##
# prepare_build <path>
# Prepares a package for building on the builddir
# returns the path where the building will occur
##
prepare_build() {
    local srcpath="$(readlink -f "$1")"
    local buildpath="$(builddir_path "$srcpath")"
    rsync -avz --exclude=.git "$srcpath/" "$buildpath/" > /dev/null
    echo "$buildpath"            
}


##
# pkgbuild_update_version <path>
# Updates the PKGBUILD from the one on the build dir (for updated version)
# It does so only for packages whose version gets updated dynamically.
# Returns true if version changes or false if it stays the same.
##
pkgbuild_update_version() {
    local srcpath="$(readlink -f "$1")"
    local buildpath="$(builddir_path "$srcpath")"
    local reponame="$(package_repo "$1")"           
    if pkgbuild_has_dynamic_version "$srcpath" && [ -f "$buildpath/PKGBUILD" ]; then
        local oldver="$(pkgbuild_parse "$srcpath/PKGBUILD" pkgver)"
        local newver="$(pkgbuild_parse "$buildpath/PKGBUILD" pkgver)"
        pkgbuild_bump_version "$srcpath" "$newver" && return 0
    fi
    return 1
}

