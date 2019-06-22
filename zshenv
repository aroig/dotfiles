#!/bin/bash

#------------------------------------------------------------------#
# NOTE:                                                            #
# In order to export the environment to the systemd --user         #
# session this file gets parsed by a shell script. So keep it      #
# in a clean format with lines like                                #
# export VAR=value                                                 #
#------------------------------------------------------------------#


function add_to_pathlist_left () {
    local cl="$1"
    local item="$2"
    local cl_content

    eval "cl_content=\$$cl"
    case ":$cl_content:" in
        *":$item:"*)                    ;;       # already there
        *) eval "$cl=$item:$cl_content" ;;
    esac
}


#------------------------------
# Personal paths
#------------------------------

export AB2_HOME="/home/abdo"
export AB2_ARCH_DIR="$AB2_HOME/arch"
export AB2_PRIV_DIR="$AB2_HOME/priv"
export AB2_DEVEL_DIR="$AB2_HOME/devel"
export AB2_SOFT_DIR="$AB2_HOME/bak/soft"
export AB2_CONF_DIR="$AB2_HOME/etc"
export AB2_MAIL_DIR="$AB2_HOME/mail"
export AB2_MUSIC_DIR="$AB2_HOME/music"
export AB2_CALIBRE_DIR="$AB2_HOME/lib"
export AB2_PROJ_DIR="$AB2_HOME/proj"
export AB2_SHARE_DIR="$AB2_HOME/share"
export AB2_SRC_DIR="$AB2_HOME/src"
export AB2_VAR_DIR="$AB2_HOME/var"
export AB2_MATHS_DIR="$AB2_HOME/maths"
export AB2_WIKI_DIR="$AB2_HOME/wiki"
export AB2_PAPERS_DIR="$AB2_HOME/maths/papers"
export AB2_VIRT_DIR="$AB2_HOME/virt"
export AB2_BUILD_DIR="$AB2_HOME/build"
export AB2_REPOS_DIR="$AB2_HOME/repos"
export AB2_TMP_DIR="$AB2_HOME/tmp"
export AB2_BAK_DIR="$AB2_HOME/bak"
export AB2_TRASH_DIR="$AB2_HOME/var/trash"



#------------------------------
# Desktop
#------------------------------

# Disable GVFS madness. It triggers my automounts constantly.
# It may be fixed in gvfs 1.31, but anyway, I do not use it.
# https://developer.gnome.org/gio/stable/running-gio-apps.html
export GIO_USE_VFS="local"
export GIO_USE_VOLUME_MONITOR="unix"

# Icon theme
export ICON_THEME="/usr/share/icons/Papirus-Dark"



#------------------------------
# Sockets
#------------------------------

export EMACS_SOCK="$XDG_RUNTIME_DIR/emacs/server"

# gpg-agent sockets. Those sockets are proxied for socket activation!
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh"
export GPG_AGENT_INFO="$XDG_RUNTIME_DIR/gnupg/S.gpg-agent::1"

# mpd socket
export MPD_HOST="$XDG_RUNTIME_DIR/mpd/mpd.socket"

# screen socket
export SCREENDIR="$XDG_RUNTIME_DIR/screen"



#------------------------------
# Tools
#------------------------------

export EMACS="/usr/bin/emacsclient -s $XDG_RUNTIME_DIR/emacs/server -c"



#------------------------------
# Devel
#------------------------------

# Sailfish
export SAILFISH_SDK_ROOT="$HOME/sdk/sailfish/sdk"

# Go
export GOPATH="$HOME/usr/go"


#------------------------------
# Wine
#------------------------------

export WINEPREFIX="$HOME/.wine32"
export WINEARCH=win32


#------------------------------
# Paths
#------------------------------

# My local terminfo
export TERMINFO="$HOME/.terminfo:/usr/share/terminfo"

# cdpath
# export CDPATH=""

# The path
add_to_pathlist_left PATH "$HOME/bin"
add_to_pathlist_left PATH "$HOME/.local/bin"
export PATH="$PATH"

