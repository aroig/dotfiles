#!/bin/bash

#------------------------------------------------------------------#
# File:     environment.sh    Environment variables                #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

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
# Some software
#------------------------------

export EMACS="$HOME/bin/emacs-client"                   # emacs
export EDITOR="/usr/bin/vim"                            # default editor
export ALTERNATE_EDITOR="/usr/bin/emacs"                # emacsclient uses this if can't find server

export BROWSER="/usr/bin/dwb"                           # default browser
export FILEMANAGER="/usr/bin/thunar"                    # the file manager
export DIFFPROG="$HOME/bin/emacs-diff"                  # tool to display diffs
export PAGER="/usr/bin/vimpager"                        # wrapper for vim
export TERMCMD="/usr/bin/termite"                       # terminal
export SHELL="/bin/zsh"

# so we can use $HOSTNAME everywhere
export HOSTNAME=$(hostname)



#------------------------------
# Some paths
#------------------------------

export AB2_HOME="/home/abdo"
export AB2_ARCH_DIR="$AB2_HOME/arch"
export AB2_PRIV_DIR="$AB2_HOME/priv"
export AB2_DEVEL_DIR="$AB2_HOME/devel"
export AB2_SOFT_DIR="$AB2_HOME/usr/soft"
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



#------------------------------
# Sockets
#------------------------------

# gpg-agent sockets. Those sockets are proxied for socket activation!
export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
export GPG_AGENT_INFO="$HOME/.gnupg/S.gpg-agent::1"

# mpd socket
export MPD_HOST="$XDG_RUNTIME_DIR/mpd/mpd.socket"

# screen socket
export SCREENDIR="$XDG_RUNTIME_DIR/screen"



#------------------------------
# Development environment
#------------------------------

# Default C/C++ compiler
export CC=gcc
export CXX=g++

# Mingw
export MINGW_ROOT="/usr/x86_64-w64-mingw32"



# Java development
export JAVA_HOME="/usr/lib/jvm/default"

# Android variables for the toolchain
export ANDROID_NDK_ROOT="/opt/android-ndk"    # root for NDK (understood upstream)
export ANDROID_SDK_ROOT="/opt/android-sdk"    # root for SDK (understood upstream)
export ANDROID_LIBS_ROOT="/opt/android-libs"  # root for custom libs (just mine)

# Android SDK wants to set this
export ANDROID_HOME="/opt/android-sdk"
export ANDROID_SWT="/usr/share/java"



# Sailfish
export SAILFISH_SDK_ROOT="$HOME/sdk/sailfish/sdk"



#------------------------------
# Wine
#------------------------------

export WINEPREFIX="$HOME/.wine32"
export WINEARCH=win32



#------------------------------
# Application variables
#------------------------------

# gtk
# disable accessibility for GTK3 apps
# https://forums.gentoo.org/viewtopic-p-7380668.html?sid=265ddf475643bf5c86fe72440cf6cbe8
# export NO_AT_BRIDGE=1 

# sage
export SAGE_ROOT="$AB2_HOME/sage"

# ranger
export RANGER_LOAD_DEFAULT_RC=FALSE

# java
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true'

# QT5 style
export QT_STYLE_OVERRIDE=gtk

# nix uses this
export SSL_CERT_FILE="/etc/ssl/certs/ca-certificates.crt"



#------------------------------
# Paths
#------------------------------

# Lua path for for personal libs.
# Don't want it for now. prefer installing the libs

# export LUA_PATH="$AB2_DEVEL_DIR/lua/?.lua;;"
# export LUA_CPATH="$AB2_DEVEL_DIR/lua/?.lua;;"

# Python
export PYTHONDOCS=/usr/share/docs/python/html/

# My local terminfo
if [[ -d $HOME/.terminfo ]]; then
    export TERMINFO="$HOME/.terminfo:/usr/share/terminfo"
fi

# cdpath
# export CDPATH=""

# The path
add_to_pathlist_left PATH "$HOME/bin"             # ~/bin
export PATH="$PATH"
