#!/bin/bash

#------------------------------------------------------------------#
# File:     environment.sh    Environment variables                #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

function add_to_pathlist_left () {
   local cl=$1
   local item=$2
   local cl_content

   eval "cl_content=\$$cl"
   if [[ -d $item ]]; then
      case ":$cl_content:" in
          *":$item:"*) ;;           # already there
          *) eval "$cl=$item:\$$cl" ;;
      esac
   fi
}



#------------------------------
# Some software
#------------------------------

export EDITOR=vim                        # default editor
export EMACS="emacsclient -c -a emacs"   # emacs
export BROWSER=chromium                  # default browser
export FILEMANAGER=thunar                # the file manager
export PAGER=vimpager                    # wrapper for vim
export TERMCMD=urxvt                     # terminal
export SHELL=zsh


#------------------------------
# Some paths
#------------------------------

export AB2_HOME="/home/abdo"
export AB2_PRIV_DIR="$AB2_HOME/priv"
export AB2_DEVEL_DIR="$AB2_HOME/devel"
export AB2_LIBS_DIR="$AB2_HOME/usr/lib"
export AB2_CONF_DIR="$AB2_HOME/etc"
export AB2_MAIL_DIR="$AB2_HOME/mail"
export AB2_MUSIC_DIR="$AB2_HOME/music"
export AB2_PROJ_DIR="$AB2_HOME/proj"
export AB2_SHARE_DIR="$AB2_HOME/share"
export AB2_VAR_DIR="$AB2_HOME/var"
export AB2_WORK_DIR="$AB2_HOME/work"
export AB2_WIKI_DIR="$AB2_HOME/work/wiki"
export AB2_PAPERS_DIR="$AB2_HOME/work/papers"


#------------------------------
# Development environment
#------------------------------

export CC="colorgcc"


#------------------------------
# Application variables
#------------------------------

# sage
export SAGE_KEEP_BUILT_SPKGS=yes    # don't rebuild from scratch
export SAGE_ROOT=$AB2_HOME/usr/sage


#------------------------------
# Paths
#------------------------------

# These are a personal things for my code
export BASHPATH="$AB2_LIBS_DIR/bash"

# Lua path for for personal libs
export LUA_PATH="$AB2_LIBS_DIR/lua/?.lua;;"
export LUA_CPATH="$AB2_LIBS_DIR/lua/?.lua;;"

# Python
export PYTHONDOCS=/usr/share/docs/python/html/

# My local terminfo
if [[ -d $HOME/.terminfo ]]; then
    export TERMINFO="$HOME/.terminfo:/usr/share/terminfo"
fi

# The path
add_to_pathlist_left PATH "$HOME/bin"
export PATH
