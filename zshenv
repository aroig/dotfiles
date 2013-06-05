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


export AB2_PRIV_DIR="$HOME/priv"
export AB2_DEVEL_DIR="$HOME/devel"
export AB2_LIBS_DIR="$HOME/usr/lib"
export AB2_CONF_DIR="$HOME/etc"
export AB2_MAIL_DIR="$HOME/mail"
export AB2_MUSIC_DIR="$HOME/music"
export AB2_PROJ_DIR="$HOME/proj"
export AB2_SHARE_DIR="$HOME/share"
export AB2_VAR_DIR="$HOME/var"
export AB2_WORK_DIR="$HOME/work"
export AB2_WIKI_DIR="$HOME/work/wiki"
export AB2_PAPERS_DIR="$HOME/work/papers"


#------------------------------
# Development environment
#------------------------------

export CC="colorgcc"


#------------------------------
# Application variables
#------------------------------

SAGE_KEEP_BUILT_SPKGS=yes    # don't rebuild from scratch


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
