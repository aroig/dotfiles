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
   local cl=$1
   local item=$2
   local cl_content

   eval "cl_content=\$$cl"
   if [[ -d $item ]]; then
      case ":$cl_content:" in
          *":$item:"*)                    ;;       # already there
          *) eval "$cl=$item:$cl_content" ;;
      esac
   fi
}



#------------------------------
# Some software
#------------------------------

export EMACSCLIENT="/usr/bin/emacsclient -s $XDG_RUNTIME_DIR/emacs/server"
export EMACS="$EMACSCLIENT -c"                          # emacs
export EDITOR="/usr/bin/vim"                            # default editor
export ALTERNATE_EDITOR="/usr/bin/emacs"                # emacsclient uses this if can't find server

export BROWSER="/usr/bin/dwb"                           # default browser
export FILEMANAGER="/usr/bin/thunar"                    # the file manager
export DIFFPROG="/usr/bin/emacs -diff"
export PAGER="/usr/bin/vimpager"                        # wrapper for vim
export TERMCMD="/usr/bin/termite"                       # terminal
export SHELL="/bin/zsh"



#------------------------------
# Some paths
#------------------------------

export AB2_HOME="/home/abdo"
export AB2_ARCH_DIR="$AB2_HOME/usr/arch"
export AB2_PRIV_DIR="$AB2_HOME/priv"
export AB2_DEVEL_DIR="$AB2_HOME/devel"
export AB2_CONF_DIR="$AB2_HOME/etc"
export AB2_MAIL_DIR="$AB2_HOME/mail"
export AB2_MUSIC_DIR="$AB2_HOME/music"
export AB2_CALIBRE_DIR="$AB2_HOME/lib"
export AB2_PROJ_DIR="$AB2_HOME/proj"
export AB2_SHARE_DIR="$AB2_HOME/share"
export AB2_VAR_DIR="$AB2_HOME/var"
export AB2_WORK_DIR="$AB2_HOME/work"
export AB2_WIKI_DIR="$AB2_HOME/work/wiki"
export AB2_PAPERS_DIR="$AB2_HOME/work/papers"



#------------------------------
# Gpg Agent settings
#------------------------------

# Those sockets are proxied for socket activation!
export SSH_AUTH_SOCK="$HOME/.gnupg/S.gpg-agent.ssh"
export GPG_AGENT_INFO="$HOME/.gnupg/S.gpg-agent::1"

# NOTE: I'll use this once I get socket activation working
# export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gpg-agent/S.gpg-agent.ssh"
# export GPG_AGENT_INFO="$XDG_RUNTIME_DIR/gpg-agent/S.gpg-agent::1"



#------------------------------
# Development environment
#------------------------------

# Default C/C++ compiler
export CC=gcc
export CXX=g++

# Mingw
export MINGW_ROOT="/usr/x86_64-w64-mingw32"

# Android
export ANDROID_NDK_ROOT="$HOME/sdk/android/ndk"
export ANDROID_SDK_ROOT="$HOME/sdk/android/sdk"
export ANDROID_QT_ROOT="$HOME/sdk/android/qt5.2"

# Sailfish
export SAILFISH_SDK_ROOT="$HOME/sdk/sailfish/sdk"



#------------------------------
# Wine
#------------------------------

export WINEPREFIX="$HOME/.wine"
export WINEARCH=win64



#------------------------------
# Application variables
#------------------------------

# gtk
# disable accessibility for GTK3 apps
# https://forums.gentoo.org/viewtopic-p-7380668.html?sid=265ddf475643bf5c86fe72440cf6cbe8
# export NO_AT_BRIDGE=1 

# sage
export SAGE_KEEP_BUILT_SPKGS='yes'    # don't rebuild from scratch
export SAGE_INSTALL_GCC='no'

export SAGE_ROOT=$AB2_HOME/sage

# ranger
export RANGER_LOAD_DEFAULT_RC=FALSE

# java
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dawt.useSystemAAFontSettings=on -Dswing.aatext=true'


#------------------------------
# Colors
#------------------------------

# LS_COLORS. color configuration for ls
# eval $(dircolors)                   # default LS_COLORS
_LS_COLORS=(
        'rs=0'              'di=01;34'          'ln=01;36'          'mh=00'
        'pi=40;33'          'so=01;35'          'do=01;35'          'bd=40;33;01' 
        'cd=40;33;01'       'or=40;31;01'       'su=37;41'          'sg=30;43'   
        'ca=30;41'          'tw=30;42'          'ow=34;42'          'st=37;44'    
        'ex=01;32'   

# packages
     '*.tar=01;31'       '*.tgz=01;31'       '*.arj=01;31'       '*.taz=01;31' 
     '*.lzh=01;31'      '*.lzma=01;31'       '*.tlz=01;31'       '*.txz=01;31' 
     '*.zip=01;31'         '*.z=01;31'         '*.Z=01;31'        '*.dz=01;31'     
      '*.gz=01;31'        '*.lz=01;31'        '*.xz=01;31'       '*.bz2=01;31'    
      '*.bz=01;31'       '*.tbz=01;31'      '*.tbz2=01;31'        '*.tz=01;31'    
     '*.deb=01;31'       '*.rpm=01;31'       '*.jar=01;31'       '*.war=01;31'    
     '*.ear=01;31'       '*.sar=01;31'       '*.rar=01;31'       '*.ace=01;31'   
     '*.zoo=01;31'      '*.cpio=01;31'        '*.7z=01;31'        '*.rz=01;31'    

# pictures
     '*.jpg=00;35'      '*.jpeg=00;35'       '*.gif=00;35'       '*.bmp=00;35'
     '*.pbm=00;35'       '*.pgm=00;35'       '*.ppm=00;35'       '*.tga=00;35' 
     '*.xbm=00;35'       '*.xpm=00;35'       '*.tif=00;35'      '*.tiff=00;35'  
     '*.png=00;35'       '*.svg=00;35'      '*.svgz=00;35'       '*.mng=00;35'  
     '*.pcx=00;35'     

# video
     '*.mov=00;35'       '*.mpg=00;35'      '*.mpeg=00;35'       '*.m2v=00;35'
     '*.mkv=00;35'      '*.webm=00;35'       '*.ogm=00;35'       '*.mp4=00;35'    
     '*.m4v=00;35'      '*.mp4v=00;35'       '*.vob=00;35'        '*.qt=00;35'     
     '*.nuv=00;35'       '*.wmv=00;35'       '*.asf=00;35'        '*.rm=00;35'     
    '*.rmvb=00;35'       '*.flc=00;35'       '*.avi=00;35'       '*.fli=00;35' 
     '*.flv=00;35'        '*.gl=00;35'      

# audio
     '*.aac=00;36'        '*.au=00;36'      '*.flac=00;36'       '*.mid=00;36'   
    '*.midi=00;36'       '*.mka=00;36'       '*.mp3=00;36'       '*.mpc=00;36'   
     '*.ogg=00;36'        '*.ra=00;36'       '*.wav=00;36'     

# unknown
      '*.dl=00;35'       '*.xcf=00;35'       '*.xwd=00;35'       '*.yuv=00;35'    
     '*.cgm=00;35'       '*.emf=00;35'       '*.axv=00;35'       '*.anx=00;35'    
     '*.ogv=00;35'       '*.ogx=00;35'    
     '*.axa=00;36'       '*.oga=00;36'       '*.spx=00;36'      '*.xspf=00;36'

# markup
    '*.html=00;35'     '*.xhtml=00;35'       '*.xml=00;35'

# latex
     '*.tex=00;32'       '*.ltb=00;32'       '*.bib=00;32'           

# documents
     '*.pdf=00;35'      '*.djvu=00;35'       '*.dvi=00;35'        '*.ps=00;35'
    '*.epub=00;35'      '*.mobi=00;35'       '*.chm=00;35'       '*.azw=00;35'

# text
     '*.org=00;33'       '*.rst=00;33'       '*.txt=00;33'
'*README.rst=01;33' '*README.md=01;33'     '*README=01;33'

# source code
 '*Makefile=00;31' '*CMakeLists.txt=00;31' '*.cmake=00;31'
      '*.hs=00;32'    
      '*.sh=00;32'       '*.zsh=00;32'        '*.el=00;32'
      '*.py=00;32'      '*.sage=00;34'       '*.lua=00;32'
       '*.c=00;32'       '*.cpp=00;32'         '*.h=00;32'       '*.hpp=00;32'

# config
    '*.conf=00;34'         '*rc=00;34'       '*.yml=00;34'

# data
  '*.pickle=00;34'      '*.json=00;34'

# systemd units
  '*.target=00;31'   '*.service=00;32'     '*.timer=00;34'      '*.path=00;36'  
   '*.mount=00;33' '*.automount=00;33'      '*.swap=00;33'
   '*.scope=00;36'     '*.slice=00;36'
  '*.socket=00;35'   '*.network=00;35'    '*.netdev=00;35'
  '*.device=00;36'      '*.link=00;36'
)
export LS_COLORS=$(printf "%s:" "${_LS_COLORS[@]}")


# SYSTEMD_COLORS. color configuration for systemd units in zsh autocompletion
_SYSTEMD_COLORS=(
          'rs=0'    '=run-*.service=0;37'   '=run-*.scope=0;37'
   '=*.target=00;33'    '=*.service=00;32'      '=*.timer=00;34'      '=*.path=00;36'   
    '=*.mount=00;31'  '=*.automount=00;31'       '=*.swap=00;31'
    '=*.scope=00;33'      '=*.slice=00;33'
   '=*.socket=00;35'    '=*.network=00;35'     '=*.netdev=00;35'
   '=*.device=00;35'       '=*.link=00;35' 

)
export SYSTEMD_COLORS=$(printf "%s:" "${_SYSTEMD_COLORS[@]}")

# gcc colors
_GCC_COLORS=(
    'error=01;31'   'warning=01;33'   'note=01;36' 
    'caret=01;32'   'locus=00;36'     'quote=00;36'
)

export GCC_COLORS=$(printf "%s:" "${_GCC_COLORS[@]}")



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
add_to_pathlist_left PATH "/usr/lib/ccache/bin"   # ccache binaries before gcc ones
add_to_pathlist_left PATH "$HOME/bin"             # ~/bin
export PATH=$PATH
