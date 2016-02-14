#!/bin/bash

#------------------------------------------------------------------#
# NOTE:                                                            #
# In order to export the environment to the systemd --user         #
# session this file gets parsed by a shell script. So keep it      #
# in a clean format with lines like                                #
# export VAR=value                                                 #
#------------------------------------------------------------------#


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


#------------------------------
# Devel
#------------------------------

# Sailfish
export SAILFISH_SDK_ROOT="$HOME/sdk/sailfish/sdk"


#------------------------------
# Wine
#------------------------------

export WINEPREFIX="$HOME/.wine32"
export WINEARCH=win32


