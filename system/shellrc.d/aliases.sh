#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     alias.sh   Aliases                                     #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

#------------------------------
# Utility aliases
#------------------------------

# ls
alias ls='ls --human-readable --color=auto --quoting-style=literal -F'
alias ll='ls++ --potsf'
alias lt='tree -C'

# filesystem stuff
alias df='dfc'
alias dul='cdu -dLh -i'
alias du='cdu -dh -i'

# monitoring
alias top='htop'
alias gl='glances'

# navigation
alias ud='pushd'
alias od='popd'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."

# file manipulation
alias trash='gvfs-trash'
alias rsy='rsync -avz --progress --delete'
alias rnm='/usr/bin/vendor_perl/rename'

# file viewing
alias vp='vimpager'
alias vc='vimcat'
alias diff='diff -u -d'
alias grep='grep --color=auto'

# emacs aliases
alias ediff='emacs-diff'
alias egit='emacs-git'

# mr
alias mr='mr --stats --color -t'

# make
alias make='TERM=xterm make -r --no-print-directory'
alias mk='TERM=xterm make -r --no-print-directory --warn-undefined-variables'
alias mkp='MAKEFLAGS="-j 4 -O target" TERM=xterm make -r --no-print-directory'
alias mkc='PATH="/usr/lib/ccache/bin:$PATH" MAKEFLAGS="-j 4 -O target" TERM=xterm make -r --no-print-directory'

# add project directory to rtags
rcadd() {
    local root="`realpath "$1"`"
    local compiledb="`find ${root}/build -name 'compile_commands.json' | head -1`"
    if [ "$compiledb" ]; then
        rc "--project-root=$root" -J "$compiledb"
    else
        echo "Could not find compilation database compile_commands.json"
        return 1
    fi
}

# project setup
alias ckc='cookiecutter'

# git aliases
alias ggr='git --no-pager grep'
alias gan="git annex"
alias glg='git log'
alias gls="abdo_git_ls"

# python
alias ipy='ipython'
alias ipy2='ipython2'

# software
alias wee='weechat-curses'
alias fehp='feh -Tpreview'
alias mailq='msmtp-queue'
alias uzbl="uzbl-tabbed"
alias octave="octave-cli"

