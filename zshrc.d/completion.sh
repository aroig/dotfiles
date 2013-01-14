#------------------------------------------------------------------#
# File:     completion.sh   Completion adjustments                 #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#



zmodload zsh/complist 
autoload -U compinit
compinit -u



setopt bash_auto_list     # Bring list on second tab.
setopt glob_complete      # Autocomplete with glob


# Don't want the trailing export. Only want to use it locally
eval 'local '${"$(dircolors)"%"export LS_COLORS"}

# LS_COLORS='rs=0:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:su=37;41:sg=30;43:tw=30;42:ow=34;42:st=37;44:ex=01;32:';


# zstyle :compinstall filename '${HOME}/.zshrc'

# Case insensitive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Completion for killall
zstyle ':completion:*:killall:*' command 'ps -u $USER -o cmd'

zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
# zstyle ':completion:*:warnings' format '%BSorry, no matches for: %d%b'
#-/buggy

# zstyle ':completion:*:pacman:*' force-list always
# zstyle ':completion:*:*:pacman:*' menu yes select

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select

zstyle ':completion::*:expand:*' tag-order all-expansions

# zstyle ':completion:*:*:kill:*' menu yes select
# zstyle ':completion:*:kill:*'   force-list always

# zstyle ':completion:*:*:killall:*' menu yes select
# zstyle ':completion:*:killall:*'   force-list always


#--------------------------#
# SSH hosts                #
#--------------------------#

#hosts=$(awk '/^Host / {printf("%s ",$2)}' ~/.ssh/config 2>/dev/null)
#zstyle ':completion:*:hosts' hosts $hosts
