#!/bin/zsh
#------------------------------------------------------------------#
# File:     keybindings.zsh    Keybinding adjustments              #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#



#------------------------------
# Keybindings
#------------------------------
bindkey -e              # Emacs
typeset -g -A key

#bindkey '\e[3~' delete-char

bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\e[2~' overwrite-mode
bindkey '^?' backward-delete-char
bindkey '^[[1~' beginning-of-line
bindkey '^[[5~' up-line-or-history
bindkey '^[[2~' overwrite-mode
bindkey '^[[3~' delete-char
bindkey '^[[4~' end-of-line
bindkey '^[[6~' down-line-or-history
bindkey '^[[A' up-line-or-search
bindkey '^[[D' backward-char
bindkey '^[[B' down-line-or-search
bindkey '^[[C' forward-char 

# for rxvt
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line

# for gnome-terminal
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line
