#!/usr/bin/zsh
#------------------------------------------------------------------#
# File:     keybindings.zsh    Keybinding adjustments              #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


# NOTE: on a terminal do C-v + key, to see the escaped character!

# NOTE: This is specific to termite, and does not seem to work... oh well



#------------------------------
# General Keybindings
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


#------------------------------
# Terminal Specific
#------------------------------

case $TERM in
    xterm-termite*)    
        bindkey "\e[H"    beginning-of-line            # [Home] - move to beginning of line
        bindkey "\e[F"    end-of-line                  # [End]  - move to the end of line
        
        bindkey "\e[1;5C" forward-word                 # [C-Right] - move forward one word
        bindkey "\e[1;5D" backward-word                # [C-Left]  - move backward one word
        ;;

    rxvt*)
        bindkey "\e[8~" end-of-line
        bindkey "\e[7~" beginning-of-line
        ;;

    gnome-terminal*)
        bindkey "\eOH" beginning-of-line
        bindkey "\eOF" end-of-line
        ;;    
esac

