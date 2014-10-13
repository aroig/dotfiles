#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     tty.sh      Helper functions for ttys                  #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


set_tty_colors_xresources() {
    # awk script to extract colors from urxvt config
    color_config="$HOME/.Xresources"
    awk_script='
    /urxvt\.color/ { 
        idx = gensub(/urxvt\.color([0-9]+):/, "\\1", "g", $1); 
        color = gensub(/#([0-9a-fA-F]+)/, "\\1", "g", $2);
        printf("\\e]P%X%s\n", idx, toupper(color));
    }'
    cat "$color_config" | awk "$awk_script" | \
    while read color; do
        echo -n "$color"
    done
}


set_tty_colors_termite() {
    # awk script to extract colors from termite config
    color_config="$HOME/.config/termite/config"
    awk_script='
    /^color/ { 
        idx = gensub(/color([0-9]+)/, "\\1", "g", $0); 
        color = gensub(/.*#([0-9a-fA-F]+)/, "\\1", "g", $0);
        printf("\\e]P%X%s\n", idx, toupper(color));
    }'
    cat "$color_config" | awk "$awk_script" | \
    while read color; do
        echo -n "$color"
    done
}


# TODO: something is wrong with this on zsh. Oh well...
set_tty_colors() {
    
    if [[ -e $HOME/.config/termite/config ]]; then
        set_tty_colors_termite >> blah
            
    elif [[ -e "$HOME/.Xresources" ]]; then
        set_tty_colors_xresources
    fi
 
    echo -e "\\e]P0000000"
}

