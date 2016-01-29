#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     tty.sh      Helper functions for ttys                  #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#


list_colors_xresources() {
    # awk script to extract colors from urxvt config
    color_config="$HOME/.Xresources"
    awk_script='
    /urxvt\.color/ { 
        idx = gensub(/urxvt\.color([0-9]+):/, "\\1", "g", $1); 
        color = gensub(/#([0-9a-fA-F]+)/, "\\1", "g", $2);
        printf("%X%s\n", idx, toupper(color));
    }'
    cat "$color_config" | awk "$awk_script"   
}


list_colors_termite() {
    # awk script to extract colors from termite config
    color_config="$HOME/.config/termite/config"
    awk_script='
    /^color/ { 
        idx = gensub(/color([0-9]+)/, "\\1", "g", $0); 
        color = gensub(/.*#([0-9a-fA-F]+)/, "\\1", "g", $0);
        printf("%X%s\n", idx, toupper(color));
    }'
    cat "$color_config" | awk "$awk_script"
}


# TODO: something is wrong with this in graphical terminals
set_tty_colors() {
   
    (
        if [[ -e $HOME/.config/termite/config ]]; then
            list_colors_termite
            
        elif [[ -e "$HOME/.Xresources" ]]; then
            list_colors_xresources
        fi
    ) | while read color; do
            echo -e -n "\e]P$color"
        done
 
    echo -e -n "\e]P0000000"
}

