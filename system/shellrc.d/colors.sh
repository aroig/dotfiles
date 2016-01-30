#!/usr/bin/bash
#------------------------------------------------------------------#
# File:     colors.sh          Shortcuts to colors                 #
# Version:                                                         #
# Author:   Abdó Roig-Maranges <abdo.roig@gmail.com>               #
#------------------------------------------------------------------#

typeset -Ag fx fg bg fg_bold fg_light bg_light _colors

# Effects
  fx[reset]='\e[00m';
   fx[bold]='\e[01m';    fx[unbold]='\e[21m';
  fx[faint]='\e[02m';   fx[unfaint]='\e[22m';
 fx[italic]='\e[03m';  fx[unitalic]='\e[23m';
  fx[under]='\e[04m';   fx[ununder]='\e[24m';
  fx[blink]='\e[05m';   fx[unblink]='\e[25m';
fx[reverse]='\e[07m'; fx[unreverse]='\e[27m';
   fx[hide]='\e[08m';    fx[unhide]='\e[28m';
 fx[strike]='\e[09m';  fx[unstrike]='\e[29m';

# color names
_colors[0]='black'
_colors[1]='red'
_colors[2]='green'
_colors[3]='yellow'
_colors[4]='blue'
_colors[5]='magenta'
_colors[6]='cyan'
_colors[7]='white'

# foreground and background colors
for c in {0..7}; do
          fg[${_colors[$c]}]="\e[3${c}m"
          bg[${_colors[$c]}]="\e[4${c}m"

     fg_bold[${_colors[$c]}]="\e[1;3${c}m"
    fg_light[${_colors[$c]}]="\e[9${c}m"
    bg_light[${_colors[$c]}]="\e[10${c}m"
done

# 256 color palette
for c in {000..255}; do
    fg[$c]="\e[38;5;${c}m"
    bg[$c]="\e[48;5;${c}m"
done

# test function
print_color_test() {
    echo ""
    echo -en "  fx: "
    echo -en "${fx[bold]}bold${fx[reset]}     "
    echo -en "${fx[italic]}italic${fx[reset]}      "
    echo -en "${fx[under]}under${fx[reset]}      "
    echo -en "${fx[blink]}blink${fx[reset]}    "
    echo -en "${fx[reverse]}reverse${fx[reset]}    "
    echo -en "${fx[strike]}strike${fx[reset]}      "
    echo ""
    echo ""

    printf "%10s %10s %10s %10s %10s\n" "fg" "bg" "fg_bold" "fg_light" "bg_light"
    echo "--------------------------------------------------------"

    local txt
    for c in ${_colors[@]}; do
        txt=$(printf "%10s " "$c")
        echo -en "${fg[$c]}$txt${fx[reset]}"
        echo -en "${bg[$c]}$txt${fx[reset]}"
        echo -en "${fg_bold[$c]}$txt${fx[reset]}"
        echo -en "${fg_light[$c]}$txt${fx[reset]}"
        echo -en "${bg_light[$c]}$txt${fx[reset]}"
        echo ""
    done
    echo ""

    local -i a b x
    for a in {0..15}; do
        for b in {0..15}; do
            x=16*$a+$b;
            c=$(printf "%03d" $x)
            printf "${bg[$c]}%3d${fx[reset]} " $x
        done
        echo ""
    done
    echo ""
}

# print_color_test


