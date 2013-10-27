#!/bin/bash
# dwb: Control s

CURRENT_STYLESHEET="$(dwbremote get setting user-stylesheet-uri)"

STYLESHEET_1="file://$HOME/.config/dwb/styles/userdark.css"
STYLESHEET_2="file://$HOME/.config/dwb/styles/user.css"

if [[ "${CURRENT_STYLESHEET}" = ${STYLESHEET_1} ]]; then
    dwbremote :local_set user-stylesheet-uri "$STYLESHEET_2"
else 
    dwbremote :local_set user-stylesheet-uri "$STYLESHEET_1"
fi
