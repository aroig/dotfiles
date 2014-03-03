#!/bin/bash

# shutdown stuff theat may need interaction
# ----------------------------------------------------------

# stop emacs
emacsclient --eval "(client-save-kill-emacs)"

# stop calibre
calibre --shutdown-running-calibre &> /dev/null

# kill apps.slice
systemctl --user kill apps.slice

# TODO: stop pidgin, boinc...

# kill dropdowns
echo "ddclient.kill_all()" | awesome-client

# kill xorg and go to console
systemctl --user start console.target

