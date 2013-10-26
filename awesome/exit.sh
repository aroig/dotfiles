#!/bin/bash

# shutdown stuff theat may need interaction
# ----------------------------------------------------------

# emacs
emacsclient --eval "(client-save-kill-emacs)"

# stopping calibre
calibre --shutdown-running-calibre &> /dev/null

# TODO: pidgin, emacsen, boinc

# kill dropdowns
echo "ddclient.kill_all()" | awesome-client

# quit awesome
echo "awesome.quit()" | awesome-client
