#!/usr/bin/bash

# NOTE: If emacs.service is not active, always succeed since there is nothing to be done.
#       It would be nice to write the dependency on emacs.service directly in systemd!

if systemctl --user is-active -q "$1"; then
    exec emacsclient -s "$2"  --eval '(abdo-save-buffers-new-frame)' | grep nil > /dev/null
fi

