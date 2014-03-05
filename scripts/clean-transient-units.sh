#!/bin/bash
set -o errexit

SYSTEMD_USER_DIR="$HOME/.config/systemd/user"

# clean transient scopes
echo "Cleaning transient units in $SYSTEMD_USER_DIR"

find "$SYSTEMD_USER_DIR" -regex '.*/run-\([0-9]+\)\.\(scope\|service\)\(\.d\)?$' | \
while read tunit; do
    rm -Rf "$tunit"
done

