#!/bin/bash

slice=apps.slice
name=transient

SYSTEMD_CONF="$HOME/.config/systemd"

while getopts n:s: opt; do
    case "$opt" in
        n) name=$OPTARG  ;;
        s) slice=$OPTARG ;;
    esac
    shift
done

declare -i index
for index in $(seq 0 100); do
    unit="run-$name-$index.service"    
    if systemctl --user is-active "$unit" > /dev/null; then continue; fi
    if [ -e "$SYSTEMD_CONF/user/$unit" ];  then continue; fi

    desc="$name $index"

    exec systemd-run --user --slice="$slice" --unit="$unit" --description="$desc" $@
    exit 0
done

echo "Can't find a transient unit name available"
exit 1






