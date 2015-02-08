#!/bin/bash

# Defaults
slice=apps.slice


if [ $# == 0 ]; then
    echo "usage: sdrun [<cmd> | <service>]"
    echo ""
    echo "Starts a command in a transient service, or an existing service template"
    echo "instantiated at the first available integer parameter. "

    exit 0
fi



available_index() {
    local basename="$1"
    local -i index
    local unit
    for index in $(seq 0 100); do
        unit="$basename$index.service" 
        if systemctl --user is-active "$unit" > /dev/null; then
            continue;
        fi
        echo "$index"
        return 0
    done
    
    echo "Can't find a transient unit name available"
    exit 1
}

case "$1" in
    *@.service)
        basename="${1%\.*}"
        index=`available_index "$basename"`
        unit="$basename$index"
        desc="$basename $index"
        cmd="systemctl --user start $unit"
        ;;
    *.service)
        basename="${1%\.*}"
        unit="$basename"
        desc="$basename"
        cmd="systemctl --user start $unit"
        ;;
    *)
        basename="run-"`basename "$1"`"-"
        index=`available_index "$basename"`
        unit="$basename$index"
        desc="transient "`basename "$1"`" $index"
        cmd="systemd-run --user --slice=$slice --unit=$unit --description=\"$desc\" $@"
        ;;
esac

while getopts n:u:s: opt; do
    case "$opt" in
        s) slice=$OPTARG ;;
        n) desc=$OPTARG ;;
        u) unit=$OPTARG ;;
    esac
    shift
done

case "$1" in
    *@.service)
        cmd="systemctl --user start $unit"
        ;;
    *.service)
        cmd="systemctl --user start $unit"
        ;;
    *)
        cmd="systemd-run --user --slice=\"$slice\" --unit=\"$unit\" --description=\"$desc\" $@"
        ;;
esac

eval "$cmd"






