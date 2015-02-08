#!/bin/bash

# Defaults
slice=apps.slice





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

usage() {
    echo "usage: $0 [ -s <slice> | -n <name> | -u <unit> ] <cmd> | <service>"
    echo ""
    echo "Starts a command in a transient service, or an existing service template"
    echo "instantiated at the first available integer parameter. "
}

if [ $# == 0 ]; then
    usage
    exit 0
fi

while getopts :n:u:s: opt; do
    case "$opt" in
        s) slice="$OPTARG.slice" ;;
        n) desc="$OPTARG" ;;
        u) unit="$OPTARG" ;;
    esac
done
shift $((OPTIND-1))

case "$1" in
    *@.service)
        basename="${1%\.*}"
        index=`available_index "$basename"`
        [ ! "$unit" ] && unit="$basename$index"
        [ ! "$desc" ] && desc="$basename $index"
        cmd="systemctl --user start $unit"
        ;;
    *.service)
        basename="${1%\.*}"
        [ ! "$unit" ] && unit="$basename"
        [ ! "$desc" ] && desc="$basename"
        cmd="systemctl --user start $unit"
        ;;
    *)
        basename="run-"`basename "$1"`"-"
        index=`available_index "$basename"`
        [ ! "$unit" ] && unit="$basename$index"
        [ ! "$desc" ] && desc="transient "`basename "$1"`" $index"
        cmd="systemd-run --user --slice=$slice --unit=$unit --description=\"$desc\" $@"
        ;;
esac

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






