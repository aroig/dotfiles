#!/usr/bin/bash

cmd="$1"

localport=${1%%:*}
remoteport=${1##*:}
aux=${1#*:}
host=${1%:*}

case $cmd in
    direct)
        args=-L "$localport:localhost:$remoteport" "$host"
    ;;

    reverse)
        args=-R "$localport:localhost:$remoteport" "$host"
    ;;
esac

exec /usr/bin/ssh -q $args
