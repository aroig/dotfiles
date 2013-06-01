#!/bin/zsh

source /home/abdo/.environment
source /home/abdo/.aliases

case "$1" in 
    start)
        /usr/bin/awesome &
    ;;

    stop)
        echo "awesome.quit()" | /usr/bin/awesome-client
    ;;

    restart)
        echo "awesome.restart()" | /usr/bin/awesome-client
    ;;
esac


