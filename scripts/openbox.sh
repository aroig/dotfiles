#!/bin/bash -login

# get the environment
source /home/abdo/.zshenv
source /home/abdo/.aliases

case "$1" in 
    start)   exec /usr/bin/openbox ;;
    stop)     ;;
    restart)  ;;
esac
        
