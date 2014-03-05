#!/bin/bash -login

# get the environment
source /home/abdo/.zshenv
source /home/abdo/.aliases

case "$1" in 
    start) 
        exec /usr/bin/awesome > /dev/null
        ;;
    stop)
        echo "ddclient.kill_all()" | /usr/bin/awesome-client
        echo "awesome.quit()" | /usr/bin/awesome-client 
        ;;
    restart)
        echo "awesome.restart()" | /usr/bin/awesome-client 
        ;;
esac
        
