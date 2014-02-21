#!/bin/bash
set -o errexit

# tcp port for the agent
RCV_PORT=11000
FWD_PORT=11001

# local socket for the agent
GPG_SOCK=$HOME/.gnupg/S.gpg-agent

case $1 in
    start)
        exec gpg-agent --daemon --use-standard-socket
        ;;

    stop)
        # TODO
        ;;

    update)
        # update tty for gpg-agent
        exec gpg-connect-agent updatestartuptty /bye
        ;;

    forward)       
        # forward local tcp socket to the agent
        exec socat TCP-LISTEN:$RCV_PORT,bind=127.0.0.1 UNIX-CONNECT:$GPG_SOCK;
        ;;

    remote)
        # create a local agent socket and forward it to a tcp port
        exec socat UNIX-LISTEN:$GPG_SOCK,unlink-close,unlink-early TCP4:localhost:$FWD_PORT;
        ;;
esac
