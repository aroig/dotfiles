#!/bin/bash

mailer_quit() {
    [ "$_exit" == "signal" ]  && exit 0
    [ "$_exit" == "inhibit" ] && _exit=immediate
}

# trap SIGINT and SIGTERM so we make sure that we quit with all mail delivered
trap mailer_quit SIGINT SIGTERM

# if _exit=signal we quit ince SIGTERM is received
# if _exit=inhibit we wait to quit until it is appropriate
# if _exit=immediate we quit as we see it.

_exit=signal

journalctl -f -n0 -q --priority=3 --since=now SYSLOG_IDENTIFIER=systemd | \
while read line; do
    user=$(id -nu)
    subject="Errors detected while monitoring journal on '$(hostname -s)'"
    message="Log entry: $line"

    # mail information and wait
    echo "mailing fail report to user $user"

    _exit=inhibit
    echo "$message" | mail -s "$subject" $user

    [ "$_exit" == 'immediate' ] && exit 0
    _exit=signal

    sleep 60
done
