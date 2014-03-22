#!/bin/bash

journalctl -f -n0 -q --priority=3 SYSLOG_IDENTIFIER=systemd | \
while read line; do
    user=$(id -nu)
    subject="Errors detected while monitoring journal on '$(hostname -s)'"
    message="Log entry: $line"

    # mail information and wait
    echo "mailing fail report to user $user"
    echo "$message" | mail -s "$subject" $user
    sleep 60
done
