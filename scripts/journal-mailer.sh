#!/bin/bash

# Monitors the journal for systemd unit startup errors.
# Batches them and sends an email report before quitting.

# trap SIGINT and SIGTERM
trap mailer_quit SIGINT SIGTERM


ERROR_LOG=""

email_report() {
    user=$(id -nu)
    host=$(hostname -s)
    subject="Errors detected while monitoring journal on '$host'"
    message="Journal:\n$1\n"

    echo "mailing fail report to user $user"
    echo -e "$message" | mail -s "$subject" $user

    ERROR_LOG=""
}


mailer_quit() {
    if [ "$ERROR_LOG" ]; then
        email_report "$ERROR_LOG"
        sleep 5   # give it some time

    else
        echo "no error reports to mail"
    fi
    exit 0
}


# data collection loop we redirect at the end to avoid spawning subshells
# and be able to change the global variable ERROR_LOG
while read line; do
    ERROR_LOG="$ERROR_LOG$line\n"
done < <(journalctl -f -n0 -q --priority=3 --since=now SYSLOG_IDENTIFIER=systemd)
