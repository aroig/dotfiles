#!/usr/bin/python3
# -*- coding: utf-8 -*-
#
# journal-monitor - Monitor systemd journal and send notifications.
# Copyright 2013 Abdó Roig-Maranges <abdo.roig@gmail.com>
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

import subprocess
import json


class JournalMonitor(object):
    def __init__(self):
        self.jorunalcmd = ['sudo', 'journalctl']
        self.address = "root"
        self.proc = None



    def _event_loop(self):
        if self.proc:
            for line in self.proc.stdout:
                item = json.loads(line.decode('utf-8'))
                self.process(item)



    def start(self):
        self.proc = subprocess.Popen(self.journalcmd +
                                     ['--output=json', '--follow', '-n2'],
                                     stdout=subprocess.PIPE)
        self._event_loop()



    def stop(self):
        if self.proc:
            self.proc.terminate()
            self.proc = None



    # TODO: queue and compact email notifications
    def mail(self, item, address=None, subject=None, message=None):
        jcur = item['__CURSOR']
        jcomm = item['_COMM']
        jhost = item['_HOSTNAME']
        jmesg = item['MESSAGE']

        addr = address or self.address
        subj = subject or "Notification: systemd event for %s on %s" % (jcomm, jhost)
        mesg = message or "Message: %s\n\nJournal extract:\n%s" % (jmesg, self.journal_fragment(jcur))

        mailp = subprocess.Popen(['mail', '-s', subj, addr])
        mailp.communicate(input=mesg)

        if mailp.returncode != 0:
            print("Error. Could not send notification email")



    def notify(self, item, origin=None, title=None, message=None):
        jcur = item['__CURSOR']
        jcomm = item['_COMM']
        jhost = item['_HOSTNAME']
        jmesg = item['MESSAGE']

        orig = origin or comm
        titl = title or "Systemd notification"
        mesg = message or jmesg

        subprocess.call(['notify-send', '-a', orig, titl, mesg])



    def journal_fragment(self, cursor, lines=10):
        raw = subprocess.check_output(self.journalcmd +
                                      ["--lines=%d" % lines,
                                       "--cursor=%s" % cursor])
        return raw.decode('utf-8')



    def process(self, item):

        if item['_COMM'] == 'systemd':
            # Notify failures
            if re.search('Failed', item['MESSAGE']):
                self.notify(item)
                self.mail(item)

        elif irem['_COMM'] == 'ddclient':
            # Notify IP changes
            if re.search("blah", item['MESSAGE']):
                self.notify(item)

            # Notify mail sync


try:
    jm = JournalMonitor()
    jm.start()

except KeyboardInterrupt:
    print("")
    sys.exit(0)

except EOFError:
    print("")
    sys.exit(1)


# vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=4:textwidth=80
