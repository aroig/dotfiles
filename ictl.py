#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# ictl - run systemctl --user start and print logs on stdout
# Copyright 2014 Abdó Roig-Maranges <abdo.roig@gmail.com>
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

from optparse import OptionParser

import re
import sys
import json
import datetime

from subprocess import Popen, check_call, CalledProcessError, PIPE
from threading import Thread


_color = {
    'gray'    : "\033[0;90m",
    'red'     : "\033[0;91m",
    'green'   : "\033[0;92m",
    'yellow'  : "\033[0;93m",
    'blue'    : "\033[0;94m",
    'magenta' : "\033[0;95m",
    'cyan'    : "\033[0;96m",
    'white'   : "\033[0;97m",
    'reset'   : "\033[0m",
}

_keywords = {
    '(W|w)arning' : 'yellow',
    'WARNING'     : 'yellow',
    '(E|e)rror'   : 'red',
    'ERROR'       : 'red',
}

class StreamWriter(Thread):
    """Gets lines from a stream and processes them"""

    def __init__(self, stream, color=False):
        super(StreamWriter, self).__init__()
        self.stream = stream
        self.color = color

    def format_service(self, name):
        if name == None: return "<none>"
        c = _color['reset']
        if self.color:
            if name == 'systemd': c = _color['green']
            else:                 c = _color['yellow']

        return '%s%s%s' % (c, name, _color['reset'])

    def format_timestamp(self, timestamp):
        if timestamp == None: return ""
        c = _color['reset']
        if self.color: c = _color['gray']
        return '%s%s%s' % (c, timestamp.strftime('%H:%M:%S'), _color['reset'])

    def format_message(self, message):
        if message == None: return ""
        for regexp, c in _keywords.items():
            message = re.sub(regexp, "%s\g<0>%s" % (_color[c], _color['reset']), str(message))
        return message

    def print_entry(self, entry):
        unit      = entry.get('_SYSTEMD_USER_UNIT', None) or entry.get('USER_UNIT', None)
        name      = entry.get('SYSLOG_IDENTIFIER', None)
        message   = entry.get('MESSAGE', None)
        hostname  = entry.get('_HOSTNAME', None)

        priority  = entry.get('PRIORITY', None)
        if priority != None: priority = int(priority)

        timestamp = entry.get('__REALTIME_TIMESTAMP', None)
        if timestamp != None: timestamp = datetime.datetime.fromtimestamp(1e-6 * int(timestamp))

        service_txt   = self.format_service(name)
        timestamp_txt = self.format_timestamp(timestamp)
        message_txt   = self.format_message(message)

        print("%s %s: %s" % (timestamp_txt, service_txt, message_txt))

    def run(self):
        for line in self.stream:
            entry = json.loads(line.decode('utf8'))
            self.print_entry(entry)
        self.stream.close()



class Journal:
    def __init__(self, color=False, lines=0):
        self.proc = None
        self.stream = None
        self.color = color
        self.lines = lines

    def __enter__(self):
        self.proc = Popen(['journalctl', '--quiet', '--follow',
                           '--output=json', '--lines=%d' % self.lines],
                          stdout=PIPE, bufsize=-1)
        self.stream = StreamWriter(stream=self.proc.stdout, color=self.color)
        self.stream.start()
        return self.proc

    def __exit__(self, type, value, traceback):
        self.proc.terminate()
        self.stream.join()
        self.proc = None
        self.stream = None



usage = """usage: %prog <units>...

Run systemctl --user start, and prints the journal log while units are starting.
"""
parser = OptionParser(usage=usage)

parser.add_option("-c", "--no-color", action="store_true", dest="nocolor",
                  help="Do not colorize output.")

parser.add_option("-f", "--follow", action="store_true", dest="follow",
                  help="Follow journal.")

(opts, args) = parser.parse_args()

try:
    # just follow journal and do nothing else
    if opts.follow:
        with Journal(color = not opts.nocolor, lines = 20) as proc:
            proc.wait()
        sys.exit(0)

    # start service while monitoring journal
    with Journal(color = not opts.nocolor):
        check_call(['systemctl', '--user', 'start'] + args)
    sys.exit(0)


except CalledProcessError as err:
    sys.exit(err.returncode)

except KeyboardInterrupt:
    print("")
    sys.exit()

except EOFError:
    print("")
    sys.exit()
