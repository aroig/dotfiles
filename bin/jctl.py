#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# jctl - Run journalctl with some nice colors
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


import re
import sys
import json
import datetime
import argparse

from collections import OrderedDict
from subprocess import Popen, check_call, CalledProcessError, PIPE
from threading import Thread


_color = {
    'gray'    : "\033[0;38;5;244m",
    'red'     : "\033[0;91m",
    'green'   : "\033[0;92m",
    'yellow'  : "\033[0;93m",
    'blue'    : "\033[0;94m",
    'magenta' : "\033[0;95m",
    'cyan'    : "\033[0;96m",
    'white'   : "\033[0;97m",

    'black'    : "\033[0;30m",
    'dred'     : "\033[0;31m",
    'dgreen'   : "\033[0;32m",
    'dyellow'  : "\033[0;33m",
    'dblue'    : "\033[0;34m",
    'dmagenta' : "\033[0;35m",
    'dcyan'    : "\033[0;36m",
    'dwhite'   : "\033[0;37m",

    'reset'   : "\033[0m"
}

_services = OrderedDict([
    ('systemd.*'      , 'green'),
    ('connman.*'      , 'blue'),
    ('wpa_supplicant' , 'blue'),
    ('xorg'           , 'magenta'),
    ('kernel'         , 'red'),
    ('.*'             , 'yellow'),
])

_hostnames = OrderedDict([
    ('.*'             , 'cyan'),
])

_keywords = OrderedDict([
    ('(W|w)arning'    , 'yellow'),
    ('WARNING'        , 'yellow'),
    ('(E|e)rror'      , 'red'),
    ('ERROR'          , 'red'),
    ('(F|f)ail'       , 'red'),
])

_patterns = OrderedDict([
    ('(\d+\.){3}\d+(/\d+)?'                 , 'red'),
])


class StreamWriter(Thread):
    """Gets lines from a stream and processes them"""

    def __init__(self, istream, ostream, color=False):
        super(StreamWriter, self).__init__()
        self.istream = istream
        self.ostream = ostream
        self.color = color

    def format_service(self, name):
        if name == None: return "<none>"

        for regexp, c in _services.items():
            if re.match(regexp, str(name)):
                return '%s%s%s' % (_color[c], str(name), _color['reset'])

    def format_hostname(self, name):
        if name == None: return "<none>"

        for regexp, c in _hostnames.items():
            if re.match(regexp, str(name)):
                return '%s%s%s' % (_color[c], str(name), _color['reset'])

    def format_timestamp(self, timestamp):
        if timestamp == None: return ""

        if self.color: c = _color['gray']
        else         : c = _color['reset']

        return '%s%s%s' % (c, timestamp.strftime('%d %b %H:%M:%S'), _color['reset'])

    def format_message(self, message):
        if message == None: return ""

        for regexp, c in _keywords.items():
            if re.search(regexp, str(message)):
                return '%s%s%s' % (_color[c], str(message), _color['reset'])

        for regexp, c in _patterns.items():
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
        hostname_txt  = self.format_hostname(hostname)
        timestamp_txt = self.format_timestamp(timestamp)
        message_txt   = self.format_message(message)

        data = "%s %s %s: %s\n" % (timestamp_txt, hostname_txt, service_txt, message_txt)
        self.ostream.write(data.encode())
        self.ostream.flush()

    def print_special(self, entry):
        data = "%s%s%s\n" % (_color['red'], entry, _color['reset'])
        self.ostream.write(data.encode())
        self.ostream.flush()

    def run(self):
        try:
            for line in self.istream:

                try:
                    entry = json.loads(line.decode(errors='ignore'))

                except:
                    # this will be the `-- Reboot --` thing
                    self.print_special(line.decode())

                self.print_entry(entry)

        except BrokenPipeError:
            pass

        self.istream.close()

        try:
            self.ostream.close()
        except BrokenPipeError:
            pass



class Pager:
    def __init__(self, follow=False, color=False, no_pager=False):
        if no_pager:
            self.args = ['cat']

        else:
            self.args = ['less']
            if follow: self.args.append('+F')
            if color:  self.args.append('-R')

        self.proc = None

    def __enter__(self):
        self.proc = Popen(self.args, stdin=PIPE, bufsize=-1)
        return self.proc

    def __exit__(self, type, value, traceback):
        try:
            self.proc.terminate()
        except ProcessLookupError:
            pass

        self.proc = None



class Journal:
    def __init__(self, args, ostream=sys.stdout.buffer, host=None, color=False):
        self.args = args
        self.color = color
        self.host = host

        self.proc = None
        self.stream = None
        self.ostream = ostream

    def __enter__(self):
        if self.host: jctl_cmd = ['ssh', self.host]
        else:         jctl_cmd = []

        jctl_cmd = jctl_cmd + ['journalctl', '--quiet', '--output=json'] + self.args

        self.proc = Popen(jctl_cmd, stdout=PIPE, bufsize=-1)

        self.stream = StreamWriter(istream=self.proc.stdout, ostream=self.ostream, color=self.color)
        self.stream.start()
        return self.proc

    def __exit__(self, type, value, traceback):
        try:
            self.proc.terminate()
        except ProcessLookupError:
            pass

        self.stream.join()
        self.proc = None
        self.stream = None



if __name__ == "__main__":

    parser = argparse.ArgumentParser(description='Print colored systemd journal data')
    parser.add_argument("-H", "--host", metavar='<host>', type=str, help="Host")

    args, rest = parser.parse_known_args()

    try:
        follow = ('-f' in rest) or ('--follow' in rest)
        no_pager = follow or ('--no-pager' in rest)

        if not follow:
            jctl_args = ['--since=-2d'] + rest

        with Pager(follow=follow, color=True, no_pager=no_pager) as pager:
            with Journal(jctl_args, ostream=pager.stdin, host=args.host, color=True) as proc:
                proc.wait()
                pager.wait()

        sys.exit(0)


    except CalledProcessError as err:
        sys.exit(err.returncode)

    except BrokenPipeError:
        sys.exit(0)

    except KeyboardInterrupt:
        sys.exit(0)

    except EOFError:
        sys.exit(0)
