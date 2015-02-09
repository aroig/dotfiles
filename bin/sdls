#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# cgls - A colored wrapper for systemd-cgls
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

from optparse import OptionParser

import os
import re
import sys
import subprocess

_color = {
    'red'     : "\033[0;31m",
    'green'   : "\033[0;32m",
    'yellow'  : "\033[0;33m",
    'blue'    : "\033[0;34m",
    'magenta' : "\033[0;35m",
    'cyan'    : "\033[0;36m",
    'white'   : "\033[0;37m",
    'reset'   : "\033[0m",
}

_color_rev = {
    '31': "red",
    '32': "green",
    '33': "yellow",
    '34': "blue",
    '35': "magenta",
    '36': "cyan",
    '37': "white"
}


def unit_regexps(pat):
    regexp = []

    # load SYSTEMD_COLORS from the environment
    for unit, a, b in re.findall('=\*\.([a-zA-Z]*)=(\d+);(\d+):', os.getenv('SYSTEMD_COLORS')):
        if len(unit.strip()) > 0:
            regexp.append((re.compile(pat % '[a-zA-Z\-][^ \n\t]*\.%s' % unit.strip(), flags=re.MULTILINE), _color_rev[b]))

    return regexp


def state_regexps():
    keywords = {
        'green'  : ['loaded', 'active', 'running', 'mounted', 'exited'],
        'yellow' : ['listening', 'waiting'],
        'red'    : ['failed']
    }

    regexp = []
    for col in keywords.keys():
        for kw in keywords[col]:
            regexp.append((re.compile('%s' % kw, flags=re.MULTILINE), col))

    return regexp


def cgls(args):
    regexp = [(re.compile('\d+\s+.*$', flags=re.MULTILINE), 'magenta')]
    regexp = regexp + unit_regexps('%s$')

    text = subprocess.check_output(['systemd-cgls', '--no-pager', '--full'] + args).decode('utf-8')
    for r, c in regexp:
        text = r.sub('%s\g<0>%s' % (_color[c], _color['reset']), text)

    print(text)



def unitls(args, unit_types):
    regexp = []
    regexp = regexp + unit_regexps('^%s')
    regexp = regexp + state_regexps()

    text = subprocess.check_output(['systemctl', '--no-pager', 'list-units', '--type=%s' % ','.join(unit_types)] + args).decode('utf-8')
    for r, c in regexp:
        text = r.sub('%s\g<0>%s' % (_color[c], _color['reset']), text)

    print(text)



usage = """usage: %prog [options]
"""
parser = OptionParser(usage=usage)

(opts, args) = parser.parse_args()

cmd = args[0]
args = args[1:]

try:
    if cmd == 'cgroups':
        cgls(args)

    elif cmd == 'units':
        unit_types=['service', 'target', 'mount', 'automount', 'path', 'socket', 'busname', 'snapshot', 'swap', 'timer', 'slice', 'scope']
        unitls(args, unit_types)


except KeyboardInterrupt:
    print("")
    sys.exit()

except EOFError:
    print("")
    sys.exit()
