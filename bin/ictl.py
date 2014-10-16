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

import sys

from subprocess import check_call, CalledProcessError
from jctl import Journal


usage = """usage: %prog <units>...

Run systemctl --user start, and prints the journal log while units are starting.
"""
parser = OptionParser(usage=usage)

parser.add_option("-c", "--no-color", action="store_true", dest="nocolor",
                  help="Do not colorize output.")


(opts, args) = parser.parse_args()

try:
    # start service while monitoring journal
    with Journal(args = ['-f', '-n0'], ostream = sys.stdout.buffer, color=not opts.nocolor):
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
