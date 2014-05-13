#!/usr/bin/env python3

from ranger.api.commands import *
import subprocess

class autojump(Command):
    """:j

    Uses autojump to set the current directory.
    """

    def execute(self):
        directory = subprocess.check_output(["autojump", "--complete", self.arg(1)])
        directory = directory.decode("utf-8", "ignore").strip()
        self.fm.execute_console("cd " + directory)
