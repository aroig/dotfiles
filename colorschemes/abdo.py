# Copyright (C) 2009-2013  Roman Zimbelmann <hut@lavabit.com>
# This software is distributed under the terms of the GNU GPL version 3.

from ranger.gui.color import *
from ranger.colorschemes.default import Default

class Scheme(Default):
    progress_bar_color = blue

    def __init__(self):
        Default.__init__(self)
        # TODO: parse LS_COLORS


    def use(self, context):
        fg, bg, attr = Default.use(self, context)


        return fg, bg, attr
