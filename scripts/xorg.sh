#!/bin/bash

# currentvt=$(fgconsole)
currentvt=2
display=$1
exec /usr/bin/xorg-launch-helper $display -nolisten tcp -noreset vt$currentvt
