--- Suits for awful

local layout = {}

layout.tile = require("abdo.layout.tile")
layout.fair = require("abdo.layout.fair")
layout.spiral = require("abdo.layout.spiral")
layout.dwindle = layout.spiral.dwindle

return layout
