#!/bin/zsh

local st="$(hg status)"
local br="$(hg branch)"
local rev="$(hg id -n)"

__CURRENT_VCS_BRANCH="$br"
__CURRENT_VCS_REV="$rev"

if [[ ! "$(echo -n "$st\n" | grep '^\s*?.*\.orig\a*$' | wc -l)" == "0" ]]; then   __CURRENT_VCS_STATUS='conflict'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*?' | wc -l)" == "0" ]]; then             __CURRENT_VCS_STATUS='untracked'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*!' | wc -l)" == "0" ]]; then             __CURRENT_VCS_STATUS='deleted'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*[ARM]' | wc -l)" == "0" ]]; then         __CURRENT_VCS_STATUS='staged'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*I' | wc -l)" == "0" ]]; then             __CURRENT_VCS_STATUS='ignored'
elif [[ "$st" == "" ]]; then                                                      __CURRENT_VCS_STATUS='sync'
else                                                                              __CURRENT_VCS_STATUS="unknown"
fi

# Don't get specific info about remote status because it is slow in mercurial
local tbrurl="$(hg showconfig paths.default)"
if [ "$tbrurl" ]; then    __CURRENT_VCS_REMOTE_STATUS="unknown"
else                      __CURRENT_VCS_REMOTE_STATUS="none"
fi
 
