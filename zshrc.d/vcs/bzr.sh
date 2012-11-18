
local st="$(bzr status --short --no-classify)"
local br="$(bzr nick)"
local rev="$(bzr revno)"

__CURRENT_VCS_BRANCH="$br"
__CURRENT_VCS_REV="$rev"

if [[ ! "$(echo -n "$st\n" | grep '^\s*\?.*\.orig\a*$' | wc -l)" == "0" ]]; then  __CURRENT_VCS_STATUS='conflict'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*?' | wc -l)" == "0" ]]; then             __CURRENT_VCS_STATUS='untracked'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*D' | wc -l)" == "0" ]]; then             __CURRENT_VCS_STATUS='deleted'
elif [[ ! "$(echo -n "$st\n" | grep '^\s*[AM]' | wc -l)" == "0" ]]; then          __CURRENT_VCS_STATUS='staged'
elif [[ "$st" == "" ]]; then                                                      __CURRENT_VCS_STATUS='sync'
else                                                                              __CURRENT_VCS_STATUS="unknown"
fi

# Don't get specific info about remote status because it is slow in mercurial
local tbrurl="$(bzr config parent_location)"
if [ "$tbrurl" ]; then    __CURRENT_VCS_REMOTE_STATUS="unknown"
else                      __CURRENT_VCS_REMOTE_STATUS="none"
fi
 
