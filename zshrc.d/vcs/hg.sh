
local st="$(hg status)"

__CURRENT_VCS_REV="$(hg id -n)"
__CURRENT_VCS_BRANCH="$(hg branch)"

if [[ "$st" == "" ]]; then
    __CURRENT_VCS_STATUS="clean"
else
    __CURRENT_VCS_STATUS="changed"
fi
