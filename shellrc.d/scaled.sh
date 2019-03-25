
# Scaled Robotics code
export SR_ROSDISTRO="melodic"
export SR_WORKSPACE="$HOME/work/scaled/code/workspace"
export SR_BUILDDIR="$HOME/build/scaled/home/$SR_ROSDISTRO/code/workspace/build"
export SR_ROSROOT="/var/lib/machines/ros-$SR_ROSDISTRO"

ros() {
    local distro="${1-$SR_ROSDISTRO}"
    local machine="ros-$distro"
    if ! machinectl show $machine 2>/dev/null | grep -q State=running; then
        sudo machinectl start "$machine"
        sleep 4
    fi
    sudo machinectl shell --uid 1000 "$machine"
}

rcd() {
    local repo="$1"
    local gitdir=$(find "$SR_WORKSPACE/src" -path "*/$repo/.git" | head -1)
    cd "${gitdir%.git}"
}

