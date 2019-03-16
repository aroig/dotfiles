
# Scaled Robotics code
export SR_ROSDISTRO="melodic"
export SR_WORKSPACE="$HOME/work/scaled/code/workspace"
export SR_BUILDDIR="$HOME/build/scaled/home/$SR_ROSDISTRO/code/workspace/build"
export SR_ROSROOT="/var/lib/machines/ros-$SR_ROSDISTRO"

ros() {
    local distro="${1-$SR_ROSDISTRO}"
    sudo machinectl start "ros-$distro"
    sleep 4
    sudo machinectl shell --uid 1000 "ros-$distro"
}
