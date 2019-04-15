
# Scaled Robotics code
export SR_ROSDISTRO="melodic"
export SR_WORKSPACE="$HOME/work/scaled/code/workspace"
export SR_DATA="$HOME/data/scaled"
export SR_BUILDDIR="$HOME/build/scaled/home/$SR_ROSDISTRO/code/workspace/build"
export SR_ROSROOT="/var/lib/machines/$SR_ROSNAME"
export SR_ROSMACHINE="ros-$SR_ROSDISTRO"
export SR_ROSUSER="abdo"

sr_ros_path() {
    local reldir=$(realpath --relative-to "$SR_WORKSPACE" "$1")
    if [ "${reldir##..}" = "${reldir}" ]; then
        local guestdir="/home/$SR_ROSUSER/code/workspace/$reldir"
        printf "%s" "$guestdir"
        return
    fi

    reldir=$(realpath --relative-to "$SR_DATA" "$1")
    if [ "${reldir##..}" = "${reldir}" ]; then
        local guestdir="/data/$reldir"
        printf "%s" "$guestdir"
        return
    fi

    printf "%s" "$1"
}

sr_ros_run() {
    local guestdir=$(sr_ros_path .)
    sudo systemd-run                      \
         --pty --wait --collect           \
         --machine "$SR_ROSMACHINE"       \
         --uid "$SR_ROSUSER"              \
         --working-directory "$guestdir"  \
         /bin/bash -c "$*"
}

sr_ros_start() {
    if ! machinectl show "$SR_ROSMACHINE" 2>/dev/null | grep -q State=running; then
        sudo machinectl start "$SR_ROSMACHINE"
        sleep 4
    fi
}

sr_ros_shell() {
    sudo machinectl shell           \
        --uid "$SR_ROSUSER"         \
        "$SR_ROSMACHINE"
}

sr_catkin_package() {
    local d=$(pwd)
    while [ -n "$d" ]; do
        if [ -e "$d/package.xml" ]; then
            printf "%s" "$(basename "$d")"
            return
        else
            d="${d%/*}"
        fi
    done
    return 1
}

ros() {
    sr_ros_start
    sr_ros_shell
}

rosh() {
    sr_ros_run "bash --login"
}

rcd() {
    local repo="$1"
    local gitdir=$(find "$SR_WORKSPACE/src" -path "*/$repo/.git" | head -1)
    cd "${gitdir%.git}"
}

rr() {
    sr_ros_run "source ~/.bashrc; rosrun $*"
}

rrh() {
    local pkg=$(sr_catkin_package)
    rr "$pkg" "$@"
}

ck() {
    sr_ros_run "source ~/.bashrc; catkin $*"
}

ckb() {
    ck build --this
}

