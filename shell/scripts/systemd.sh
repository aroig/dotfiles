
#------------------------------
# systemd utility functions
#------------------------------

sd_log() {
    systemd-cat -t "shell" echo "$1"
}
