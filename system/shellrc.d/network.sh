
# firewall
fws() {
    sudo iptables -L firewall
    echo ""
    sudo iptables -n -L sshguard
}

fwban() {
    sudo iptables -A sshguard -s "$1" -j DROP
}

fwunban() {
    sudo iptables -D sshguard -s "$1" -j DROP
}

# network
gateway() {
    host `ip route list 0/0 | awk '{print $3}'` | awk '{print $5}'
}

