#!/usr/bin/bash

hostname="$1"
macadress=`cat /etc/ethers | grep '^[^# ].*' | grep "$hostname" | cut -d' ' -f1`

if [ "$macadress" ]; then
    echo "$hostname: $macadress"
    wol "$macadress"
else
    echo "Can't find mac for host '$hostname'"
    exit 1
fi
