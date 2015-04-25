#!/usr/bin/bash

ACTION="$1"
HOST="$2"

AWS_ARGS="--output text"

if [ -z "$ACTION" ] || [ -z "$HOST" ]; then
    echo "action or host is empty"
    exit 1
fi

    

# load config
source "$HOME/.aws/ec2/$HOST.conf"

# NOTE: those two may be slow
# TODO: get instance id from keypair? 
# TODO: get ami id from ami name?


instance_is_running() {
    raw="$(aws ec2 describe-instances --output text --instance-ids "$INSTANCE" --filter 'Name=instance-state-name,Values=running')"
    [ "$raw" ]    
}

instance_is_stopped() {
    raw="$(aws ec2 describe-instances --output text --instance-ids "$INSTANCE" --filter 'Name=instance-state-name,Values=stopped')"
    [ "$raw" ]    
}

instance_start() {    
    if instance_is_stopped; then
        aws ec2 start-instances $AWS_ARGS --instance-ids "$INSTANCE"
        aws ec2 wait instance-running $AWS_ARGS --instance-ids "$INSTANCE"
    fi
}

instance_stop() {    
    if instance_is_running; then
        aws ec2 stop-instances $AWS_ARGS --instance-ids "$INSTANCE"
        aws ec2 wait instance-stopped $AWS_ARGS  --instance-ids "$INSTANCE"
    fi
}



volume_available() {
    raw="$(aws ec2 describe-volumes --output text --volume-ids "$VOLUME" --filter 'Name=status,Values=available')"
    [ "$raw" ]    
}

volume_attached() {
    local vol="$1"
    raw="$(aws ec2 describe-volumes --output text --volume-ids "$VOLUME" --filter 'Name=status,Values=in-use')"
    [ "$raw" ]
}

volume_attach() {
    if volume_available; then
        aws ec2 attach-volume $AWS_ARGS --volume-id "$VOLUME" --instance-id "$INSTANCE" --device "$DEVICE"
        aws ec2 wait volume-in-use $AWS_ARGS --volume-ids "$VOLUME"
    fi
}

volume_detach() {
    if volume_attached; then
        aws ec2 detach-volume $AWS_ARGS --volume-id "$VOLUME"
        aws ec2 wait volume-available $AWS_ARGS --volume-ids "$VOLUME"
    fi
}



host_online() {
    local host="$1"
    ping -q -c 1 "$host" 2>&1 > /dev/null
}

volume_mount() {
    mount_scr="
        if sudo cryptsetup status $LUKS | grep -qs inactive > /dev/null; then
            sudo cryptsetup --key-file=- open --type luks $DEVICE $LUKS
        fi
        sudo systemctl start $MOUNT
    "
    if host_online "$HOST"; then
        echo -n "$VOLKEY" | ssh "$HOST" "$mount_scr"
    fi
}

volume_umount() {
    # TODO: check this, it fails on umount
    umount_scr="
        sudo systemctl stop $MOUNT
        if sudo cryptsetup status $LUKS | grep -qs active > /dev/null; then
            sudo cryptsetup close --type luks $LUKS
        fi
    "
    if host_online "$HOST"; then
        ssh "$HOST" "$umount_scr"
    fi
}



case "$ACTION" in
    launch)
        # TODO: Get image id
        # TODO: do not launch if alredy existing instance
        # TODO: store instance ID in config file
        aws ec2 run-instances $AWS_ARGS --image-ids "$IMAGE" --count 1 --instance-type "$ITYPE" --key-name "$KEYPAIR" --security-group-ids "$SGROUP" --subnet-id "$SUBNET"
        ;;

    terminate)
        # TODO: remove instance id from config
        ;;

    snapshot)
        # TODO: do snapshot and store new snapshot id in config file
        ;;
    
    start)
        instance_start
        ;;

    stop)
        volume_umount
        volume_detach
        instance_stop
        ;;

    attach)
        instance_start
        volume_attach
        ;;

    detach)
        volume_umount
        volume_detach
        ;;

    mount)
        instance_start
        volume_attach
        volume_mount
        ;;

    umount)        
        volume_umount
        ;;

    status)
        aws ec2 describe-instances --instance-id "$INSTANCE"
        ;;
esac
