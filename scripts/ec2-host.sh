#!/usr/bin/bash

action="$1"
host="$2"

AWS_ARGS="--output text"

# load config
source "$HOME/.aws/ec2/$host.conf"

# TODO: get instance id from keypair
# TODO: get ami id from ami name

case "$action" in

    launch)
        # TODO: Get image id
        # TODO: do not launch if alredy existing instance
        aws ec2 run-instances $AWS_ARGS --image-ids "$IMAGE" --count 1 --instance-type "$ITYPE" --key-name "$KEYPAIR" --security-group-ids "$SGROUP" --subnet-id "$SUBNET"
        # TODO: store instanceid in config
    ;;

    terminate)
        # TODO: remove instance id from config
        ;;

    snapshot)
        # TODO
        ;;
    
    start)
        aws ec2 start-instances $AWS_ARGS --instance-ids "$INSTANCE"
        aws ec2 wait instance-running $AWS_ARGS --instance-ids "$INSTANCE"
        ;;

    stop)
        aws ec2 stop-instances $AWS_ARGS --instance-ids "$INSTANCE"
        aws ec2 wait instance-stopped $AWS_ARGS  --instance-ids "$INSTANCE"
        ;;

    attach)
        # TODO: make it idempotent
        aws ec2 attach-volume $AWS_ARGS --volume-id "$VOLUME" --instance-id "$INSTANCE" --device "$DEVICE"
        aws ec2 wait volume-in-use $AWS_ARGS --volume-ids "$VOLUME"
        ;;

    detach)
        # TODO: make it idempotent
        aws ec2 attach-volume $AWS_ARGS --volume-id "$VOLUME" --instance-id "$INSTANCE" --device "$DEVICE"
        aws ec2 wait volume-available $AWS_ARGS --volume-ids "$VOLUME"
        ;;

    mount)       
        mount_scr="
            if sudo cryptsetup status $LUKS | grep -qs inactive > /dev/null; then
                sudo cryptsetup --key-file=- open --type luks $DEVICE $LUKS
            fi
            sudo systemctl start $MOUNT
        "
        echo -n "$VOLKEY" | ssh "$host" "$mount_scr"       
        ;;

    umount)
        umount_scr="
            sudo systemctl stop $MOUNT
            sudo cryptsetup close --type luks $LUKS
        "       
        ssh "$host" "$umount_scr"
        ;;

esac
