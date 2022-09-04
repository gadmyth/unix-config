#!/bin/bash

should_notify=$1
# toggle volume
pactl set-sink-mute $(pactl get-default-sink) toggle
if [[ "$should_notify" == "true" ]]; then
    mute_status=$(pactl get-sink-mute $(pactl get-default-sink) | egrep -o "yes|no")
    msg=""
    if [[ "$mute_status" == "yes" ]]; then
        msg="Muted"
    else
        msg="Sound"
    fi

    nid=1
    if [[ -f ~/.notification_id ]]; then
       nid=$(cat ~/.notification_id)
    fi
    if [[ -z $nid ]]; then
       nid=1
    fi

    new_nid=$(notify-send "${msg}" -t 1000 -p -r $nid)
    echo -n ${new_nid} > ~/.notification_id
fi
