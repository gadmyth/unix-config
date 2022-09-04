#!/bin/bash

delta=$1
should_notify=$2
# adjust volume
pactl set-sink-volume $(pactl get-default-sink) "$delta"
if [[ "$should_notify" == "true" ]]; then
    current_volume=$(pactl get-sink-volume $(pactl get-default-sink) | egrep -o "[0-9]+?%" | head -n 1)

    nid=1
    if [[ -f ~/.notification_id ]]; then
       nid=$(cat ~/.notification_id)
    fi
    if [[ -z $nid ]]; then
       nid=1
    fi

    new_nid=$(notify-send "${current_volume}" -t 1000 -p -r $nid)
    echo -n ${new_nid} > ~/.notification_id
fi
