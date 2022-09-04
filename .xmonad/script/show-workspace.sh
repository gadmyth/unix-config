#!/bin/bash

delay=$1
msg=$2

nid=1
if [[ -f ~/.notification_id ]]; then
    nid=$(cat ~/.notification_id)
fi
if [[ -z $nid ]]; then
    nid=1
fi

new_nid=$(notify-send -t $delay "$msg" -p -r $nid)
echo -n ${new_nid} > ~/.notification_id

