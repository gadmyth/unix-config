#!/bin/bash

function toggle-panel() {
    autohide_behavior=$(xfconf-query -c xfce4-panel -p /panels/panel-$1/autohide-behavior 2>/dev/null)
    if [ "0" == "${autohide_behavior}" ]; then
        xfconf-query -c xfce4-panel -n -p /panels/panel-$1/autohide-behavior -t int -s 2
        xfconf-query -c xfce4-panel -p /panels/panel-$1/leave-opacity -s 0
    elif [ "2" == "${autohide_behavior}" ]; then
        xfconf-query -c xfce4-panel -n -p /panels/panel-$1/autohide-behavior -t int -s 0
        xfconf-query -c xfce4-panel -p /panels/panel-$1/leave-opacity -s 100
    fi
}

toggle-panel 1
toggle-panel 2
