#!/bin/bash



if [[ "$(uname -n)" == "new-fedora" ]]; then
    xrandr --output DP-2 --primary --auto --output DP-1 --auto --scale 1.5x1.5 --rotate left --right-of DP-2
elif [[ "$(uname -n)" == "old-fedora" ]]; then
    xrandr --output VGA-1 --primary --auto --output DP-1 --auto --scale 0.7x0.7 --left-of VGA-1
fi
