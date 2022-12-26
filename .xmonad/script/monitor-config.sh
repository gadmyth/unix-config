#!/bin/bash

# 0.9999x0.9999 scale will fix mouse flashing 

if [[ "$(uname -n)" == "new-fedora" ]]; then
    xrandr --output DP-2 --primary --auto --scale 0.9999x0.9999 --output DP-1 --auto --scale 1.5x1.5 --rotate left --right-of DP-2
elif [[ "$(uname -n)" == "old-fedora" ]]; then
    xrandr --output DP-1 --primary --auto --scale 0.7x0.7 --output VGA-1 --auto --scale 0.9999x0.9999 --right-of DP-1
fi
