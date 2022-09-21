#!/bin/bash


XMONAD_DEVICE="new-fedora"

if [[ "$XMONAD_DEVICE" == "new-fedora" ]]; then
    xrandr --output DP-2 --primary --auto --output DP-1 --auto --scale 1.5x1.5 --rotate left --right-of DP-2
fi