#!/bin/bash

if [[ "${WATCH_XSCREENSAVER}" == "1" ]]; then
    xscreensaver-command -watch | while read state; do
        case "$state" in
            "LOCK"*)
                echo "The screen is locked..."
                ;;
            "UNBLANK"*)
                echo "The screen is unlocked, set screensaver timeout: $1, cycle: $2..."
                xset s $1 $2
                ;;
        esac
    done
fi
