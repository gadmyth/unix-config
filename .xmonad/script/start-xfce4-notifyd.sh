#!/bin/bash

[[ $((`pgrep -c xfce4-notifyd`)) -eq 0 ]] && /usr/lib64/xfce4/notifyd/xfce4-notifyd
