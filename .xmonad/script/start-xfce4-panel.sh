#!/bin/bash

# restart xfce4-panel
# xfce4-panel -q; xfce4-panel -d

# don't restart xfce4-panel
[[ $((`pgrep -c xfce4-panel`)) -eq 0 ]] && xfce4-panel -d
