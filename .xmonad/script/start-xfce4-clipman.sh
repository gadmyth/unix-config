#!/bin/bash

[[ $((`pgrep -c xfce4-clipman`)) -eq 0 ]] && xfce4-clipman

