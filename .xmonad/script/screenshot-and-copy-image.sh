#!/bin/bash

xfce4-screenshooter --region --mouse --clipboard | optipng -strip all -o7 - | xclip -selection clipboard -t image/png
