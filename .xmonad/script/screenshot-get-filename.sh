#!/bin/bash

image_file=~/Pictures/$(date '+%Y-%m-%d_%H-%M-%S').png

xfce4-screenshooter --region --mouse --save ${image_file}

if [[ -f ${image_file} ]]; then
    echo -n ${image_file} | xclip -selection c
fi
