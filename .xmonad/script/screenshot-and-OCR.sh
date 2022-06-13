#!/bin/bash

image_file=~/Pictures/$(date '+%Y-%m-%d_%H-%M-%S').png

xfce4-screenshooter --region --mouse --save ${image_file}

if [[ -f ${image_file} ]]; then
    
    ocr_output_file=/tmp/$(date '+%Y-%m-%d_%H-%M-%S')

    system_dpi=$(xrdb -query | grep dpi | awk '{print $2}')

    # sudo dnf install tesseract tesseract-langpack-chi_sim
    tesseract ${image_file} ${ocr_output_file} --dpi ${system_dpi} -l chi_sim

    emacsclient -c ${ocr_output_file}.txt
fi
