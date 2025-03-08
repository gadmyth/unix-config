#!/bin/bash

image_file="$(mktmp).png"

xfce4-screenshooter --region --mouse --save ${image_file}

if [[ -f ${image_file} ]]; then
    system_dpi=$(xrdb -query | grep dpi | awk '{print $2}')

    # sudo dnf install tesseract tesseract-langpack-chi_sim
    echo "$(tesseract ${image_file} - --dpi ${system_dpi} -l chi_sim)" | xsel --clipboard
fi

rm ${image_file}
