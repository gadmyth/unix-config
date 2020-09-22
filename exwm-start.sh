#!/bin/bash

# copied from /etc/X11/xinit/xinitrc-common
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap

sysresources=/etc/X11/Xresources
sysmodmap=/etc/X11/Xmodmap

# merge in defaults
[ -r "$sysresources" ] && xrdb -nocpp -merge "$sysresources"
[ -r "$userresources" ] && xrdb -merge "$userresources"

# start the initial applications
yong -d

# start exwm
emacs
