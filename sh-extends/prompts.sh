#!/bin/bash

function prompt {
	case $TERM in xterm*|rxvt*)
		TITLEBAR='';;
	*)
		TITLEBAR='';;
	esac

    if [ "$SHELL" = "/bin/bash" ]; then
        # change the PS1
        # PS1="\e[0;31m\e[47m[\!|\u@\h][\$(totalsize.sh)][\w]\e[m\n"
        local ip=$(ifconfig $(route -n | grep ^0.0.0.0 | awk '{print $NF}') | grep inet | grep -v inet6 | awk '{print $2}')
        PS1="\e[0;31m\e[47m[\!|\u@$ip][\$(date +%k:%M:%S)][\w]\e[m\n"
        # change the PS2
        PS2="continue-->"

        # change PS4
        PS4='$0.$LINENO+ '
    fi
}

prompt
