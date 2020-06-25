#!/bin/bash

function prompt {
	case $TERM in xterm*|rxvt*)
		TITLEBAR='';;
	*)
		TITLEBAR='';;
	esac

    if [ "$SHELL" = "/bin/bash" ]; then
        # change the PS1
        local ip=$(ifconfig $(route -n get default | grep interface | awk '{print $2}') | grep "inet " | grep -v 127.0.0.1 | awk '{print $2}')
        PS1="\e[0;31m\e[47m[\!|\u@$ip][\$(date +%k:%M:%S)][\w]\e[m\n"
        # change the PS2
        PS2="continue-->"
        # change PS4
        PS4='$0.$LINENO+ '
    fi
}

prompt
