#!/bin/bash

function default-interface() {
    os="$(uname -s)"
    case "${os}" in
        Linux*)  route -n | grep ^0.0.0.0 | awk '{print $NF}';;
        Darwin*) route -n get default | grep interface | awk '{print $2}';;
        *) ""
    esac
}

function current-ip() {
    ifconfig $(default-interface | head -n 1) | grep "inet " | grep -v 127.0.0.1 | awk '{print $2}'
}

function current-git-branch() {
    local branch=$(test -x "`command -v git`" && git -C . rev-parse 2>/dev/null && br=`git branch | grep "\*"` && echo ${br/* /})
    test ! -z $branch && test ! -z "$(git status --untracked-files=no --porcelain)" && branch="${branch}*"
    test ! -z $branch && branch="[$branch]"
    echo -n "$branch"
}

function prompt {
	case $TERM in xterm*|rxvt*)
		TITLEBAR='';;
	*)
		TITLEBAR='';;
	esac

    if [ "$SHELL" = "/bin/bash" ]; then
        # change the PS1
        PS1="\e[0;31m\e[47m[\!|\u@\$(current-ip)][\$(date +%k:%M:%S)]\e[m\e[47m[\${EDITOR}|\${http_proxy}]\e[m\e[0;31m\e[47m[\w]\$(current-git-branch)\e[m\n"
        # change the PS2
        PS2="continue-->"
        # change PS4
        PS4='$0.$LINENO+ '
    fi
}

prompt
