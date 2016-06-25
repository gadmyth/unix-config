#!/bin/bash

# for tty color
cecho() {
	while [[ $# > 1 ]]; do
		case $1 in
			red)	echo -n "$(tput setaf 1)";;
			green)	echo -n "$(tput setaf 2)";;
			blue)	echo -n "$(tput setaf 3)";;
			purple)	echo -n "$(tput setaf 4)";;
			cyan)	echo -n "$(tput setaf 5)";;
			grey)	echo -n "$(tput setaf 6)";;
			white)	echo -n "$(tput setaf 7)";;
			bold)	echo -n "$(tput bold)";;
			*) 	break;;
		esac
		shift
	done
	echo "$*$(tput sgr0)"
}

message_action() {
	cecho blue $*
}

message_status() {
	cecho green $*;
}

error() {
	cecho red $*
}
