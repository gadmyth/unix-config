#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#alias ls='ls --color=auto'
if [ "$SHELL" = "/bin/bash" ]; then
	PS1='[\u@\h \W]\$ '
fi
[[ -f ~/.bash_alias ]] && . ~/.bash_alias
[[ -f ~/mybash_profile ]] && . ~/mybash_profile
