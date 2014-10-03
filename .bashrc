#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

#alias ls='ls --color=auto'
PS1='[\u@\h \W]\$ '
[[ -f ~/mybash_profile ]] && . ~/mybash_profile
[[ -f ~/.bash_alias ]] && . ~/.bash_alias
