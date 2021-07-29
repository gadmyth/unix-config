#!/bin/bash

function ipt-allow() {
    sudo iptables -I INPUT -p "$1" --dport "$2" -s "$3" -j ACCEPT
}

function ipt-allow-all() {
    sudo iptables -I INPUT -p "$1" --dport "$2" -j ACCEPT
}

function ipt-delete() {
    sudo iptables -D INPUT $1
}

function ipt-list-all() {
    sudo iptables -nL
}

function ipt-list() {
    ipt-list-all | grep $1
}

function ipt-save() {
    sudo iptables-save -f /etc/sysconfig/iptables.save
}

function ipt-restore() {
    sudo iptables-restore /etc/sysconfig/iptables.save
}
