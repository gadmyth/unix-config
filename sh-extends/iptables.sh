#!/bin/bash

function ipt-allow-input() {
    sudo iptables -I INPUT -p "$1" --dport "$2" -s "$3" -j ACCEPT
}

function ipt-allow-input-all() {
    sudo iptables -I INPUT -p "$1" --dport "$2" -j ACCEPT
}

function ipt-delete-input() {
    sudo iptables -D INPUT $1
}

function ipt-list-all() {
    sudo iptables -nvL --line-numbers
}

function ipt-list-input() {
    ipt-list-input-all | grep $1
}

function ipt-save() {
    sudo iptables-save -f /etc/sysconfig/iptables.save
}

function ipt-restore() {
    sudo iptables-restore /etc/sysconfig/iptables.save
}

function ipt-restore-default() {
    sudo iptables-restore /etc/sysconfig/iptables
}
