#!/bin/bash

# -- nebula tool: https://github.com/slackhq/nebula

export PATH=$PATH:/opt/nebula/bin 

function nb-start() {
    sudo PATH=$PATH:/opt/nebula/bin nohup nebula -config /opt/nebula/config/config.yml > /opt/nebula/nebula.log &
}

function nb-stop() {
    sudo pkill nebula
}

function nb-restart() {
    nb-stop; nb-start; nb-log
}

function nb-check() {
    ping 172.16.16.1
}

function nb-slow-check() {
    while true; do
        cecho green `date "+%Y-%m-%d %H:%M:%S"`
        ping -c 1 172.16.16.1 | egrep "PING|from"
        echo ""
        sleep 5
    done
}

function nb-log() {
    tail -fn 500 /opt/nebula/nebula.log
}

