#!/bin/bash

# -- nebula tool: https://github.com/slackhq/nebula

function nb-start() {
    sudo /opt/nebula/bin/nebula -config /opt/nebula/config/config.yml > /var/log/nebula.log &
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
        ping -c 1 -w 2 172.16.16.1 | egrep "PING|from"
        echo ""
        sleep 3
    done
}

function nb-log() {
    tail -fn 500 /var/log/nebula.log
}

function nb-change-version() {
    if [ -d /opt/nebula/bin/$1 ]; then
        rm -rf /opt/nebula/bin/nebula
        ln -s /opt/nebula/bin/$1/nebula /opt/nebula/bin/nebula

        rm -rf /opt/nebula/bin/nebula-cert
        ln -s /opt/nebula/bin/$1/nebula-cert /opt/nebula/bin/nebula-cert
    else
        echo "/opt/nebula/bin/$1 does not exist"
    fi
}

function nb-ls-version() {
    ls /opt/nebula/bin/ | grep -v nebula
}

function nb-version() {
    /opt/nebula/bin/nebula -version
}

function nb-bin() {
    cd /opt/nebula/bin
}

function nb-config() {
    cd /opt/nebula/config
}
