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
        ping -c 2 172.16.16.1 | grep -E "PING|from"
        echo ""
        sleep 3
    done
}

# low memory version
function nb-slow-check-lm() {
    watch -n 3 "echo `date \"+%Y-%m-%d %H:%M:%S\"`; ping -c 2 172.16.16.1 | grep -E \"PING|from\""
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

function nb2bin() {
    cd /opt/nebula/bin
}

function nb2config() {
    cd /opt/nebula/config
}

function nb2cert() {
    cd /opt/nebula/cert
}

function nb-install-new-version() {
    # 参数不能为空
    [[ -z "$1" ]] && echo "Usage: nb-install-new-version <version>" && return 1
    
    local nb_version=$1
    local install_dir=/opt/nebula/bin/${nb_version}
    mkdir -p ${install_dir}
    cd ${install_dir}
    
    # wget 输出到 stdout 然后解压
    echo "download and untar..."
    if wget -qO- https://github.com/slackhq/nebula/releases/download/v${nb_version}/nebula-linux-amd64.tar.gz | tar -zxvf -; then
        echo "✅ Nebula ${nb_version} installed successfully to ${install_dir}"
        ${install_dir}/nebula -version
    else
        echo "❌ Failed to install Nebula ${nb_version}"
        return 1
    fi
}
