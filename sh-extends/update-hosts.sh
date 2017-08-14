#!/bin/bash

function update-hosts-frome-zip() {
    unzip $* -d /tmp
    sudo cp /tmp/hosts /etc/hosts
    restart-network
}