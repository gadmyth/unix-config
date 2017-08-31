#!/bin/bash

function turn-on-mac-wifi-proxy() {
    sudo networksetup -setsecurewebproxystate 'Wi-Fi' on
    sudo networksetup -setwebproxystate 'Wi-Fi' on
}

function turn-off-mac-wifi-proxy() {
    sudo networksetup -setsecurewebproxystate 'Wi-Fi' off
    sudo networksetup -setwebproxystate 'Wi-Fi' off
}

function turn-on-mac-wifi-autoproxy() {
    sudo networksetup -setautoproxyurl 'Wi-Fi' "http://autoproxy.wanda.cn/proxy.pac"
}

function turn-off-mac-wifi-autoproxy() {
    sudo networksetup -setautoproxystate 'Wi-Fi' off
}

function mac-wifi-proxy-state() {
    echo "------ Secure Web Proxy ------"
    sudo networksetup -getsecurewebproxy 'Wi-Fi'
    echo ""
    echo "--------- Web Proxy ----------"
    sudo networksetup -getwebproxy 'Wi-Fi'
    echo ""
    echo "-------- Auto Proxy ----------"
    sudo networksetup -getautoproxyurl 'Wi-Fi'
    echo ""
}
