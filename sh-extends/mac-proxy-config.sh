#!/bin/bash

function mac-wifi-proxy-on() {
    sudo networksetup -setsecurewebproxystate 'Wi-Fi' on
    sudo networksetup -setwebproxystate 'Wi-Fi' on
}

function mac-wifi-proxy-off() {
    sudo networksetup -setsecurewebproxystate 'Wi-Fi' off
    sudo networksetup -setwebproxystate 'Wi-Fi' off
}

function mac-wifi-autoproxy-on() {
    sudo networksetup -setautoproxyurl 'Wi-Fi' "http://autoproxy.wanda.cn/proxy.pac"
}

function mac-wifi-autoproxy-off() {
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