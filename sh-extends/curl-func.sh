#!/bin/bash

function curl-download() {
   curl -L -C - -O $*
}

function curl-download-with-privoxy() {
   curl -L -C - -x "http://localhost:8118" -O $*
}

function curl-download-with-socks5() {
   curl -L -C - -x "socks5://localhost:1080" -O $*
}
