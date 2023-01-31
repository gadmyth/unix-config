function turn-on-proxy-privoxy() {
    export https_proxy=127.0.0.1:8118
    export http_proxy=127.0.0.1:8118
}

function turn-on-proxy-socks5() {
    export https_proxy=127.0.0.1:1080
    export http_proxy=127.0.0.1:1080
}

function turn-off-proxy() {
    export https_proxy=
    export http_proxy=
}

function show-proxy() {
    echo "https_proxy: $https_proxy"
    echo "http_proxy: $http_proxy"
}
