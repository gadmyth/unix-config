function turn-on-proxy() {
    export https_proxy=127.0.0.1:8118
    export http_proxy=127.0.0.1:8118
}

function turn-off-proxy() {
    export https_proxy=
    export http_proxy=
}

function show-proxy() {
    echo "https_proxy: $https_proxy"
    echo "http_proxy: $http_proxy"
}
