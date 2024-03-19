#!/bin/bash

set -euo pipefail

sh_path="${0}"
root_path=$(dirname "${0}")
root_path=$(realpath "${root_path}")
data_path="${root_path}/data"

docker="docker"
container_image="docker.io/prom/prometheus:v2.51.0"
container_name="prometheus.zig_prometheus"

docker_volume_opts=""

prometheus_port=9090

usage() {
    echo "unknown command: " "$@"
    echo "Usage: ${sh_path} start|stop|restart"
}

start() {
    "${docker}" run -d --name "${container_name}" -p ${prometheus_port}:9090 "${container_image}"
    echo "Waiting for prometheus to start ..."
    until { curl -s -f -k http://localhost:${prometheus_port}/api/v1/status/runtimeinfo 2>&1; } >/dev/null; do echo "..."; sleep 2; done
    echo "Prometheus started (available on http://localhost:${prometheus_port}/)."
}

stop() {
    rm -rf "${data_path}"
    { "${docker}" rm -f "${container_name}" 2>&1; } >/dev/null
}

restart() {
    stop
    start
}


declare -A commands=(
    [main]=usage
    [start]=start
    [stop]=stop
    [restart]=restart
)
"${commands[${1:-main}]:-${commands[main]}}" "$@"