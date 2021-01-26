#!/bin/sh

NET_NAME=owls_net
DOCKER_NAME=stephb9959/tip-owls-1
TIP_CONTROLLER_NAME=debfarm1-node-a.arilia.com
TIP_CONTROLLER_IP=10.3.11.1

# clean networks and create the testing network
docker network prune --force
docker network create \
  --driver=bridge \
  --subnet=172.21.0.0/16 \
  --ip-range=172.21.10.0/24 \
  --gateway=172.21.0.1 \
  $NET_NAME

#stop previously running images
docker container stop manager node1
docker container rm manager node1 --force

#create directories for logs
rm -rf docker_logs_manager
rm -rf docker_logs_node1

mkdir docker_logs_manager
mkdir docker_logs_node1

HOSTNAMES="--add-host mgr.owls.net:172.21.10.2 --add-host node1.owls.net:172.21.10.3 --add-host $TIP_CONTROLLER_NAME:$TIP_CONTROLLER_IP"

docker run  -d -p 9091:9090 --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_manager:/app_data/logs" \
            -e ERL_NODE_NAME="mgr@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput" -e ERL_NODE_TYPE="manager" \
            --ip="172.21.10.2" $HOSTNAMES \
            --name="manager" $DOCKER_NAME

docker run  -d --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_node1:/app_data/logs" \
            -e ERL_NODE_NAME="node1@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput" -e ERL_NODE_TYPE="node" \
            --ip="172.21.10.3" $HOSTNAMES \
            --name="node1" $DOCKER_NAME


