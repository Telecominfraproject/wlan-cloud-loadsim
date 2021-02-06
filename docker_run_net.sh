#!/bin/sh

HUBNAME=tip-tip-wlan-cloud-loadsim.jfrog.io
IMAGE_NAME=tip-owls-1
DOCKER_NAME=$HUBNAME/$IMAGE_NAME

NET_NAME=owls_net
# You must set this to the resolvable name of your TIP controller
TIP_CONTROLLER_NAME=debfarm1-node-a.arilia.com
# You must set this to the IP address of your TIP controller
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

#
# A simulation file is used to describe how a simulation should run. Here is the content...
#
# simulation:
#   name: sim1
#   ca:
#     name: tip1
#     cert: tipcert.pem   (this file should be in the $PWD/ssl dir)
#     key: tipkey.pem     (this file should be in the $PWD/ssl dir)
#     password: mypassword
#   server: (should be the same name as TIP_CONTROLLER_NAME
#   port: 6643
#   devices: 10
#

docker run  -d -p 9091:9090 --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_manager:/app_data/logs" \
            --volume="$PWD/scripts:/scripts" \
            -e ERL_NODE_NAME="mgr@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput" -e ERL_NODE_TYPE="manager" -e TIP_AUTH="2" -e SIM_SCRIPT="sim1.yaml" \
            --ip="172.21.10.2" $HOSTNAMES \
            --name="manager" $DOCKER_NAME

docker run  -d --init \
            --network=owls_net \
            --volume="$PWD/ssl:/etc/ssl/certs" \
            --volume="$PWD/docker_logs_node1:/app_data/logs" \
            -e ERL_NODE_NAME="node1@mgr.owls.net" -e ERL_OPTIONS="-noshell -noinput" -e ERL_NODE_TYPE="node" -e TIP_AUTH="2" \
            --ip="172.21.10.3" $HOSTNAMES \
            --name="node1" $DOCKER_NAME


