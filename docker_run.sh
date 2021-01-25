mkdir docker_logs
docker run -it -p 9091:9090 --init --volume="$PWD/ssl:/etc/ssl/certs" --volume="$PWD/docker_logs:/app_data/logs" -e ERL_NODE_NAME="simmanager1@renegademac.arilia.com" tip-owls-1
