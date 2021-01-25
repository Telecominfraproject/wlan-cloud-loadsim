docker build --no-cache --tag tip-owls-manager  -f Dockerfile-manager
docker build --no-cache --tag tip-owls-node     -f Dockerfile-node
docker build --no-cache --tag tip-owls-monitor  -f Dockerfile-monitor
