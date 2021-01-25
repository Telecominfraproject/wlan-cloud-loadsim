docker build --no-cache --tag tip-owls-manager --file Dockerfile-manager .
docker build --no-cache --tag tip-owls-node    --file Dockerfile-node .
docker build --no-cache --tag tip-owls-monitor --file Dockerfile-monitor .
