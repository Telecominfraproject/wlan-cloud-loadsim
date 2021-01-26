echo "Removing docker images before build..."
docker rmi -f $(docker images -a -q)
echo "Building tip-owls-1 image..."
docker build --no-cache --tag tip-owls-1 .
IMAGE_ID=`docker images -q tip-owls-1`
docker login --username=stephb9959
docker images
docker tag $IMAGE_ID stephb9959/tip-owls-1:latest
echo "Updating dockerhub witjh the latest image..."
docker push stephb9959/tip-owls-1
