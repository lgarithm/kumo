set -e

cd `dirname $0`
cp -r ../../Kumo .
tag=kumo-example
docker build -t $tag . 

docker-compose down
docker-compose up
docker-compose down
