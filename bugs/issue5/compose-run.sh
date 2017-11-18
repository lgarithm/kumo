set -e

cd `dirname $0`

. ./docker-build.sh

# docker-compose down
docker-compose up
