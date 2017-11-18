set -e

cd `dirname $0`

. ./docker-build.sh

docker run --rm -it $tag
