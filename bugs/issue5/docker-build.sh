set -e

cd `dirname $0`

tag=kumo-debug-issue5
docker build -t $tag .
