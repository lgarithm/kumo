#!/bin/sh
set -e

cabal v1-install HTTP wai warp split utf8-string network-simple
