FROM ubuntu:zesty

ADD sources.list /etc/apt/
ADD config /root/.cabal/
RUN apt update && \
    apt install -y curl make haskell-platform && \
    cabal update && \
    cabal install http wai warp split utf8-string
RUN apt install -y strace
ADD debug.hs /haskell/src/
RUN cd /haskell/src && ghc -O debug.hs

ENTRYPOINT [ "/haskell/src/debug" ]
