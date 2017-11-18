FROM ubuntu:zesty

ADD apt/sources.list.ustc /etc/apt/sources.list
ADD .cabal /root/.cabal
RUN apt update && \
    apt install -y curl make haskell-platform && \
    cabal update && \
    cabal install http wai warp split utf8-string
RUN apt install -y strace man
ADD Kumo /haskell/src/Kumo
ADD *.hs /haskell/src/
ADD Makefile /haskell/src/
RUN make -C /haskell/src

ENTRYPOINT [ "/haskell/src/main" ]
