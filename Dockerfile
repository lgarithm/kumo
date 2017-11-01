FROM haskell
RUN apt update && \
    apt install -y make && \
    cabal update && \
    cabal install http wai warp split utf8-string
ADD . /haskell/src
RUN make -C /haskell/src
ENTRYPOINT [ "/bin/bash" ]
