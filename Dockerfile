FROM darinmorrison/haskell:latest
MAINTAINER Ramith Jayatilleka <ramith.jayatilleka@gmail.com>
RUN apt-get update
RUN cabal update && cabal install cabal-install
