FROM sestrella/haskell:7.8

RUN apt-get update
RUN apt-get install -y libghc-zlib-dev

ADD corasick-park.cabal /corasick-park/
WORKDIR /corasick-park

RUN cabal update
RUN cabal install --only-dependencies --enable-tests

ADD . /corasick-park
