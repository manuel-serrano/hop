FROM debian:jessie

RUN apt-get update && apt-get install -y \
    bash \
    make \
    wget \
    git \
    gcc \
    autoconf \
    automake \
    curl \
    pkg-config \
    libtool \
    libgmp-dev libgmp3-dev libgmp10 \
    libssl1.0.0 libssl-dev \
    libsqlite3-0 libsqlite3-dev \
    libasound2 libasound2-dev \
    libpulse0 libpulse-dev \
    libflac8 libflac-dev \
    libmpg123-0 libmpg123-dev \
    libavahi-core7 libavahi-core-dev libavahi-common-dev libavahi-common3 \
    libavahi-client3 libavahi-client-dev

WORKDIR /tmp

RUN wget http://redrock.inria.fr:8888/home/serrano/prgm/distrib/bigloo4.3b.tar.gz \
    && tar -xzf bigloo*.tar.gz && cd bigloo* \
    && ./configure && make -j$(nproc) && make install

RUN wget http://redrock.inria.fr:8888/home/serrano/prgm/distrib/hop-3.2.0-pre1.tar.gz \
    && tar -xzf hop* && cd hop* \
    && ./configure && make -j$(nproc) && make install \
    && cd ..; rm -rf hop-3.2.0/hopc

RUN git clone https://github.com/colinvidal/hiphop.git \
    && (cd hiphop; git checkout master) \
    && mv hiphop /usr/local/lib/hop/3.2.0/node_modules/

RUN useradd --no-log-init -d /home/hop -s /bin/bash hop

COPY hoprc.hop /home/hop/.config/hop/hoprc.hop

RUN rm -rf /tmp/*

USER hop
ENV HOME /home/hop

STOPSIGNAL SIGINT

ENTRYPOINT ["/bin/bash"]
ENTRYPOINT ["/usr/local/bin/hop"]
