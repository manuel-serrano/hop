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

RUN wget ftp://ftp-sop.inria.fr/indes/fp/Bigloo/bigloo-latest.tar.gz \
    && tar -xzf bigloo*.tar.gz && cd bigloo*/ \
    && ./configure && make -j$(nproc) && make install

RUN wget ftp://ftp-sop.inria.fr/indes/fp/Hop/hop-3.4.0.tar.gz \
    && tar -xzf hop* && cd hop*/ \
    && ./configure --license=academic && make -j$(nproc) && make install

RUN useradd --no-log-init -d /home/hop -s /bin/bash hop

RUN rm -rf /tmp/*

RUN mkdir -p /home/hop/.config/hop && \
    echo "(add-user! \"anonymous\" :directories '* :services '*)" \
      > /home/hop/.config/hop/hoprc.hop && \
    chown -R hop /home/hop

USER hop
ENV HOME /home/hop

STOPSIGNAL SIGINT

ENTRYPOINT ["/bin/bash"]
ENTRYPOINT ["/usr/local/bin/hop"]
