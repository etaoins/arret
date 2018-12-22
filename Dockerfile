FROM debian:buster
ARG CARGO_PATH=/root/.cargo/bin/cargo

RUN \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get -y install curl build-essential zlib1g-dev llvm-7 llvm-7-dev && \
  apt-get clean

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y

ADD . /root/arret
WORKDIR /root/arret

RUN $CARGO_PATH build
RUN $CARGO_PATH test
RUN $CARGO_PATH build --release

ENTRYPOINT ["/root/.cargo/bin/cargo", "run", "--release", "repl"]
