FROM debian:stretch
ARG CARGO_PATH=/root/.cargo/bin/cargo

RUN \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get -y install curl build-essential zlib1g-dev

RUN curl https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN echo "deb http://apt.llvm.org/stretch/ llvm-toolchain-stretch-7 main" >> /etc/apt/sources.list

RUN \
  apt-get update && \
  apt-get -y install llvm-7 llvm-7-dev && \
  apt-get clean

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly

ADD . /root/arret
WORKDIR /root/arret

RUN $CARGO_PATH build
RUN $CARGO_PATH test
RUN $CARGO_PATH build --release

ENTRYPOINT ["/root/.cargo/bin/cargo", "run", "--release"]
