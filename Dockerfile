FROM debian:jessie
ARG CARGO_PATH=/root/.cargo/bin/cargo
ENV LLVM_SYS_60_PREFIX /usr/lib/llvm-6.0

RUN \
  apt-get update && \
  apt-get -y upgrade && \
  apt-get -y install curl build-essential zlib1g-dev

RUN curl https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN echo "deb http://apt.llvm.org/jessie/ llvm-toolchain-jessie-6.0 main" >> /etc/apt/sources.list

RUN \
  apt-get update && \
  apt-get -y install llvm-6.0 llvm-6.0-dev && \
  apt-get clean

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain nightly

ADD . /root/arret
WORKDIR /root/arret

RUN $CARGO_PATH build
RUN $CARGO_PATH test
RUN $CARGO_PATH build --release

ENTRYPOINT ["/root/.cargo/bin/cargo", "run"]
