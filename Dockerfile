FROM debian:buster

ARG rust_toolchain=stable

RUN \
  apt-get update && \
  apt-get -y install curl clang-7 zlib1g-dev llvm-7 llvm-7-dev && \
  apt-get clean

# Use Clang as it understands LLVM target triples
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang-7 100

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain $rust_toolchain
ENV PATH "/root/.cargo/bin:${PATH}"

ADD . /root/arret
WORKDIR /root/arret

RUN cargo build --release

CMD ["/root/arret/target/release/arret", "repl"]
