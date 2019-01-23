FROM debian:buster AS full-compiler

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

FROM debian:buster-slim AS repl
COPY --from=full-compiler /root/arret/.arret-root /root/arret/.arret-root
COPY --from=full-compiler /root/arret/stdlib/arret /root/arret/stdlib/arret
COPY --from=full-compiler /root/arret/target/release/arret /root/arret/target/release/arret
COPY --from=full-compiler /root/arret/target/release/*.so /root/arret/target/release/

WORKDIR /root/arret
CMD ["/root/arret/target/release/arret", "repl"]
