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

ADD . /opt/arret
WORKDIR /opt/arret

RUN cargo build --release

###

FROM debian:buster-slim AS repl

ARG vcs_ref

COPY --from=full-compiler /opt/arret/.arret-root /opt/arret/.arret-root
COPY --from=full-compiler /opt/arret/stdlib/arret /opt/arret/stdlib/arret
COPY --from=full-compiler /opt/arret/target/release/arret /opt/arret/target/release/arret
COPY --from=full-compiler /opt/arret/target/release/*.so /opt/arret/target/release/

RUN groupadd arret && useradd -r -g arret arret
USER arret:arret

WORKDIR /opt/arret
ENTRYPOINT ["/opt/arret/target/release/arret"]
CMD ["repl"]

# Label the commit that was used to build this
LABEL \
  org.label-schema.vcs-ref=$vcs_ref \
  org.label-schema.vcs-url="https://github.com/etaoins/arret"
