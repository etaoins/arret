FROM ubuntu:19.04 AS build-env

RUN \
  apt-get update && \
  apt-get -y install curl gcc zlib1g-dev libstdc++-8-dev llvm-7 llvm-7-dev && \
  apt-get clean

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain 1.35.0
ENV PATH "/root/.cargo/bin:${PATH}"

# These are the minimum required files for `cargo fetch`
# This allows the `cargo fetch` to be cached between other source code changes
ADD Cargo.toml Cargo.lock /opt/arret/
ADD syntax/Cargo.toml /opt/arret/syntax/
ADD runtime/Cargo.toml /opt/arret/runtime/
ADD runtime-syntax/Cargo.toml /opt/arret/runtime-syntax/
ADD rfi-derive/Cargo.toml /opt/arret/rfi-derive/
ADD stdlib/rust/Cargo.toml /opt/arret/stdlib/rust/
ADD compiler/Cargo.toml /opt/arret/compiler/
ADD driver/Cargo.toml /opt/arret/driver/

WORKDIR /opt/arret

RUN cargo fetch
ADD . /opt/arret

###

FROM build-env as full-compiler
RUN cargo build --release

###

FROM ubuntu:19.04 AS repl

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
