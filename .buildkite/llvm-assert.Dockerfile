ARG LLVM_VERSION=8.0.0
ARG LLVM_ROOT=/opt/llvm-8

##

FROM fedora:30 AS llvm-build
ARG LLVM_VERSION
ARG LLVM_ROOT

RUN dnf install -y file cmake ninja-build gcc-c++ xz && \
  dnf clean all

WORKDIR /usr/src
RUN curl http://releases.llvm.org/${LLVM_VERSION}/llvm-${LLVM_VERSION}.src.tar.xz -sS | \
  tar -Jx --no-same-owner

WORKDIR /usr/src/llvm-build

# We need to be careful to use less than 4GiB on our build agents
RUN cmake \
  -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=${LLVM_ROOT} \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_TARGETS_TO_BUILD=X86 \
  -DLLVM_ENABLE_WARNINGS=OFF \
  -DLLVM_USE_LINKER=gold \
  ../llvm-${LLVM_VERSION}.src

RUN ninja install

##

FROM fedora:30
ARG LLVM_ROOT

COPY --from=llvm-build ${LLVM_ROOT} ${LLVM_ROOT}

RUN dnf install -y gcc-c++ valgrind && \
  dnf clean all

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain 1.35.0

ENV PATH "/root/.cargo/bin:${PATH}"
RUN rustup component add rustfmt

ENV LLVM_SYS_70_PREFIX "${LLVM_ROOT}"
