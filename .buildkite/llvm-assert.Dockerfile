FROM fedora:29

ARG LLVM_MAJOR=8
ARG LLVM_VERSION=8.0.0
ARG LLVM_ROOT=/opt/llvm-${LLVM_MAJOR}

RUN dnf install -y file cmake ninja-build gcc-c++ valgrind xz && \
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
  -DLLVM_TARGETS_TO_BUILD=X86 \
  -DLLVM_ENABLE_WARNINGS=OFF \
  -DLLVM_USE_LINKER=gold \
  ../llvm-${LLVM_VERSION}.src

RUN ninja install && rm -Rf /usr/src/llvm-build
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain 1.35.0

ENV PATH "/root/.cargo/bin:${PATH}"
RUN rustup component add rustfmt

ENV LLVM_SYS_70_PREFIX "${LLVM_ROOT}"
