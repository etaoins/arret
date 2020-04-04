ARG LLVM_VERSION=10.0.0
ARG LLVM_ROOT=/opt/llvm-10

##

FROM fedora:31 AS fedora-common

RUN dnf install -y gcc-c++
# `dnf clean all` happens in later stages

##

FROM fedora-common AS llvm-build
ARG LLVM_VERSION
ARG LLVM_ROOT

RUN dnf install -y file cmake ninja-build xz && \
  dnf clean all

WORKDIR /usr/src

RUN curl https://github.com/llvm/llvm-project/releases/download/llvmorg-10.0.0/llvm-10.0.0.src.tar.xz -sSL | \
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

FROM fedora-common
ARG LLVM_ROOT

COPY --from=llvm-build ${LLVM_ROOT} ${LLVM_ROOT}

RUN dnf install -y valgrind && \
  dnf clean all

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain 1.42.0 --profile=minimal

ENV PATH "/root/.cargo/bin:${PATH}"
RUN rustup component add rustfmt

ENV LLVM_SYS_90_PREFIX "${LLVM_ROOT}"
