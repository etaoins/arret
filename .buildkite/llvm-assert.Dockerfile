ARG LLVM_VERSION=11.1.0
ARG LLVM_ROOT=/opt/llvm-11

##

FROM fedora:33 AS fedora-common

RUN dnf install -y gcc-c++
# `dnf clean all` happens in later stages

##

FROM fedora-common AS llvm-build
ARG LLVM_VERSION
ARG LLVM_ROOT

RUN dnf install -y file cmake ninja-build xz && \
  dnf clean all

WORKDIR /usr/src

RUN curl https://github.com/llvm/llvm-project/releases/download/llvmorg-11.1.0/llvm-11.1.0.src.tar.xz -sSL | \
  tar -Jx --no-same-owner

WORKDIR /usr/src/llvm-build

# We need to be careful to use less than 4GiB on our build agents
RUN cmake \
  -GNinja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX=${LLVM_ROOT} \
  -DLLVM_ENABLE_ASSERTIONS=ON \
  -DLLVM_TARGETS_TO_BUILD=AArch64 \
  -DLLVM_ENABLE_WARNINGS=OFF \
  # Disable a spammy ABI change warning on GCC 7 that `ENABLE_WARNINGS=OFF`
  # doesn't suppress.
  -DCMAKE_CXX_FLAGS=-Wno-psabi \
  -DLLVM_USE_LINKER=gold \
  ../llvm-${LLVM_VERSION}.src

RUN ninja install

##

FROM fedora-common
ARG LLVM_ROOT

COPY --from=llvm-build ${LLVM_ROOT} ${LLVM_ROOT}

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain 1.53.0 --profile=minimal --component rustfmt

ENV PATH "/root/.cargo/bin:${PATH}"
ENV LLVM_SYS_100_PREFIX "${LLVM_ROOT}"
