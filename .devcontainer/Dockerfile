ARG COMPILER_TYPE=llvm

FROM fedora:39 as base-llvm-pre

# ARG COMPILER_VERSION=17.0.1

# RUN dnf install -y 'dnf-command(copr)' \
#  && dnf copr enable -y @fedora-llvm-team/llvm-snapshots \
#  && PACKAGE_SUFFIX=$(dnf info --installed coreutils | sed -n 's/Release *: .*\(\.fc[0-9]\+\)$/\1/p;s/Architecture *: \(.\+\)$/.\1/p' | tr -d '\n') \
#  && dnf install -y \
#       llvm-${COMPILER_VERSION}${PACKAGE_SUFFIX} \
#       clang-${COMPILER_VERSION}${PACKAGE_SUFFIX} \
#       lld-${COMPILER_VERSION}${PACKAGE_SUFFIX} \
#       clang-tools-extra-${COMPILER_VERSION}${PACKAGE_SUFFIX}

RUN dnf install -y \
      llvm \
      clang \
      lld \
      clang-tools-extra \
      compiler-rt \
      libcxx \
      libcxxabi \
      llvm-libunwind \
      american-fuzzy-lop-clang \
      clang-analyzer


FROM base-llvm-pre AS build_iwyu

RUN dnf install -y \
      clang-devel \
      llvm-devel \
      git \
      cmake \
      ninja-build

RUN mkdir /prj \
 && cd /prj \
 && git clone -b master --depth 1 https://github.com/include-what-you-use/include-what-you-use.git \
 && cmake -GNinja \
      -Sinclude-what-you-use \
      -Bbuild \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=/usr \
      -DCMAKE_PREFIX_PATH=/usr/lib64 \
 && cmake --build build \
 && DESTDIR=/install cmake --install build

FROM base-llvm-pre AS base-llvm

COPY --from=build_iwyu /install /


FROM fedora:38 as base-gcc

ARG COMPILER_VERSION=13.0.1-0.12

RUN PACKAGE_SUFFIX=$(dnf info --installed coreutils | sed -n 's/Release *: .*\(\.fc[0-9]\+\)$/\1/p;s/Architecture *: \(.\+\)$/.\1/p' | tr -d '\n') \
 && dnf install -y --allowerasing \
      gcc-${COMPILER_VERSION}${PACKAGE_SUFFIX} \
      gcc-c++-${COMPILER_VERSION}${PACKAGE_SUFFIX}


FROM base-${COMPILER_TYPE} as base

RUN dnf install -y \
      pip \
      git \
      procps-ng \
      cmake \
      ninja-build \
      iwyu \
 && dnf clean all

RUN pip install --no-cache-dir -U compdb cmake-format

