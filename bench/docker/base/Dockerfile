# A base image for measuring dotcl next to SBCL on Linux x86_64.
#
# What is in it, and nothing else:
#   - Ubuntu 24.04
#   - .NET SDK 10
#   - dotcl, installed as the .NET global tool (version: DOTCL_VERSION)
#   - SBCL from roswell's sbcl-bin (version: SBCL_VERSION)
#   - one Quicklisp dist version (QL_DIST) installed for both, so the two
#     implementations load the same releases. dotcl also gets its own overlay
#     dist, as `ql:setup` installs it for every user.
#
# Images for a particular measurement build FROM this one and add what that
# measurement needs (C libraries, for instance). Moving to a new dotcl release
# is a rebuild of this image with a different DOTCL_VERSION:
#
#   docker build -t dotcl-bench-base:0.1.32 \
#     --build-arg DOTCL_VERSION=0.1.32 bench/docker/base
#
# Running:
#   dotcl file.lisp                 dotcl runs a script and exits
#   ros run -- --non-interactive --load file.lisp
#                                   SBCL, with Quicklisp already loaded

FROM ubuntu:24.04

ARG DOTCL_VERSION=0.1.32
ARG SBCL_VERSION=2.6.8
ARG ROSWELL_VERSION=26.02.116
ARG QL_DIST=2026-01-01

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    DOTNET_ROOT=/usr/share/dotnet \
    DOTNET_CLI_TELEMETRY_OPTOUT=1 \
    DOTNET_NOLOGO=1 \
    PATH=/root/.dotnet/tools:/usr/share/dotnet:/root/.roswell/bin:$PATH

# Tools the measurements themselves need: a C compiler (cffi-grovel and
# wrapper files run cc), git and curl (Quicklisp, roswell), coreutils timeout.
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      ca-certificates curl bzip2 git make build-essential pkg-config \
      libcurl4t64 libicu74 zlib1g-dev \
 && rm -rf /var/lib/apt/lists/*

# .NET SDK 10 from the official install script.
RUN curl -fsSL https://dot.net/v1/dotnet-install.sh -o /tmp/dotnet-install.sh \
 && bash /tmp/dotnet-install.sh --channel 10.0 --install-dir /usr/share/dotnet \
 && rm /tmp/dotnet-install.sh \
 && dotnet --version

# roswell, then SBCL through it. `ros setup` also installs Quicklisp.
RUN curl -fsSL -o /tmp/ros.tbz \
      https://github.com/roswell/roswell/releases/download/v${ROSWELL_VERSION}/roswell-${ROSWELL_VERSION}-linux-x86_64.tar.bz2 \
 && tar -C /tmp -xjf /tmp/ros.tbz \
 && make -C /tmp/roswell install \
 && rm -rf /tmp/ros.tbz /tmp/roswell \
 && ros install sbcl-bin/${SBCL_VERSION} \
 && ros use sbcl-bin/${SBCL_VERSION} \
 && ros run -- --version

# dotcl as a global tool.
RUN dotnet tool install -g dotcl --version ${DOTCL_VERSION} \
 && dotcl --version < /dev/null

# The same Quicklisp dist version for both implementations.
COPY pin-dist.lisp /opt/bench/pin-dist.lisp
RUN QL_DIST=${QL_DIST} dotcl /opt/bench/pin-dist.lisp < /dev/null \
 && QL_DIST=${QL_DIST} ros run -- --non-interactive --load /opt/bench/pin-dist.lisp < /dev/null

WORKDIR /work
