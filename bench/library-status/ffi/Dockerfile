# The library-status rows that need a C library, measured where the C library
# is installed. Builds on the shared base image (bench/docker/base), which has
# dotcl, SBCL and one Quicklisp dist version; this adds the C libraries and
# their development headers, nothing else.
#
#   docker build -t dotcl-bench-base:0.1.32 bench/docker/base
#   docker build -t dotcl-bench-ffi:0.1.32 \
#     --build-arg BASE=dotcl-bench-base:0.1.32 bench/library-status/ffi
#
# run.sh in this directory does the measuring; it is mounted, not copied in:
#
#   docker run --rm -v "$PWD/bench/library-status/ffi:/work" \
#     dotcl-bench-ffi:0.1.32 sh /work/run.sh
#
# Which package serves which system (from each system's
# cffi:define-foreign-library and grovel files):
#   sdl2, sdl2-image, sdl2-ttf   libsdl2-dev, libsdl2-image-dev, libsdl2-ttf-dev
#   lispbuilder-sdl              libsdl1.2-dev (SDL 1.2, sdl12-compat)
#   cl-yaml (cl-libyaml)         libyaml-dev
#   cl-devil                     libdevil-dev (libIL, libILU, libILUT)
#   cl-out123                    libmpg123-dev (libout123.so)
#   pzmq, common-lisp-jupyter    libzmq3-dev (zmq.h for the grovel file)
#   glop                         libx11-dev, libgl-dev, libxrandr-dev,
#                                libxcomposite-dev
#   glfw-blob, glad-blob,        prebuilt blobs; they need libGL and the X
#   bodge-glfw                   libraries at load time. libglfw3-dev as well
#   cffi-libffi (struct by value) libffi-dev

ARG BASE=dotcl-bench-base:0.1.32
FROM ${BASE}

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      libffi-dev \
      libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev \
      libsdl1.2-dev \
      libyaml-dev \
      libdevil-dev \
      libmpg123-dev \
      libzmq3-dev \
      libx11-dev libgl-dev libxrandr-dev libxcomposite-dev \
      libglfw3-dev \
 && rm -rf /var/lib/apt/lists/*
