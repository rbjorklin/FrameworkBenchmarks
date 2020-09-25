FROM fedora:32

#ARG BENCHMARK_ENV=local

ENV VERSION 4.11.1
#ENV BENCHMARK_ENV $BENCHMARK_ENV

WORKDIR /webmachine

RUN dnf install --assumeyes opam diffutils postgresql-devel
RUN opam init --disable-sandboxing --auto-setup --compiler ${VERSION}
RUN opam install --yes dune webmachine caqti caqti-lwt caqti-driver-postgresql cohttp-lwt-unix ptime ezjsonm lwt_ppx

COPY ./src /webmachine

RUN eval $(opam env) ; dune build --profile release tfb.exe

CMD /webmachine/_build/default/tfb.exe
