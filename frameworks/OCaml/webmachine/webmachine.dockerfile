FROM fedora:32

WORKDIR /webmachine

RUN dnf install --assumeyes opam diffutils postgresql-devel
RUN opam init --disable-sandboxing --auto-setup
RUN opam install --yes dune webmachine caqti caqti-lwt caqti-driver-postgresql cohttp-lwt-unix
RUN opam install --yes ptime
RUN opam install --yes ezjsonm
RUN opam install --yes lwt_ppx

COPY ./src /webmachine

RUN eval $(opam env) ; dune build tfb.exe

CMD /webmachine/_build/default/tfb.exe
