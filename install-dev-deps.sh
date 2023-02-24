#!/usr/bin/env bash

opam update

opam upgrade

opam install zarith mtime containers containers-data msat oseq menhir ocaml-lsp-server ocamlformat
