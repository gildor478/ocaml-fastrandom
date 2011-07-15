(* OASIS_START *)
(* DO NOT EDIT (digest: f4d4fce69d4e8565d984f9b3e38d5ab3) *)
This is the README file for the ocaml-fastrandom distribution.

(C) 2009 Sylvain Le Gall

Fast random number generator

A random number generator compatible with standard library Random module (pre
OCaml 3.12).

It contains C code to speed up generation and a function to skip a lot of
numbers at once. Benchmarks show a 3 fold increase in speed for generating
randome numbers.

See the files INSTALL.txt for building and installation instructions. See the
file COPYING.txt for copying conditions. 

Home page: http://ocaml-fastrandom.forge.ocamlcore.org


(* OASIS_STOP *)
