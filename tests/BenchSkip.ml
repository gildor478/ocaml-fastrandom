
(** Benchmark random number skipping 
    @author Sylvain Le Gall
  *)

open Benchmark;;

let skip_count =
  128
;;

let count = 
  (1024 * 1024) / skip_count
;;

let ignore_int : int -> unit = ignore
;;

let res = 
  throughputN 
    3
    (
      ["FastRandom.skip", 
       (fun () ->
          for i = 1 to count do
            FastRandom.skip skip_count;
            ignore_int (FastRandom.bits ())
          done),
       ();

       "FastRandom.no_skip",
       (fun () ->
          for i = 1 to count do
            for j = 1 to skip_count do
              ignore_int (FastRandom.bits ())
            done;
            ignore_int (FastRandom.bits ())
          done),
       ();

       "Random.skip",
       (fun () ->
          for i = 1 to count do
            for j = 1 to skip_count do
              ignore_int (Random.bits ())
            done;
            ignore_int (Random.bits ())
          done),
       ();
      ]
    )
;;

print_newline();;
tabulate res;;

