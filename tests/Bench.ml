
(** Benchmark for FastRandom
    @author Sylvain Le Gall
  *)

open Benchmark;;

let count = 
  1025 * 1024
;;

let ignore_int : int -> unit = ignore
;;

let buffer_bits n =
 (Printf.sprintf "FastRandom.bits(%d)" n),
 (fun () ->
    let s =
      FastRandom.State.make ~buffer:n [|123456|]
    in
      for i = 1 to count do
        ignore_int (FastRandom.State.bits s)
      done),
 ()
;;

let res = 
  throughputN 
    3
    (
      ["FastRandom.bits", 
       (fun () ->
          for i = 1 to count do
            ignore_int (FastRandom.bits ())
          done),
       ();

       "Random.bits",
       (fun () ->
          for i = 1 to count do
            ignore_int (Random.bits ())
          done),
       ();
      ]
    @
      (List.map buffer_bits
         [
           16;
           32;
           64;
           128;
           512;
           1024;
           2048;
           4096;
           8192;
           16384;
         ])
    )
;;

print_newline();;
tabulate res;;

